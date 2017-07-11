
#' Embed tweet
#'
#' @param id status id of the tweet to embed
#'
#' @return html embeddable tweet, e.g. inside a [shiny::renderUI()] 
#' @export
#'
#' @examples
#' embed_tweet( "266031293945503744" )
#' 
#' @importFrom htmltools HTML
#' @importFrom jsonlite fromJSON
#' @export
embed_tweet <- function(id){
  url <- paste0( "https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F", id )
  tweet <- HTML( fromJSON(url)$html )
  
  class( tweet ) <- c( "tweet", class(tweet) )
  tweet
}

#' @importFrom htmltools html_print
#' @export
print.tweet <- function(x, ... ){
  html_print(x)
  invisible(x)
}

#' random tweet
#' 
#' @param query twitter query, see [rtweet::search_tweets]
#'
#' @examples
#' \dontrun{
#'   embed_tweet( random_tweet("#rstats") )
#' }
#' @importFrom rtweet search_tweets
#' @importFrom dplyr sample_n pull
#' @export
random_tweet <- function(query = "#rstats" ){
  search_tweets( query ) %>% 
    sample_n(1) %>% 
    pull(status_id)
}

emoji_regex <- "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF\u2600-\u27ff]+"
not_equal <- function( x, text = "-" ) x[ x != text ]

#' Extract emojis
#'
#' @param text text with emojis
#'
#' @return a tibble with the columns `Emoji` and `n`
#' 
#' @references 
#' Inspired from [](http://seankross.com/2017/05/30/Which-Emojis-Does-Lucy-Use-in-Commit-Messages.html)
#' 
#' @export
#' @importFrom stringr str_extract_all str_split
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>% set_names
extract_emojis <- function(text){
  str_extract_all(text, emoji_regex ) %>% 
    flatten_chr() %>% 
    str_split("") %>% 
    flatten_chr() %>% 
    not_equal("-") %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    as_tibble() %>% 
    set_names( c("Emoji", "n") )
}

#' Extract information about users that use emojis
#' 
#' @param tweets tweets data set
#' @export
extract_emojis_users <- function(tweets){
  
  data <- tweets %>% 
    select( user_id, text ) %>% 
    mutate( 
      emojis = str_extract_all(text, emoji_regex ) %>% map( not_equal, "-" )
    ) %>% 
    filter( map_int(emojis, length) > 0 ) %>% 
    group_by( user_id ) %>% 
    summarise( 
      emojis = map(emojis, ~ flatten_chr(str_split(., "") ) ) %>% flatten_chr() %>% table() %>% list()
    ) %>% 
    mutate( 
      total = map_int(emojis, sum), 
      distinct = map_int(emojis, length), 
      emojis = map_chr( emojis, ~ paste( names(.)[ order(., decreasing = TRUE)], collapse = "") )
    ) %>% 
    arrange( desc(total) )

  left_join( data, lookup_users(data$user_id), by = "user_id" ) %>% 
    mutate( img = sprintf('<img src="%s" />', profile_image_url ) ) %>% 
    select( img, name, total, distinct, emojis )
  
}



#' @importFrom rlang enquo
most <- function( tweets, n = 6, var ){
  var <- enquo(var)
  tweets %>% 
    top_n( n = 6, !!var ) %>% 
    arrange( desc(favorite_count) ) %>% 
    pull(status_id)
  
}

#' most popular tweets
#'
#' @param tweets tweets data
#' @param n number of tweets to extract
#'
#' @export
#'
#' @importFrom dplyr top_n arrange desc pull
most_popular <- function( tweets, n = 6 ){
  most(tweets, n, favorite_count)
}

#' @rdname most_popular
#' @export
most_retweeted <- function(tweets, n = 6 ){
  most(tweets, n, retweet_count)
}

#' @rdname most_popular
#' @export
#' @importFrom utils head
most_recent <- function( tweets, n = 6 ){
  tweets %>% 
    arrange(desc(created_at)) %>% 
    head(6) %>% 
    pull(status_id)
}

#' Summarise hashtags
#'
#' @param hashtags vector of hashtags
#'
#' @return a tibble
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom purrr flatten_chr
#' @importFrom stringr str_split
#' @importFrom dplyr filter group_by summarise n arrange desc
summarise_hashtags <- function( hashtags ){
  tibble(
    hashtag   = flatten_chr( str_split( hashtags, " " ) )
  ) %>%
    filter( !is.na(hashtag) ) %>%
    group_by(hashtag) %>%
    summarise( n = n() ) %>%
    arrange( desc(n) )
}


#' Extract medias from tweets
#'
#' @param tweets tweets tibble
#'
#' @export
#' @importFrom glue glue
#' @importFrom dplyr select mutate
extract_medias <- function( tweets ){
  tweets %>%
    filter( !is.na(media_url) ) %>%
    select( status_id, user_id, media_url, favorite_count ) %>%
    arrange( desc( favorite_count ) ) %>%
    mutate( media = glue( '<img src="{media_url}" width="100%"/> ' ) ) %>%
    select( favorite_count, media )
}

#' Extract users information
#'
#' @param x user ids
#'
#' @importFrom tibble tibble
#' @importFrom purrr flatten_chr
#' @importFrom stringr str_split
#' @importFrom dplyr filter group_by summarise arrange desc bind_cols select
#' @importFrom rtweet lookup_users
#' 
#' @export
extract_users <- function( x ){
  users <- tibble(
    id   = flatten_chr(str_split( x, " " ) )
  ) %>%
    filter( !is.na(id) ) %>%
    group_by(id) %>%
    summarise( n = n() ) %>%
    arrange( desc(n) )
  
  left_join( 
    users, 
    lookup_users(users$id), 
    by = c("id" = "user_id")
  )
}

#' organise users in a data table
#' 
#' @param data tibble with user information
#'
#' @importFrom DT datatable
#' @export
users_datatable <- function( data ){
  data %>% 
    mutate( img = sprintf('<img src="%s" />', profile_image_url ) ) %>% 
    select( img, name, n, followers_count ) %>% 
    datatable( options = list( pageLength = 10), escape = FALSE )
}

#' pack data 
#' 
#' @param data data 
#' @param var variable to pack
#' @param ... see [base::cut]
#'
#' @export
#' @importFrom rlang enquo quo_text := 
#' @importFrom dplyr filter group_by summarise arrange desc bind_cols select
pack <- function(data, var, ... ){
  var <- enquo(var)
  
  data %>%
    mutate( group = cut( n, seq(0, max(n)+10, ...  )) ) %>% 
    group_by( group ) %>% 
    summarise( !!quo_text(var) := paste(!!var, collapse = ", ") ) %>% 
    arrange( desc(group) )
}

update_search <- function( data, ... ){
  res <- search_tweets( ... )
  bind_rows( filter( data, ! status_id %in% res$status_id  ), res )
}


