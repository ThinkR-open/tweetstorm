
#' Embed tweet
#'
#' @param id status id of the tweet to embed
#'
#' @return html embeddable tweet, e.g. inside a \code{\link[shiny]{renderUI}}
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
  HTML( fromJSON(url)$html )
}

#' Extract emojis
#'
#' @param text text with emojis
#'
#' @return a tibble with the columns \code{Emoji} and \code{Frequency}
#' 
#' @references 
#' Inspired from \url{http://seankross.com/2017/05/30/Which-Emojis-Does-Lucy-Use-in-Commit-Messages.html}
#' 
#' @export
#' @importFrom stringr str_extract_all str_split
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
extract_emojis <- function(text){
  str_extract_all(text, "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF]+") %>% 
    unlist() %>%
    str_split("") %>% 
    unlist() %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    as_tibble() %>% 
    set_names( c("Emoji", "Frequency") )
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

