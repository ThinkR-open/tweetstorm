
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
