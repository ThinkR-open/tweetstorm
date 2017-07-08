
#' Embed tweet
#'
#' @param id status id of the tweet to embed
#'
#' @return html embeddable tweet, e.g. inside a \code{\link[shiny]{renderUI}
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
