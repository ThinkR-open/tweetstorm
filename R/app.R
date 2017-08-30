#' Run shiny app
#'
#' @param display.mode display mode
#' @param \dots see \code{\link[shiny]{runApp}}
#' @export
#' @examples
#' \dontrun{
#'   app()
#' }
app <- function(display.mode = "normal", ...) {
  appDir <- system.file("shiny", package = "tweetstorm")
  if (appDir == "") {
    stop("Could not find. Try re-installing `tweetstorm`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = display.mode, ... )
}
