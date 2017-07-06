library(shinydashboard)
library(shiny)
library(jsonlite)

tweet <- function(id){
  url <- paste0( "https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F", id )
  HTML( jsonlite::fromJSON(url)$html )
}

ui <- dashboardPage(
  dashboardHeader(title = "#useR2017 tweetstorm"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(

    tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),



    # Boxes need to be put in a row (or column)
    fluidRow(

      valueBoxOutput("n_tweets"),
      valueBoxOutput("n_screen_name")

    ),

    fluidRow(
      box(
        title = "Most popular tweet (favorite count)",
        uiOutput("most_popular_tweet")
      ),

      box(
        title = "Most retweeted",
        uiOutput("most_retweeted")
      )

    )
  )
)

server <- function(input, output) {

  tweets <- search_tweets( "#useR2017 #user2017", n = 18000,  include_rts = FALSE)

  n_tweets <- reactive({
    nrow(tweets)
  })

  n_screen_name <- reactive({
    length( unique(tweets$screen_name) )
  })

  most_popular_tweet <- reactive({
    tweets %>% top_n( n = 1, favorite_count ) %>% pull(status_id)
  })

  most_retweeted <- reactive({
    tweets %>% top_n( n = 1, retweet_count ) %>% pull(status_id)
  })


  output$n_tweets <- renderValueBox({
    valueBox( "Tweets", n_tweets(), icon = icon("twitter"), color = "purple" )
  })

  output$n_screen_name <- renderValueBox({
    valueBox( "Users", n_screen_name(), icon = icon("user"), color = "orange" )
  })

  output$most_popular_tweet <- renderUI({
    tweet( most_popular_tweet() )
  })

  output$most_retweeted <- renderUI({
    tweet( most_retweeted() )
  })

}

shinyApp(ui, server)

