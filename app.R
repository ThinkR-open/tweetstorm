library(shinydashboard)
library(shiny)
library(jsonlite)
library(purrr)
library(rtweet)
library(tidyverse)
library(stringr)

tweet <- function(id){
  url <- paste0( "https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F", id )
  HTML( fromJSON(url)$html )
}

ui <- dashboardPage(
  dashboardHeader(title = "#useR2017 tweetstorm"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(

    # tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),

    # Boxes need to be put in a row (or column)
    fluidRow(

      valueBoxOutput("n_tweets", width = 2),
      valueBoxOutput("n_screen_name", width = 2),
      valueBoxOutput("n_hashtags", width = 2)

    ),

    fluidRow(
      tabBox( title = "Popular", id = "popular_tabbox", width = 4,
        tabPanel( "Most Popular tweets",
          uiOutput("most_popular_tweets")
        ),
        tabPanel( "Most Retweeted",
          uiOutput("most_retweeted")
        )
      ),

      tabBox( title = "Users", id = "users_tabbox", width = 4,
        tabPanel( "User", DT::dataTableOutput("users") ),
        tabPanel( "Cited", DT::dataTableOutput("cited_users") ),
        tabPanel( "Replied to", DT::dataTableOutput("replied_users") )
      ),

      tabBox( title = "Content", id = "content_tabbox", width = 4,
        tabPanel( "Hashtags", DT::dataTableOutput("hashtags") ),
        tabPanel( "Medias", DT::dataTableOutput("medias") )
      )
    )
  )
)

user_data <- function( x ){
  users <- tibble(
    id   = str_split( x, " " ) %>% flatten_chr()
  ) %>%
    filter( !is.na(id) ) %>%
    group_by(id) %>%
    summarise( n = n() ) %>%
    arrange( desc(n) )

  lookups <- lookup_users(users$id)

  res <- bind_cols( select(users, -id), lookups )
  res
}

server <- function(input, output) {

   tweets <- search_tweets( "#useR2017 #user2017", n = 18000,  include_rts = FALSE)

  n_tweets <- reactive({
    nrow(tweets)
  })

  n_screen_name <- reactive({
    length( unique(tweets$screen_name) )
  })

  most_popular_tweets <- reactive({
    tweets %>% top_n( n = 5, favorite_count ) %>% arrange( desc(favorite_count) ) %>% pull(status_id)
  })

  most_retweeted <- reactive({
    tweets %>% top_n( n = 5, retweet_count ) %>% arrange( desc(retweet_count) ) %>% pull(status_id)
  })

  users <- reactive( user_data( tweets$user_id ) )
  cited <- reactive( user_data( tweets$mentions_user_id ) )
  replied_users <- reactive( user_data( tweets$in_reply_to_status_user_id ) )

  hashtags <- reactive({
    tibble(
      hashtag   = str_split( tweets$hashtags, " " ) %>% flatten_chr()
    ) %>%
      filter( !is.na(hashtag) ) %>%
      group_by(hashtag) %>%
      summarise( n = n() ) %>%
      arrange( desc(n) )
  })

  medias <- reactive({
    tweets %>%
      filter( !is.na(media_url) ) %>%
      select( status_id, user_id, media_url, favorite_count ) %>%
      arrange( desc( favorite_count ) ) %>%
      mutate( media = sprintf( '<img src="%s" width="100%%"/> ', media_url ) ) %>%
      select( favorite_count, media )
  })


  output$n_tweets <- renderValueBox({
    valueBox( "Tweets", n_tweets(), icon = icon("twitter"), color = "purple" )
  })

  output$n_screen_name <- renderValueBox({
    valueBox( "Users", n_screen_name(), icon = icon("user"), color = "orange" )
  })

  output$n_hashtags <- renderValueBox({
    valueBox( "Hashtags", nrow(hashtags()), icon = icon("hashtag"), color = "blue" )
  })

  output$most_popular_tweets <- renderUI({
    div( map( most_popular_tweets(), tweet ) )
  })

  output$most_retweeted <- renderUI({
    div( map( most_retweeted(), tweet ) )
  })

  output$users <- DT::renderDataTable({
    DT::datatable( select( users(), name, n, followers_count ) )
  })

  output$cited_users <- DT::renderDataTable({
    DT::datatable( select( cited(), name, n, followers_count ) )
  })

  output$replied_users <- DT::renderDataTable({
    DT::datatable( select( replied_users(), name, n, followers_count ) )
  })

  output$hashtags <- DT::renderDataTable({
    DT::datatable( hashtags() )
  })

  output$medias <- DT::renderDataTable({
    DT::datatable( medias(), escape = FALSE, options = list( pageLength = 2) )
  })

}

shinyApp(ui, server)

