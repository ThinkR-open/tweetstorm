library(shinydashboard)
library(shiny)
library(jsonlite)
library(purrr)
library(rtweet)
library(tidyverse)
library(stringr)
library(DT)

dataTableOutput <- DT::dataTableOutput
renderDataTable <- DT::renderDataTable
datatable <- function(...) DT::datatable( ..., rownames = FALSE )

tweet <- function(id){
  url <- paste0( "https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F", id )
  HTML( fromJSON(url)$html )
}

ui <- dashboardPage(
  dashboardHeader(title = "#useR2017 tweetstorm"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(

    # Boxes need to be put in a row (or column)
    fluidRow(

      valueBoxOutput("n_tweets", width = 2),
      valueBoxOutput("n_screen_name", width = 2),
      valueBoxOutput("n_hashtags", width = 2), 
      valueBoxOutput("n_emojis", width = 2), 
      valueBoxOutput("n_medias", width = 2)

    ),

    fluidRow(
      tabBox( title = "Popular", id = "popular_tabbox", width = 4,
        tabPanel( icon("calendar"), dataTableOutput("recent_tweets") ), 
        tabPanel( icon("heart"), dataTableOutput("most_popular_tweets") ),
        tabPanel( icon("retweet"), dataTableOutput("most_retweeted") )
      ),

      tabBox( title = "Users", id = "users_tabbox", width = 4,
        tabPanel( icon("user"), dataTableOutput("users") ),
        tabPanel( icon( "quote-right"), dataTableOutput("cited_users") ),
        tabPanel( icon("reply"), dataTableOutput("replied_users") )
      ),

      tabBox( title = "Content", id = "content_tabbox", width = 4,
        tabPanel( "Hashtags", dataTableOutput("hashtags") ),
        tabPanel( "Medias", dataTableOutput("medias") ), 
        tabPanel( "Emojis", dataTableOutput("emojis") )
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


server <- function(input, output, session) {

  getTweets <- function( id){
    n <- length(id)
    withProgress(min = 0, max = n, value = 0, message = "extract tweets", {
      
      tibble( 
          tweet = map( id, ~{ 
            res <- tweet(.) 
            incProgress(amount = 1)
            res
          } )
        ) %>% 
        datatable( options = list( pageLength = 2) )
    })
  }
  
  query <- "#useR2017 #user2017"
  minute <- 60 * 1000
  
  tweets <- reactivePoll(minute, session, 
    checkFunc = function(){
      # can be smart here about not calling search_tweets all the time
      # and maybe accross sessions ... 
      withProgress(min = 0, max = 1, value = .4, message = "polling", {
        
        search_tweets(query, include_rts = FALSE) %>% 
            arrange( desc(created_at) ) %>% 
            head(1) %>% 
            pull(status_id)    
        
      })
      
    }, 
    valueFunc = function(){
      n <- 18000
      withProgress(min=0, max=1, value = .2, message = "updating tweets", {
        search_tweets( query, n = n, include_rts = FALSE)  
      })
    }
  )
  
  emojis <- reactive({
    tweets()$text %>%
      str_extract_all("[\\uD83C-\\uDBFF\\uDC00-\\uDFFF]+") %>% unlist() %>%
      str_split("") %>% unlist() %>% 
      table() %>% sort(decreasing = TRUE) %>% as_tibble() %>% 
      set_names( c("Emoji", "Frequency") ) %>% 
      filter( ! str_detect(Emoji, "^[-[:space:]]" ) )
  })
  
  
  n_tweets <- reactive({
    nrow(tweets())
  })

  n_screen_name <- reactive({
    length( unique(tweets()$screen_name) )
  })

  most_popular_tweets <- reactive({
    tweets() %>% top_n( n = 6, favorite_count ) %>% arrange( desc(favorite_count) ) %>% pull(status_id)
  })

  most_retweeted <- reactive({
    tweets() %>% top_n( n = 6, retweet_count ) %>% arrange( desc(retweet_count) ) %>% pull(status_id)
  })
  
  recent_tweets <- reactive({
    tweets() %>% arrange(desc(created_at)) %>% head(6) %>% pull(status_id)
  })

  users <- reactive( user_data( tweets()$user_id ) )
  cited <- reactive( user_data( tweets()$mentions_user_id ) )
  replied_users <- reactive( user_data( tweets()$in_reply_to_status_user_id ) )

  hashtags <- reactive({
    tibble(
      hashtag   = str_split( tweets()$hashtags, " " ) %>% flatten_chr()
    ) %>%
      filter( !is.na(hashtag) ) %>%
      group_by(hashtag) %>%
      summarise( n = n() ) %>%
      arrange( desc(n) )
  })

  medias <- reactive({
    tweets() %>%
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
  
  
  output$n_emojis <- renderValueBox({
    emos <- nrow(emojis())
    valueBox( "Emojis", emos, icon = icon("heart"), color = "olive" )
  })
  
  output$n_medias <- renderValueBox({
    emos <- nrow(medias())
    
    valueBox( "Media", emos, icon = icon("image"), color = "red" )
  })
  
  
  output$most_popular_tweets <- renderDataTable( getTweets( most_popular_tweets() ) )
  output$most_retweeted <- renderDataTable( getTweets( most_retweeted() ) )
  output$recent_tweets <- renderDataTable( getTweets( recent_tweets()  ) )
  
  output$users <- renderDataTable({
    datatable( select( users(), name, n, followers_count ), options = list( pageLength = 20) )
  })

  output$cited_users <- renderDataTable({
    datatable( select( cited(), name, n, followers_count ), options = list( pageLength = 20) )
  })

  output$replied_users <- renderDataTable({
    datatable( select( replied_users(), name, n, followers_count ), options = list( pageLength = 20) )
  })

  output$hashtags <- renderDataTable({
    datatable( hashtags(), options = list( pageLength = 20) )
  })

  output$medias <- renderDataTable({
    datatable( medias(), escape = FALSE, options = list( pageLength = 2) )
  })

  output$emojis <- renderDataTable({
    datatable( emojis(), escape = FALSE, options = list( pageLength = 20) )
  })
  
}

shinyApp(ui, server)

