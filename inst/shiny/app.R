library(shinydashboard)
library(shiny)
library(jsonlite)
library(purrr)
library(rtweet)
library(tidyverse)
library(stringr)
library(DT)
library(tweetstorm)

dataTableOutput <- DT::dataTableOutput
renderDataTable <- DT::renderDataTable
datatable <- function(...) DT::datatable( ..., rownames = FALSE )

thinkr_branding <- function( repo = "tweetstorm" ){
  absolutePanel( # class = "panel panel-default panel-side",
    style = "z-index: 2000",
    fixed = TRUE, draggable = TRUE,
    top  = 10, left = "auto", right = 20,
    
    width = "250px",
    div(
      tags$a( target="_blank", href = "http://www.thinkr.fr", tags$img(src="thinkR1.png", height = "30px", id = "logo") ),
      tags$a( target="_blank", href = paste0( "https://github.com/ThinkRstat/", repo), tags$img(src="https://cdn0.iconfinder.com/data/icons/octicons/1024/mark-github-256.png", height = "30px") ),
      tags$a( target="_blank", href = "https://twitter.com/thinkR_fr", tags$img(src="https://cdn3.iconfinder.com/data/icons/social-icons-5/128/Twitter.png", height = "30px") ),
      tags$a( target="_blank", href = "https://www.facebook.com/ThinkR-1776997009278055/", tags$img(src="https://cdn4.iconfinder.com/data/icons/social-messaging-ui-color-shapes-2-free/128/social-facebook-circle-128.png", height = "30px") )
    )
    
  )
}

ui <- dashboardPage( skin = "black", 
  dashboardHeader(title = "tweetstorm"),
  dashboardSidebar(
    
    textInput("query", label = "Query", value = "#useR2017"),
    actionButton( "refresh", label = NULL, icon = icon( "refresh" ) ),
    
    sliderInput( "max_tweets", label = "Number of Tweets",
      min = 1000, max = 18000, value = 2000, step = 500 )

  ),
  dashboardBody(

    thinkr_branding(), 
    
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
        tabPanel( icon("quote-right"), dataTableOutput("cited_users") ),
        tabPanel( icon("reply"), dataTableOutput("replied_users") )
      ),

      tabBox( title = "Content", id = "content_tabbox", width = 4,
        tabPanel( "Emojis", dataTableOutput("emojis") ), 
        tabPanel( icon( "hashtag" ), dataTableOutput("hashtags") ),
        tabPanel( icon("image"), dataTableOutput("medias") )
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

  getTweets <- function( id ){
    n <- length(id)
    withProgress(min = 0, max = n, value = 0, message = "extract tweets", {
      
      tibble( 
          tweet = map( id, ~{ 
            res <- embed_tweet(.) 
            incProgress(amount = 1)
            res
          } )
        ) %>% 
        datatable( options = list( pageLength = 3) )
    })
  }
  
  tweets <- reactive({
    input$refresh
    withProgress(min=0, max=1, value = .2, message = "updating tweets", {
      search_tweets(isolate(input$query), n = isolate(input$max_tweets), include_rts = FALSE )
    })
  })
  
  emojis <- reactive({
    extract_emojis(tweets()$text) 
  })

  n_tweets <- reactive({
    nrow(tweets())
  })

  n_screen_name <- reactive({
    length( unique(tweets()$screen_name) )
  })

  most_popular_tweets <- reactive( most_popular( tweets(), n = 6 ) )
  most_retweeted_tweets <- reactive( most_retweeted( tweets(), n = 6) )
  recent_tweets <- reactive( most_recent(tweets(), n = 6) )

  users <- reactive( user_data( tweets()$user_id ) )
  cited <- reactive( user_data( tweets()$mentions_user_id ) )
  replied_users <- reactive( user_data( tweets()$in_reply_to_status_user_id ) )

  hashtags <- reactive( summarise_hashtags( tweets()$hashtags ) )

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
  output$most_retweeted <- renderDataTable( getTweets( most_retweeted_tweets() ) )
  output$recent_tweets <- renderDataTable( getTweets( recent_tweets()  ) )
  
  userDataTable <- function( data ){
    data %>% 
      mutate( img = sprintf('<img src="%s" />', profile_image_url ) ) %>% 
      select( img, name, n, followers_count ) %>% 
      datatable( options = list( pageLength = 10), escape = FALSE )
  }
  
  output$users <- renderDataTable({
    userDataTable( users() )
  })

  output$cited_users <- renderDataTable({
    userDataTable( cited() )
  })

  output$replied_users <- renderDataTable({
    userDataTable( replied_users() )
  })

  output$hashtags <- renderDataTable({
    data <- hashtags() %>%
      mutate( group = cut( n, seq(0, max(n)+10, by = 5   )) ) %>% 
      group_by( group ) %>% 
      summarise( hashtag = paste(hashtag, collapse = ", ") ) %>% 
      arrange( desc(group) )
    
    datatable( data, options = list( pageLength = 20) )
  })

  output$medias <- renderDataTable({
    datatable( medias(), escape = FALSE, options = list( pageLength = 2) )
  })

  output$emojis <- renderDataTable({
    
    data <- emojis() %>% 
      mutate( group = cut( Frequency, seq(0, max(Frequency)+10, by = 2   )) ) %>% 
      group_by( group ) %>% 
      summarise( Emoji = paste(Emoji, collapse = "") ) %>% 
      arrange( desc(group) )
    
    datatable( data , escape = FALSE, options = list( pageLength = 20) )
  })
  
}

shinyApp(ui, server)

