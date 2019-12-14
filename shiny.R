suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(ggiraph))

James <- readRDS("data/james.rds")
Durant <- readRDS("data/durant.rds")
Image <- readRDS("data/image.rds")

ui <- fixedPage(
  navbarPage(title = "James VS Durant",
             tabPanel(
               title = "Basic",
               fixedRow(
                 column(width = 3,
                        h2("Player Stats"),
                        div(align = "center",
                            uiOutput(outputId = "player_photo_basic")),
                        radioButtons(inputId = "player_name_basic", label = "player",
                                     choices = c("Lebron James", "Kevin Durant")),
                        selectInput(inputId = "player_stats_1", label = "Stats 1",
                                    choices = c("Minutes Per Game", "Points", "Rebounds", "Assists",
                                                "Blocks", "Steals", "Field Goal Percentage",
                                                "3-Point Field Goal Percentage", "Free Throw Percentage")),
                        selectInput(inputId = "player_stats_2", label = "Stats 2",
                                    choices = c("Minutes Per Game", "Points", "Rebounds", "Assists",
                                                "Blocks", "Steals", "Field Goal Percentage",
                                                "3-Point Field Goal Percentage", "Free Throw Percentage"))
                 ),
                 column(width = 9,
                        h2(textOutput("basic_player")),
                        h4(textOutput("basic_info")),
                        ggiraphOutput("basic_plot"),
                        div(align = "right",
                            "Source: "))
               ),
               fixedRow(
                 column(width = 3,
                        h2("Stats Comparison"),
                        selectInput(inputId = "basic_season_J", label = "Season for James",
                                    choices = unique(James$avg$season)),
                        selectInput(inputId = "basic_season_D", label = "Season for Durant",
                                    choices = unique(Durant$avg$season))
                 ),
                 column(width = 9,
                        h3(textOutput("summary_stats_header")),
                        tableOutput("summary_stats"),
                        div(align = "right",
                            "Source: "),
                        "Note :", br(),
                        "MIN -- Minutes Per Game", br(),
                        "PTS -- Points", br(),
                        "REB -- Rebounds", br(),
                        "AST -- Assists", br(),
                        "STL -- Steals", br(),
                        "BLK -- Blocks", br(),
                        "FG% -- Field Goal Percentage", br(),
                        "3P% -- 3-Point Field Goal Percentage", br(),
                        "FT% -- Free Throw Percentage"
                 )
               )
             ),
             tabPanel(title = "Advanced",
                      fixedRow(
                        column(width = 3,
                               h2("Player Advanced Data"),
                               radioButtons(inputId = "advance_player_name", label = "Player",
                                            choices = c("Lebron James", "Kevin Durant")),
                               radioButtons(inputId = "advance_data_type", label = "Type",
                                            choices = c("Game Location", "Shot Distance",
                                                        "Shot Type", "Time Left in Quarter"))
                        ),
                        
                        column(width = 9,
                               h2(textOutput("advance_player_name")),
                               plotOutput("advance_plot"),
                               div(align = "right",
                                   h4(textOutput("advance_plot_source"))),
                               h3(textOutput("advance_summary_stats_header")),
                               tableOutput("advance_summary_stats"),
                               div(align = "right",
                                   h4(textOutput("advance_table_source")))
                        )
                      ),
                      fluidRow(
                        column(width = 3,
                               selectInput(inputId = "advance_data_type_2", label = "Type",
                                           choices = c("Game Location", "Game Result",
                                                       "Shot Points", "Shot Distance",
                                                       "Shot Type", "Quarter",
                                                       "Time Left in Quarter",
                                                       "Margin","Opponent","Month")),
                               selectInput(inputId = "advance_james_season", label = "Season for James",
                                           choices =  unique(James$avg$season)),
                               selectInput(inputId = "advance_durant_season", label = "Season for Durant",
                                           choices = unique(Durant$avg$season))
                        ),
                        
                        column(width = 9,
                               h2(textOutput("advance_title")),
                               fluidRow(
                                 column(width = 4.5,
                                        plotOutput("advance_p1")
                                 ),
                                 column(width = 4.5,
                                        plotOutput("advance_p2")
                                 )
                               )
                        )
                      )
             ),
             tabPanel(title = "Record",
                      h1("Durant VS James"),
                      h2("Rugular Season"),
                      fluidRow(
                        column(width = 6,
                               ggiraphOutput("record_reg_plot")),
                        column(width = 6,
                               fluidRow(
                                 column(width = 2),
                                 column(width = 2, uiOutput("J_MIA")),
                                 column(width = 2, uiOutput("D_OKC"))
                               ),
                               tableOutput("record_reg_table"))
                      )
             )
  )
)

server <- function(input, output){
  # Basic Page
  
  ## function to create image
  createImage <- function(url, width){
    sprintf('<img src="%s" width="%s"></img>', url, as.character(width))
  }
  
  ## player photo
  output$player_photo_basic <- renderText({
    ifelse(input$player_name_basic == "Lebron James",
           createImage(Image$profile.photo[2], 150),
           createImage(Image$profile.photo[4], 200)
    )
    
  })
  
  ## team for player
  team_full <- function(team){
    case_when(
      team == "OKC" ~ "Oklahoma City Thunder",
      team == "SEA" ~ "Seattle SuperSonics (Now is Oklahoma City Thunder)",
      team == "GS" ~ "Golden State Warriors",
      team == "CLE" ~ "Cleveland Cavaliers",
      team == "MIA" ~ "Miami Heat",
      team == "LAL" ~ "Los Angeles Lakers")
  }
  
  ## Text
  output$basic_player <- renderText({input$player_name_basic})
  output$basic_info <- renderText({
    ifelse(input$player_stats_1 == input$player_stats_2, input$player_stats_1,
           paste(input$player_stats_1, "&", input$player_stats_2))
  })
  output$summary_stats_header <- renderText({
    paste("James in Season", input$basic_season_J,
          "VS", "Durant in Season", input$basic_season_D)
  })
  
  ## function to plot
  switch_stats <- function(x){
    switch(x, "Minutes Per Game" = "MIN", "Points" = "PTS", "Rebounds" = "REB", "Assists" = "AST",
           "Blocks" = "BLK", "Steals" = "STL", "Field Goal Percentage" = "FG%", 
           "3-Point Field Goal Percentage" = "3P%", "Free Throw Percentage" = "FT%")
  }
  
  stats_plot <- function(player, stats1, stats2, style){
    if(player == "Kevin Durant"){
      data <- Durant$avg
    }
    if(player == "Lebron James"){
      data <- James$avg
    }
    if(stats1 == stats2){
      tmp <- data.frame(x = data[["season"]],
                        y = data[[switch_stats(stats1)]],
                        colour = data[["regular"]])
      tmp$detail <- paste("Season:", tmp$x, "\n", stats1, ":", tmp$y)
      g <- ggplot(data = tmp) +
        geom_point_interactive(aes(x = x, y = y, tooltip = detail,
                                   group = colour, colour = colour), size = 5) +
        geom_line(aes(x = x, y = y, group = colour, colour = colour), size = 1.5) +
        labs(x = "Season", y = stats1) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              legend.title = element_text(size =  20),
              legend.text = element_text(size = 15))
    }else{
      tmp <- data.frame(x = data[[switch_stats(stats1)]],
                        y = data[[switch_stats(stats2)]],
                        colour = data[["regular"]])
      tmp$detail <- paste("Season:", data[["season"]], "\n",
                          stats1, ":", tmp$x, "\n", stats2, ":", tmp$y)
      g <- ggplot(data = tmp) +
        geom_point_interactive(aes(x = x, y = y, colour = colour, 
                                   tooltip = detail), size = 5) +
        labs(x = stats1, y = stats2) +
        theme_light() +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 20),
              legend.title = element_text(size =  20),
              legend.text = element_text(size = 15))
    }
    return(girafe(code = print(g)))
  }
  
  ## stats plot
  output$basic_plot <- renderggiraph({
    stats_plot(input$player_name_basic, input$player_stats_1,
               input$player_stats_2, input$style_basic)
  })
  
  ## function to get table
  basic.table <- function(df, target, player){
    df$Player <- player
    data <- df %>%
      filter(season == target) %>%
      select(Player, regular, MIN, PTS, REB, AST, BLK, STL, `FG%`, `3P%`, `FT%`)
    rownames(data) = NULL
    colnames(data)[2] <- "Season Type"
    data
  }
  
  ## stats table
  output$summary_stats <- renderTable({
    rbind(basic.table(James$avg, input$basic_season_J, "James"),
          basic.table(Durant$avg, input$basic_season_D, "Durant"))
  })
  
  # Advanced Page
  
  # Record Page
  
}

shinyApp(ui, server)