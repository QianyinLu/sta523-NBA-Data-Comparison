suppressMessages(library(tidyverse))
suppressMessages(library(shiny))

James <- readRDS("data/james.rds")
Durant <- readRDS("data/durant.rds")
Image <- readRDS("data/image.rds")

ui <- fixedPage(
  navbarPage(title = "James VS Durant",
             tabPanel(
               title = "Basic",
               fixedRow(
                 column(width = 4,
                        h2("Player Stats"),
                        uiOutput(outputId = "player_photo_basic"),
                        selectInput(inputId = "player_name_basic", label = "player",
                                    choices = c("Lebron James", "Kevin Durant")),
                        selectInput(inputId = "player_stats_1", label = "Stats 1",
                                    choices = c("Minute", "Points", "Rebound", "Assist",
                                                "Block", "Steal", "Field Goal Percentage",
                                                "3-Point Percentage", "Free Shoot Percentage")),
                        selectInput(inputId = "player_stats_2", label = "Stats 2",
                                    choices = c("Minute", "Points", "Rebound", "Assist",
                                                "Block", "Steal", "Field Goal Percentage",
                                                "3-Point Percentage", "Free Shoot Percentage")),
                        radioButtons(inputId = "style_basic", label = "Style",
                                     choices = c("light", "dark"))
                        ),
                 column(width = 8,
                        h2(textOutput("basic_player")),
                        h4(textOutput("basic_info")),
                        plotOutput("basic_plot"))
               ),
               fixedRow(
                 column(width = 4,
                        h2("Stats Comparison"),
                        selectInput(inputId = "basic_season_J", label = "Season for James",
                                    choices = unique(James$avg$season)),
                        selectInput(inputId = "basic_season_D", label = "Season for Durant",
                                    choices = unique(Durant$avg$season))
                        ),
                 column(width = 8,
                        h3(textOutput("summary_stats_header")),
                        tableOutput("summary_stats"))
                 )
               ),
             tabPanel(title = "Advanced",
                      fixedRow(
                        column(width = 4,
                               h2("Player Advanced Data"),
                               selectInput(inputId = "advance_player_name", label = "Player",
                                           choices = c("Lebron James", "Kevin Durant")),
                               selectInput(inputId = "advance_data_type", label = "Type",
                                           choices = c("Game Location", "Game Result",
                                                       "Shot Points", "Shot Distance",
                                                       "Shot Type", "Quarter",
                                                       "Time Left in Quarter",
                                                       "Margin","Opponent","Month")),
                               radioButtons(inputId = "advance_style", label = "Style",
                                            choices = c("light", "dark"))
                        ),
                        
                        column(width = 8,
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
                        column(width = 4,
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
                        
                        column(width = 8,
                               h2(textOutput("advance_title")),
                               fluidRow(
                                 column(width = 6,
                                        plotOutput("advance_p1")
                                 ),
                                 column(width = 6,
                                        plotOutput("advance_p2")
                                 )
                               )
                        )
                      )
             )
  )
)

server <- function(input, output){
  # Basic Page
  
  ## player photo
  
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
           paste(input$player_stats_1, "VS", input$player_stats_2))
  })
  output$summary_stats_header <- renderText({
    paste("James in Season", input$basic_season_J,
          "VS", "Durant in Season", input$basic_season_D)
  })
  
  ## function to plot
  switch_stats <- function(x){
    switch(x, "Minute" = "MIN", "Points" = "PTS", "Rebound" = "REB", "Assist" = "AST",
           "Block" = "BLK", "Steal" = "STL", "Field Goal Percentage" = "FG%", 
           "3-Point Percentage" = "3P%", "Free Shoot Percentage" = "FT%")
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
      g <- ggplot(data = tmp, aes(x = x, y = y, group = colour, colour = colour)) +
        geom_point(size = 5) +
        geom_line(size = 1.5) +
        labs(x = "Season", y = stats1)
      if(style == "light"){
        g <- g +
          theme_light() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        }else{
        g <- g +
          theme_dark() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        }
    }else{
      tmp <- data.frame(x = data[[switch_stats(stats1)]],
                        y = data[[switch_stats(stats2)]],
                        colour = data[["regular"]])
      g <- ggplot(data = tmp, aes(x = x, y = y, colour = colour)) +
        geom_point(size = 5) +
        labs(x = stats1, y = stats2)
      if(style == "light"){
        g <- g +
          theme_light() +
          theme(axis.text = element_text(size = 15),
                axis.title = element_text(size = 20),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
      }else{
        g <- g +
          theme_dark() +
          theme(axis.text = element_text(size = 15),
                axis.title = element_text(size = 20),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
      }
    }
    
    return(g)
  }
  
  ## stats plot
  output$basic_plot <- renderPlot({
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
}

shinyApp(ui, server)