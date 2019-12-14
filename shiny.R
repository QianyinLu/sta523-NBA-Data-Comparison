suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(ggiraph))

James <- readRDS("data/james.rds")
Durant <- readRDS("data/durant.rds")
Image <- readRDS("data/image.rds")
direct <- readRDS("data/direct.rds")

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
                            "Source: https://www.basketball-reference.com/"))
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
                            "Source: https://www.basketball-reference.com/"),
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
                               h2("Advanced Data"),
                               radioButtons(inputId = "advance_player_name", label = "Player",
                                           choices = c("Lebron James", "Kevin Durant")),
                               radioButtons(inputId = "advance_data_type", label = "Type",
                                           choices = c("Game Location", "Shot Distance",
                                                       "Shot Type", "Time Left in Quarter"))
                        ),
                        
                        column(width = 9,
                               h2(textOutput("advance_player_name")),
                               h3(textOutput("advance_player_info")),
                               fluidRow(
                                 column(width = 6,
                                        ggiraphOutput("advance_plot")),
                                 column(width = 3,
                                        uiOutput("advance_image"))
                               ),
                               div(align = "right",
                                   "Source: https://www.basketball-reference.com/")
                        )
                      ),
                      fluidRow(
                        column(width = 3,
                               h2("Comparison"),
                               radioButtons(inputId = "advance_data_type_2", label = "Type",
                                           choices = c("Game Location", "Shot Distance",
                                                       "Shot Type", "Time Left in Quarter")),
                               sliderInput("advanced_season_J", "Season for James", 2003, 2018, 2003, step = 1, 
                                           animate=animationOptions(interval=1000, loop = T,
                                                                    playButton = "Start", pauseButton = "Stop")),
                               sliderInput("advanced_season_D", "season for Durant", 2007, 2018, 2007, step = 1, 
                                           animate=animationOptions(interval=1000, loop = T,
                                                                    playButton = "Start", pauseButton = "Stop"))
                        ),
                        
                        column(width = 9,
                               h2(textOutput("advance_title")),
                               fluidRow(
                                 column(width = 9,
                                        h3(textOutput("advance_player_name2")),
                                        h4(textOutput("advance_player_info2")),
                                        plotOutput("advance_p1")
                                 ),
                                 column(width = 9,
                                        h3(textOutput("advance_player_name3")),
                                        h4(textOutput("advance_player_info3")),
                                        plotOutput("advance_p2")
                                 )
                               ),
                               div(align = "right",
                                   "Source: https://www.basketball-reference.com/"),
                               h3("Advanced summary stats for Lebron James"),
                               h4(textOutput("advance_summary_header_J")),
                               tableOutput("advance_summary_stats_J"),
                               h4(textOutput("Advanced summary stats for Kevin Durant")),
                               tableOutput("advance_summary_stats_D"),
                               div(align = "right",
                                   "Source: https://www.basketball-reference.com/"),
                               "Note:", br(),
                               "FG: Field Goals", br(),
                               "FGA: Field Goals Attempts", br(),
                               "FG%: Field Goals Percentage"
                        )
                      )
             ),
             tabPanel(title = "Record",
                      h1("Durant VS James"),
                      h2("Regular Season"),
                      fluidRow(
                        column(width = 6,
                               plotOutput("record_reg_plot1"),
                               div(align = "right",
                                   "Source: https://www.espn.com/nba/")),
                        column(width = 6,
                               fluidRow(
                                 column(width = 2,
                                        div(align = "center",
                                            uiOutput("J_MIA"))),
                                 column(width = 2),
                                 column(width = 2,
                                        div(align = "center",
                                            uiOutput("D_OKC"))
                                 )
                               ),
                               tableOutput("record_reg_table")
                               )
                      ),
                      h3("When Durant is in Thunder/SuperSonics while James in Heats"),
                      fluidRow(column(width = 6,
                             plotOutput("record_reg_plot2"),
                             div(align = "right",
                                 "Source: https://www.espn.com/nba/")),
                             column(width = 6,
                                    uiOutput("record_image1"))
                             ),
                      h3("When Durant is in Thunder while James in Caveliers"),
                      fluidRow(column(width = 6,
                             plotOutput("record_reg_plot3"),
                             div(align = "right",
                                 "Source: https://www.espn.com/nba/")),
                             column(width = 6,
                                    uiOutput("record_image2"))),
                      h3("When Durant is in Warrior while James in Caveliers/Lakers"),
                      fluidRow(column(width = 6,
                             plotOutput("record_reg_plot4"),
                             div(align = "right",
                                 "Source: https://www.espn.com/nba/")),
                             column(width = 6,
                                    uiOutput("record_image3"))),
                      h2("The NBA Final"),
                      h3("Oklahoma City Thunder VS Miami Heat"),
                      fluidRow(column(width = 6,
                             div(align = "center",
                                 h3("2011-2012 Final Stats")),
                             plotOutput("record_reg_plot5"),
                             div(align = "right",
                                 "Source: https://www.espn.com/nba/")),
                             column(width = 6,
                                    uiOutput("record_image4"))),
                      h3("Golden State Warriors VS Cleveland Cavaliers (Twice)"),
                      fluidRow(column(width = 6,
                             div(align = "center",
                                 h3("2016-2017 Final Stats")),
                             plotOutput("record_reg_plot6"),
                             div(align = "right",
                                 "Source: https://www.espn.com/nba/")),
                             column(width = 6,
                                    uiOutput("record_image5"))),
                      fluidRow(column(width = 6,
                             div(align = "center",
                                 h3("2017-2018 Final Stats")),
                             plotOutput("record_reg_plot7"),
                             div(align = "right",
                                 "Source: https://www.espn.com/nba/")),
                             column(width = 6,
                                    uiOutput("record_image6")))
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
  
  ## function to make season
  season.make <- function(start, end = NULL){
    if(is.null(end)){
      end = start
    }
    sapply(start:end, function(x){
      paste0(x, "-", x + 1)
    })
  }
  
  ## text
  output$advance_player_name <- renderText({input$advance_player_name})
  output$advance_player_info <- renderText({input$advance_data_type})
  output$advance_player_name2 <- renderText({
    paste("James in Season", season.make(input$advanced_season_J))
  })
  output$advance_player_name3 <- renderText({
    paste("Durant in Season", season.make(input$advanced_season_D))
  })
  output$advance_player_info2 <- renderText({input$advance_data_type_2})
  output$advance_player_info3 <- renderText({input$advance_data_type_2})
  output$advance_summary_header_J <- renderText({input$advance_data_type_2})
  output$advance_summary_header_D <- renderText({input$advance_data_type_2})
  
  ## table
  table.J <- reactive({
    James$shoot[[input$advance_data_type_2]] %>%
      filter(season == season.make(input$advanced_season_J)) %>%
      select(Value, FG, FGA, `FG%`)
  })
  output$advance_summary_stats_J <- renderTable({
    table.J()
  })
  table.D <- reactive({
    Durant$shoot[[input$advance_data_type_2]] %>%
      filter(season == season.make(input$advanced_season_D)) %>%
      select(Value, FG, FGA, `FG%`)
  })
  output$advance_summary_stats_D <- renderTable({
    table.D()
  })
  
  ## image
  output$advance_image <- renderText({
    url <- ifelse(input$advance_player_name == "Lebron James",
                  Image$James[[2]],
                  Image$Durant[[3]])
    return(createImage(url, width = 400))
  })
  
  ## Animate plot
  anigraph_1 <- reactive({
    if(input$advance_data_type_2 == "Game Location"){
      data <- James$shoot$`Game Location`
      data <- data[data$season == season.make(input$advanced_season_J),]
      
      g <- ggplot(data, aes(x = Value, y = `FGA`, fill = Value)) + 
        geom_bar(stat='identity') +
        labs(y="Field Goals Attempts", 
             x=NULL,
             color=NULL)+
        theme_light() +
        ylim(0, 1200) +
        coord_polar() +
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
      
      return(g)
    }
    if(input$advance_data_type_2 == "Shot Distance"){
      data <- James$shoot$`Shot Distance`
      data$Value <- factor(data$Value,
                           levels = unique(data$Value))
      data <- data[data$season == season.make(input$advanced_season_J),]
      
      g <- ggplot(data, aes(x=Value, y=`FG%`, color = Value)) + 
        geom_point(size=5) + 
        geom_segment(aes(x=Value, 
                         xend=Value, 
                         y=0, 
                         yend=`FG%`),
                     color="#b2b2b2", size=1) +
        labs(y="Field Goal Percentage",
             x="Time Left") + 
        ylim(0,1) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15),
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              legend.title = element_text(size =  20),
              legend.text = element_text(size = 15))
      
      return(g)
    }
    if(input$advance_data_type_2 == "Shot Type"){
      data <- James$shoot$`Shot Type`
      data <- data[data$season == season.make(input$advanced_season_J),]
      
      g <- ggplot(data, aes(x = Value, y = `FGA`, fill = Value)) + 
        geom_bar(stat='identity') +
        labs(y="Field Goals Attempts", 
             x=NULL,
             color=NULL)+
        theme_light() +
        ylim(0, 1650) +
        coord_polar() +
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
      
      return(g)
    }
    if(input$advance_data_type_2 == "Time Left in Quarter"){
      data <- James$shoot$`Time Left in Quarter`
      data$Value <- factor(data$Value,
                           levels = c( "< 3 minutes", "3-6 minutes", "> 6 minutes"))
      data <- data[data$season == season.make(input$advanced_season_J),]
      
      g <- ggplot(data, aes(x=Value, y=`FG%`, color = Value)) + 
        geom_point(size=5) + 
        geom_segment(aes(x=Value, 
                         xend=Value, 
                         y=0, 
                         yend=`FG%`),
                     color="#b2b2b2", size=1) +
        labs(y="Field Goal Percentage",
             x="Time Left") +
        theme_light() +
        ylim(0, 0.6) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15),
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              legend.title = element_text(size =  20),
              legend.text = element_text(size = 15))
      return(g)
    }
  })
  output$advance_p1 <- renderPlot({
    return(anigraph_1())
  })
  anigraph_2 <- reactive({
    if(input$advance_data_type_2 == "Game Location"){
      data <- Durant$shoot$`Game Location`
      data <- data[data$season == season.make(input$advanced_season_D),]
      
      g <- ggplot(data, aes(x = Value, y = `FGA`, fill = Value)) + 
        geom_bar(stat='identity') +
        labs(y="Field Goals Attempts", 
             x=NULL,
             color=NULL)+
        theme_light() +
        ylim(0, 1200) +
        coord_polar() +
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
      
      return(g)
    }
    if(input$advance_data_type_2 == "Shot Distance"){
      data <- Durant$shoot$`Shot Distance`
      data$Value <- factor(data$Value,
                           levels = unique(data$Value))
      data <- data[data$season == season.make(input$advanced_season_D),]
      
      g <- ggplot(data, aes(x=Value, y=`FG%`, color = Value)) + 
        geom_point(size=5) + 
        geom_segment(aes(x=Value, 
                         xend=Value, 
                         y=0, 
                         yend=`FG%`),
                     color="#b2b2b2", size=1) +
        labs(y="Field Goal Percentage",
             x="Time Left") + 
        ylim(0,1) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15),
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              legend.title = element_text(size =  20),
              legend.text = element_text(size = 15))
      return(g)
    }
    if(input$advance_data_type_2 == "Shot Type"){
      data <- Durant$shoot$`Shot Type`
      data <- data[data$season == season.make(input$advanced_season_D),]
      
      g <- ggplot(data, aes(x = Value, y = `FGA`, fill = Value)) + 
        geom_bar(stat='identity') +
        labs(y="Field Goals Attempts", 
             x=NULL,
             color=NULL)+
        theme_light() +
        ylim(0, 1650) +
        coord_polar() +
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15))
      
      return(g)
    }
    if(input$advance_data_type_2 == "Time Left in Quarter"){
      data <- Durant$shoot$`Time Left in Quarter`
      data$Value <- factor(data$Value,
                           levels = c( "< 3 minutes", "3-6 minutes", "> 6 minutes"))
      data <- data[data$season == season.make(input$advanced_season_D),]
      
      g <- ggplot(data, aes(x=Value, y=`FG%`, color = Value)) + 
        geom_point(size=5) + 
        geom_segment(aes(x=Value, 
                         xend=Value, 
                         y=0, 
                         yend=`FG%`),
                     color="#b2b2b2", size=1) +
        labs(y="Field Goal Percentage",
             x="Time Left") + 
        theme_light() +
        ylim(0,0.6) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15),
              axis.title = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              legend.title = element_text(size =  20),
              legend.text = element_text(size = 15))
      return(g)
    }
  })
  output$advance_p2 <- renderPlot({
    anigraph_2()
  })
  
  ## Overall plot
  output$advance_plot <- renderggiraph({
    if(input$advance_player_name == "Lebron James"){
      if(input$advance_data_type == "Game Location"){
        g <- ggplot() + 
          geom_col_interactive(data = James$shoot$`Game Location`,
                               aes(x = season, y = `FG%`, fill = Value, 
                                   tooltip = paste("FG%:",`FG%`)),
                               position = "dodge") +
          labs(x = "Season", y = "Field Goal Percentage") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
      if(input$advance_data_type == "Shot Distance"){
        g <- ggplot(James$shoot$`Shot Distance`) +
          geom_line(aes(x=season, y = `FG%`, group = Value, colour = Value), size = 1.5) +
          geom_point_interactive(aes(x=season, y = `FG%`,
                                     group = Value, colour = Value,
                                     tooltip = paste("FG%:", `FG%`)), size = 5) +
          labs(x = "Season", y = "Field Goal Percentage") + 
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
      if(input$advance_data_type == "Shot Type"){
        g <- ggplot(James$shoot$`Shot Type`) + 
          geom_line(aes(x=season, y = FGA, group = Value, colour = Value), size = 1.5) +
          geom_point_interactive(aes(x=season, y = FGA, group = Value, colour = Value,
                                     tooltip = paste("FGA:", FGA)), size = 5) +
          labs(x = "Season", y = "Field Goal Attempt") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
      if(input$advance_data_type == "Time Left in Quarter"){
        g <- ggplot() + 
          geom_col_interactive(data = Durant$shoot$`Time Left in Quarter`, aes(x = season, y = `FG%`, fill = Value, tooltip = paste("FG%:", `FG%`)), position = "dodge") +
          labs(x = "Season", y = "Field Goal Percentage") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
    }
    if(input$advance_player_name == "Kevin Durant"){
      if(input$advance_data_type == "Game Location"){
        g <- ggplot() + 
          geom_col_interactive(data = Durant$shoot$`Game Location`,
                               aes(x = season, y = `FG%`, fill = Value, 
                                   tooltip = paste("FG%:",`FG%`)),
                               position = "dodge") +
          labs(x = "Season", y = "Field Goal Percentage") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
      if(input$advance_data_type == "Shot Distance"){
        g <- ggplot(Durant$shoot$`Shot Distance`) +
          geom_line(aes(x=season, y = `FG%`, group = Value, colour = Value), size = 1.5) +
          geom_point_interactive(aes(x=season, y = `FG%`,
                                     group = Value, colour = Value,
                                     tooltip = paste("FG%:", `FG%`)), size = 5) +
          labs(x = "Season", y = "Field Goal Percentage") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
      if(input$advance_data_type == "Shot Type"){
        g <- ggplot(Durant$shoot$`Shot Type`) + 
          geom_line(aes(x=season, y = FGA, group = Value, colour = Value), size = 1.5) +
          geom_point_interactive(aes(x=season, y = FGA, group = Value, colour = Value,
                                     tooltip = paste("FGA:", FGA)), size = 5) +
          labs(x = "Season", y = "Field Goal Attempt") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
      if(input$advance_data_type == "Time Left in Quarter"){
        g <- ggplot() + 
          geom_col_interactive(data = Durant$shoot$`Time Left in Quarter`, aes(x = season, y = `FG%`, fill = Value, tooltip = paste("FG%:", `FG%`)), position = "dodge") +
          labs(x = "Season", y = "Field Goal Percentage") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
                axis.title = element_text(size = 20),
                axis.text.y = element_text(size = 15),
                legend.title = element_text(size =  20),
                legend.text = element_text(size = 15))
        return(girafe(code = print(g)))
      }
    }
  })
  
  # Record Page
  
  ## Image
  output$D_OKC <- renderText({
    createImage(Image$profile.photo[1], 150)
  })
  output$J_MIA <- renderText({
    createImage(Image$profile.photo[5], 200)
  })
  output$record_image1 <- renderText({
    createImage(Image$James.vs.Durant[[5]], 400)
  })
  output$record_image2 <- renderText({
    createImage(Image$James.vs.Durant[[6]], 400)
  })
  output$record_image3 <- renderText({
    createImage(Image$James.vs.Durant[[2]], 400)
  })
  output$record_image4 <- renderText({
    createImage(Image$James.vs.Durant[[1]], 400)
  })
  output$record_image5 <- renderText({
    createImage(Image$James.vs.Durant[[4]], 400)
  })
  output$record_image6 <- renderText({
    createImage(Image$James.vs.Durant[[3]], 400)
  })
  
  ## plot
  direct[is.na(direct)] <- 0  
  regular <- direct$regular
  playoff <- direct$playoff
  regular1 <- regular[1:26,]
  regular2 <- regular[27:32,]
  regular3 <- regular[-c(1:32),]
  final.2012 <- playoff[1:10,]
  final.2017 <- playoff[11:20,]
  final.2018 <- playoff[21:28,]
  
  compare.plot <- function(game){
    game.sum <- game%>%
      group_by(player)%>%
      summarize("Minutes" = mean(MP, na.rm = T) ,"Points" = mean(PTS, na.rm = T), "Rebounds" = mean(TRB, na.rm = T),
                "Assists" = mean(AST, na.rm = T), "Blocks" = mean(BLK, na.rm = T), "Steels" = mean(STL, na.rm = T), 
                "Field Goals" = mean(`FG%`, na.rm = T), "Three-points" = mean(`3P%`, na.rm = T), "Free Throw" = mean(`FT%`))
    
    
    game.diff <- rbind(data.frame(-diff(as.matrix(game.sum[,-1]))),
                       data.frame(diff(as.matrix(game.sum[,-1]))))
    game.diff[game.diff < 0] <- 0 
    game.diff[2,] <- -game.diff[2,]
    game.summary <- cbind(game.sum[,1],game.diff)%>%
      gather(key = "category", value = "value", -player)
    
    ggplot(game.summary, aes(x = category, y = value, group = category))+
      geom_bar(stat='identity', aes(fill= player),width = .5)+
      scale_fill_manual(name="Player", 
                        labels = c("Durant", "James"), 
                        values = c("James"="#00ba38", "Durant"="#f8766d")) + 
      labs(x = "", y = "Winning Percentage")+
      scale_y_continuous(labels = NULL) +
      coord_flip() +
      theme_light() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            legend.title = element_text(size =  20),
            legend.text = element_text(size = 15))
  }
  output$record_reg_table <- renderTable({
    regular %>%
      group_by(player)%>%
      summarize("MIN" = mean(MP) ,"PTS" = mean(PTS), "REB" = mean(TRB),
                "AST" = mean(AST), "BLK" = mean(BLK), "STL" = mean(STL), 
                "FG%" = mean(`FG%`), "3P%" = mean(`3P%`), "FT%" = mean(`FT%`, na.rm = T))
  })
  
  output$record_reg_plot1 <- renderPlot(
    compare.plot(regular)
  )
  
  output$record_reg_plot2 <- renderPlot(
    compare.plot(regular1)
  )
  
  output$record_reg_plot3 <- renderPlot(
    compare.plot(regular2)
  )
  
  output$record_reg_plot4 <- renderPlot(
    compare.plot(regular3)
  )
  
  output$record_reg_plot5 <- renderPlot(
    compare.plot(final.2012)
  )
  
  output$record_reg_plot6 <- renderPlot(
    compare.plot(final.2017)
  )
  
  output$record_reg_plot7 <- renderPlot(
    compare.plot(final.2018)
  )
}


shinyApp(ui, server)