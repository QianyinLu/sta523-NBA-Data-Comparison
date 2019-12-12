suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(tidyverse))

James <- readRDS("data/james.rds")
Durant <- readRDS("data/durant.rds")

season.make <- function(start, end = NULL){
  if(is.null(end)){
    end = start
  }
  sapply(start:end, function(x){
    paste0(x, "-", x + 1)
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "James VS Durant"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Regular & Playoff", tabName = "rp", icon = icon("th")),
      menuItem("Shooting", tabName = "sh", icon = icon("th")),
      menuItem("Head to head", tabName = "hh", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home
      tabItem(tabName = "home",
              h1("Home page"),
              br(),
              hr()
      ),
      
      # Regular & Playoff
      tabItem(tabName = "rp",
              h1("Regular & Playoff"),
              fluidRow(
                     box(width = 6,
                       selectInput(inputId = "rpseasonD", label = "Season for Kevin Durant",
                                   choices = season.make(2007, 2019),
                                   selected = "2018-2019"),
                       selectInput(inputId = "rorpD", label = "Period",
                                   choices = c("Regular", "Playoff"),
                                   selected = "Regular"),
                       actionButton(inputId = "rpactionD", label = "Show")
                     ),
                     box(width = 6,
                       selectInput(inputId = "rpseasonJ", label = "Season for Lebron James",
                                   choices = season.make(2003, 2019),
                                   selected = "2018-2019"),
                       selectInput(inputId = "rorpJ", label = "Period",
                                   choices = c("Regular", "Playoff"),
                                   selected = "Regular"),
                       actionButton(inputId = "rpactionJ", label = "Show")
                     )
                     ),
              fluidRow(
                     box(width = 6,
                         DT::dataTableOutput(outputId = "rpD")
                     ),
                     box(width = 6,
                         DT::dataTableOutput(outputId = "rpJ")
                     )
              ),
      ),
      
      # Shooting
      tabItem(tabName = "sh",
              h1("Shooting")
      ),
      
      # Home
      tabItem(tabName = "hh",
              h1("Head to head")
      )
    )
  )
)

server <- function(input, output){
  D.rp <- eventReactive(input$rpactionD, {
    if(input$rorpD == "Regular"){
      Durant$avg[Durant$avg$season == input$rpseasonD, ] %>%
        select(-season, -Team, -GP, -GS, -FG, -FGA, -`3P`, -`3PA`,
               -FT, -FTA)
    }
    else{
      
    }
  })
  output$rpD <- DT::renderDataTable({
    rp <- as.data.frame(t(as.matrix.data.frame(D.rp())))
  })
  J.rp <- eventReactive(input$rpactionJ, {
    if(input$rorpD == "Regular"){
      James$avg[James$avg$season == input$rpseasonJ, ] %>%
        select(-season, -Team, -GP, -GS, -FG, -FGA, -`3P`, -`3PA`,
               -FT, -FTA)
    }
    else{
      
    }
  })
  output$rpJ <- DT::renderDataTable({
    rp <- as.data.frame(t(as.matrix.data.frame(J.rp())))
  })
}

shinyApp(ui, server)