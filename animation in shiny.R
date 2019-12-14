ui <- fluidPage(
  ## Set up loop target
  sliderInput("theMonth", "Month", 1, 8, 1, step = 1, 
              animate=animationOptions(interval=1000, loop = T,
                                       playButton = T, pauseButton = T)),
  plotOutput(outputId="case_age_plot")
)

server <- function(input, output){
  anigraph <- reactive({
    # extract specific month. Since month will change automatically, it keeps renewed.
    data <- data[data$Month == input$theMonth,]
    # g <- ggplot .....
    # return(g)
  })
  
  output$case_age_plot <- renderPlot({
    anigraph() # parentheses are essensial
    
  })
}

