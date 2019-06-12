# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Lonely Cards Simulation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput('replications',
                  'Number of Simulations:',
                  min=10000,
                  max=1000000,
                  step=1000,
                  value=1000),
      helpText("A simulation-based approach to this Riddler Express puzzle on 538:",
               "http://fivethirtyeight.com/features/can-you-deal-with-these-card-game-puzzles/",
               "One simulation is one 'game.'")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      mainPanel(
        plotOutput("outcome"),
        textOutput("percentage")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cards <- function() {
    deck <- rep(c('ace', as.character(seq(2:10)), 'jack', 'queen', 'king'), times=4)
    return(as.numeric(!(sum(deck == sample(deck)) > 0))) #1 is win, 0 is lose
  }
  
  #plot proportions, not frequency
  output$outcome <- renderPlot({
    results <- replicate(input$replications, cards())
    ggplot() +
      theme_minimal() +
      geom_bar(aes(x=as.factor(results), y=..count../sum(..count..)), 
                     color='darkblue', fill='darkblue', position='identity') +
      scale_x_discrete(labels=c('Lose', 'Win')) +
      xlab("Outcome") +
      ylab('Proportion')
  })
  output$percentage <-renderText({
    results <- replicate(input$replications, cards())
    paste("You win ", round(sum(results/input$replications) * 100, 2), "% of the time.", sep='')
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
