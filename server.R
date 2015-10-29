library(shiny)
library(ggplot2)
library(reshape2)
source("wordCountTracker.R")

shinyServer(function(input, output) {
  output$main <- renderPlot({
    plotWC_proj(minDate = input$daterange[1], maxDate = input$daterange[2])
  })
  
  submitWC <- eventReactive(input$submit3, {
    recordWC(input$words, input$date, input$project, input$writer)
  })
  
  output$confirm <- renderText({
    submitWC()
  })

  })