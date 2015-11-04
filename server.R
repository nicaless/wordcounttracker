library(shiny)
library(ggplot2)
library(reshape2)
source("wordCountTracker.R")
source("nanowrimo.R")

shinyServer(function(input, output) {
  output$main <- renderPlot({
    plotWC_proj(minDate = input$daterange[1], maxDate = input$daterange[2])
  })
  
  output$NaNo_in_session <- renderUI({
    printSummary_Site()
  })
  
  output$nano_site <- renderPlot({
    plotWC_Site(input$nano_site)
  })
  
  output$nano_region_summary <- renderTable({
    getRegionSummary(input$region)
  }, include.rownames=FALSE)
  
  output$nano_region_history <- renderPlot({
    plotHistory_Regions(input$region, val = input$region_val)
  })
  
  output$nano_user_summary <- renderPlot({
    plotSummary_Users(input$user)
  })
  
  output$nano_user_history <- renderPlot({
    plotHistory_Users(input$user)
  })
  
  submitWC <- eventReactive(input$submit3, {
    recordWC(input$words, input$date, input$project, input$writer)
  })
  
  output$confirm <- renderText({
    submitWC()
  })

  })