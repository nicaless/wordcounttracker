library(shiny)
#library(ggplot2)
#library(reshape2)
source("wordCountTracker.R")
source("nanowrimo.R")

shinyServer(function(input, output) {
  ## Year Round
  output$main <- renderPlot({
    plotWC_proj(minDate = input$daterange[1], maxDate = input$daterange[2], plotby = input$main_val)
  })
  
  output$main_summary <- renderTable({
    getWC_Summary(minDate = input$daterange[1], maxDate = input$daterange[2])
  }, include.rownames=FALSE)
  
  output$main_projs <- renderTable({
    getWC_Table(minDate = input$daterange[1], maxDate = input$daterange[2])
  }, include.rownames=FALSE)
  
  submitWC <- eventReactive(input$submit3, {
    recordWC(input$words, input$date, input$project, input$writer)
  })
  
  output$confirm <- renderText({
    submitWC()
  })
  
  output$past_site <- renderPlot({
    plotSiteHistory(input$past, input$past_graph_type)
  })
  
  output$past_regions <- renderPlot({
    plotRegionHistoryCorr(input$past_regions, input$past, input$past_plotx, input$past_ploty, input$past_graph_type)
  })
  
  output$past_regions_top <- renderTable({
    printWinnerTable(input$past_regionsm, input$past)
  }, include.rownames=FALSE)
  
  output$past_site_summary <- renderTable({
    printSiteTable(input$past)
  }, include.rownames=FALSE)
  
  
  # Only when NaNo in session
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
  
  output$nano_user_forecast <- renderPlot({
    forecastCompletion(input$user[1])
  })

  })