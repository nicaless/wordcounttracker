library(shiny)

shinyUI(fluidPage(
  titlePanel("Simple Wordcount Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Check it out!"),
      p("select..."),
      
      h4("See how the world is writing this NaNoWriMo"),
      
      textInput("region",
                label = "Region",
                value = "World"),
      
      actionButton("submit0", label = "Render Map"),
      
      h4("See WriMos' current stats"),
      
      textInput("user",
                label = "Username",
                value = "Enter NaNoWriMo usernames separated by commas..."),
      
      actionButton("submit1", label = "See Stats"),
      
      h4("See what writers have entered into the Simple Wordcount Tracker"),
      dateRangeInput("daterange", 
                     label = "Date range"),
      #actionButton("submit2", label = "See Stats"),
      
      h4("What have you written today?"),
      
      numericInput("words",
                  label = "Words",
                  value = 0),
      textInput("project",
                label = "Project",
                value = "Enter the name of your project..."),
      textInput("writer",
                label = "Writer",
                value = "Enter your name..."),
      dateInput("date", 
                label = "Date", 
                value = Sys.Date()),
      actionButton("submit3", label = "Submit"),
      
      textOutput("confirm")
    ),
      
    mainPanel(h4("Wordcount over Time"),
              plotOutput("main"),
              plotOutput("support")
              #textOutput("test")
    )
  )
))