library(shiny)


shinyUI(fluidPage(
  titlePanel("Simple Wordcount Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      #h4("Check out what I've been writing"),
      p("DESCRIPTION HERE ABOUT MY APP"),
      
      dateRangeInput("daterange", 
                     label = "Date range"),
      #actionButton("submit0", label = "See Nicaless' Stats"),
      
      h4("See how the world is writing this NaNoWriMo"),
      selectInput("nano_site", 
                  label = "Site Stats",
                  choices = list("WordCount",
                                 "Average")
                                 ),
      
      p("DESCRIPTION HERE about where to find regions and being comma separated"),
      textInput("region",
                label = "Region",
                value = "usa-california-san-francisco"),
      selectInput("region_val", 
                  label = "Region Stats",
                  choices = list("Donations",
                                 "Donors",
                                 "Participants",
                                 "WCAverage")
                  ),

      p("DESCRIPTION HERE about where to find regions and being comma separated"),
      textInput("user",
                label = "Username",
                value = "nicaless"),

      h4("What have you written today?"),
      p("Description about how to re-render above graph and that how it won't be saved"),
      
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
      
    mainPanel(h4("Words I've Written over Time"),
              plotOutput("main"),
              h4("NaNoWriMo Stats"),
              htmlOutput("NaNo_in_session"),
              h5("Site Stats"),
              plotOutput("nano_site"),
              h5("Region Stats"),
              tableOutput("nano_region_summary"),
              plotOutput("nano_region_history"),
              h5("User Stats"),
              plotOutput("nano_user_summary"),
              plotOutput("nano_user_history")
    )
  )
))