library(shiny)


shinyUI(fluidPage(
  titlePanel("Simple Word Count Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      p("We adore the NaNoWriMo word count tracker, so we decided to make one of our own to use year round!"),
      p("For fun, we also played around with NaNoWriMo's 'wcapi' to visualize everyone's novel-writing progress with R. Take a look!"),
      p("You can find the code for this app at https://github.com/nicaless/wordcounttracker"),
      p("Send and questions/concerns/feedback to nfronda@gmail.com"),
      
      dateRangeInput("daterange", 
                     label = "Date range",
                     start = Sys.Date() - 60,
                     end = Sys.Date()),
      selectInput("main_val", 
                  label = "Writer/Project",
                  choices = list("Project",
                                 "Writer")
      ),

      h4("See how the world is writing this NaNoWriMo"),
      selectInput("nano_site", 
                  label = "Site Stats",
                  choices = list("WordCount",
                                 "Average")
                                 ),
      h5("Region Stats"),
      p("A list of all valid Regions can be found on the NaNoWriMo website. 
        Follow the example below to enter a region name and see its stats. 
        Enter multiple regions separated by commas."),
      textInput("region",
                label = "Region",
                value = "usa-california-san-francisco"),
      selectInput("region_val", 
                  label = "Metric",
                  choices = list("Donations",
                                 "Donors",
                                 "Participants",
                                 "WCAverage")
                  ),
      h5("User Stats"),
      p("Enter a WriMos' username below to see their progress!
        Enter multiple usernames separated by commas."),
      textInput("user",
                label = "Username",
                value = "nicaless"),

      h4("What have you written today?"),
      p("Submit your word count in the form below and see it plotted above! Unfortunately, the app currently does not save any entered data permanently, but we're working on it!"),
      
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
      
    mainPanel(h4("Words Written over Time"),
              plotOutput("main"),
              tableOutput("main_summary"),
              tableOutput("main_projs"),
              h4("NaNoWriMo Stats"),
              htmlOutput("NaNo_in_session"),
              h5("Site Stats"),
              plotOutput("nano_site"),
              h5("Region Stats"),
              tableOutput("nano_region_summary"),
              plotOutput("nano_region_history"),
              h5("User Stats"),
              plotOutput("nano_user_summary"),
              plotOutput("nano_user_history"),
              plotOutput("nano_user_forecast")
    )
  )
))