library(shiny)


past_region_history_data <- read.csv("data/regionnames.csv", stringsAsFactors = F)
regions_list = unique(past_region_history_data$RegionNames)

shinyUI(fluidPage(
  titlePanel("Simple Word Count Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      div(
        p("I love the NaNoWriMo word count tracker, so I decided to make one of my own to use year round!"), 
        p("For fun, I also played around with NaNoWriMo's word count web API to visualize everyone's novel-writing progress with R. Take a look!"),
        HTML(paste("You can find the code for this app on ", a(href="https://github.com/nicaless/wordcounttracker", "my GitHub"), 
                   " and read more about it on ", 
                   a(href="http://nicaless.github.io/2015/11/09/My%20First%20Shiny%20App.html", "my blog."))),
        br(),
        br(),
        p("Send any questions/concerns/feedback to nfronda@gmail.com"),
        p("DISCLAIMER: I am in no way affiliated with NaNoWriMo. This project was created using publicly available data from NaNoWriMo.org for personal research interests.")),
      
      ### Year Round - My Personal Writing (and others who enter their writing stats)
      h4("My Personal Word Count Tracker. See what I'm writing!"),
      dateRangeInput("daterange", 
                     label = "Date range",
                     start = Sys.Date() - 60,
                     end = Sys.Date()),
      selectInput("main_val", 
                  label = "Writer/Project",
                  choices = list("Project",
                                 "Writer")),
      
      h4("What have you written today?"),
      p("Submit your word count in the form below and see it plotted!"),
      
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
      
      textOutput("confirm"),
      
      h4("See how the world wrote in past NaNoWriMos"),
      selectInput("past",
                  label = "Past NaNoWriMo Date",
                  choices = list("November 2015"),
                  selected = list("November 2015"),
                  multiple = T),
      selectInput("past_plotx",
                  label = "X axis value",
                  choices = list("Date", "Min","Max","WCAverage","StDev","Participants",
                                 "Donations","Donors","Region","DonationParticipantRatio",
                                 "DonorParticipantRatio"),
                  selected = "Date"),
      selectInput("past_ploty",
                  label = "Y axis value",
                  choices = list("Date", "Min","Max","WCAverage","StDev","Participants",
                                 "Donations","Donors","Region","DonationParticipantRatio",
                                 "DonorParticipantRatio"),
                  selected = "WCAverage"),
      selectInput("past_graph_type",
                  label = "Graph Type",
                  choices = list("bar","scatter","line"),
                  selected = "bar"),
      selectInput("past_regions",
                  label = "Regions",
                  choices = regions_list,
                  selected = list("USA :: California :: San Francisco","Europe :: England :: London",
                                  "Europe :: France :: Paris","USA :: New York :: New York City"),
                  multiple = T),
  
      
      
      
      #### Only for when NaNoWriMo is in Session CLEAN UP BEFORE NEXT NANOWRIMO
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
      # Might change this so regions are a select input as above 
      #and then parse to the '-' format from the '::' format
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
                value = "nicaless")
  ),
      
    mainPanel(h4("Words Written over Time"),
              ### Year Round
              plotOutput("main"),
              tableOutput("main_summary"),
              tableOutput("main_projs"),
              
              h4("Past NaNoWriMo Stats"),
              p(strong("Past Site Data")),
              plotOutput("past_site"),
              p(strong("Past Regional Data")),
              plotOutput("past_regions"),
              p(strong("Top Regions of past NaNoWriMos (if multiple sessions selected, shows Top Regions over all selected NaNoWriMos)")),
              tableOutput("past_regions_top"),
              p(strong("Site Stats of past NaNoWriMos")),
              tableOutput("past_site_summary"),
              
              ### Only when NaNoWriMo in Session
              h4("Current NaNoWriMo Stats"),
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