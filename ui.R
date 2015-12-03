library(shiny)


past_region_history_data <<- read.csv("data/regionhistory_11_2015.csv", stringsAsFactors = F)
regions_list = unique(past_region_history_data$Region)

shinyUI(fluidPage(
  titlePanel("Simple Word Count Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      p("We adore the NaNoWriMo word count tracker, so we decided to make one of our own to use year round!"),
      p("For fun, we also played around with NaNoWriMo's 'wcapi' to visualize everyone's novel-writing progress with R. Take a look!"),
      p("You can find the code for this app at https://github.com/nicaless/wordcounttracker"),
      p("Send and questions/concerns/feedback to nfronda@gmail.com"),
      
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
      ##add more info here later
      selectInput("past",
                  label = "Past WriMos",
                  choices = list("November 2015"),
                  selected = list("November 2015"),
                  multiple = T),
      selectInput("past_regions",
                  label = "Regions",
                  choices = regions_list,
                  selected = list("USA :: California :: San Francisco","Europe :: England :: London",
                                  "Europe :: France :: Paris","USA :: New York :: New York City"),
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
                  selected = "bar")
  
      
      
      
      #### Only for when NaNoWriMo is in Session CLEAN UP BEFORE NEXT NANOWRIMO
#       h4("See how the world is writing this NaNoWriMo"),
#       selectInput("nano_site", 
#                   label = "Site Stats",
#                   choices = list("WordCount",
#                                  "Average")
#                                  ),
#       h5("Region Stats"),
#       p("A list of all valid Regions can be found on the NaNoWriMo website. 
#         Follow the example below to enter a region name and see its stats. 
#         Enter multiple regions separated by commas."),
#       textInput("region",
#                 label = "Region",
#                 value = "usa-california-san-francisco"),
#       selectInput("region_val", 
#                   label = "Metric",
#                   choices = list("Donations",
#                                  "Donors",
#                                  "Participants",
#                                  "WCAverage")
#                   ),
#       h5("User Stats"),
#       p("Enter a WriMos' username below to see their progress!
#         Enter multiple usernames separated by commas."),
#       textInput("user",
#                 label = "Username",
#                 value = "nicaless")
  ),
      
    mainPanel(h4("Words Written over Time"),
              ### Year Round
              plotOutput("main"),
              tableOutput("main_summary"),
              tableOutput("main_projs"),
              
              h4("Past NaNoWriMo Stats"),
              plotOutput("past_regions"),
              p("Top Regions of past WriMos (if multiple WriMos selected, shows Top Regions over all selected WriMos"),
              tableOutput("past_regions_top"),
              p("Site Stats of past WriMos"),
              tableOutput("past_site")
              
              ### Only when NaNoWriMo in Session
#               h4("NaNoWriMo Stats"),
#               htmlOutput("NaNo_in_session"),
#               h5("Site Stats"),
#               plotOutput("nano_site"),
#               h5("Region Stats"),
#               tableOutput("nano_region_summary"),
#               plotOutput("nano_region_history"),
#               h5("User Stats"),
#               plotOutput("nano_user_summary"),
#               plotOutput("nano_user_history"),
#               plotOutput("nano_user_forecast")
    )
  )
))