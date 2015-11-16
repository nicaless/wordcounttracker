library(ggplot2)
library(reshape2)
library(rdrop2)
library(googlesheets)

token <- drop_auth()
saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)

outputDir <- "wordcounttrackerdata"

saveData <- function(data) {
  #data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir, dtoken = token)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir, dtoken = token)
  filePaths <- filesInfo$path
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  return(data)
}

my_records <<- loadData()
my_records$Date = as.Date(my_records$Date)
all_records <<- my_records
MinDate <<- min(all_records$Date)

recordWC <- function(wordcount, date, project, writer) {
  date = as.Date(date)
  
  if (wordcount <= 0) {
    return()
  }
  if (project == "Enter the name of your project...") {
      project = "Untitled"
  }
  if (writer == "Enter your name...") {
      writer = "anonymous"
  }
  new_record = data.frame(Date = date, Project = project, WordCount = wordcount, Writer = writer)
  all_records <<- rbind(all_records, new_record)
  #saveData(new_record)
  paste(writer, " submitted ", 
      wordcount, " for ", 
      project, 
      " on ", date, 
      sep = "")
}


plotWC_proj <- function(minDate = MinDate, maxDate = Sys.Date(), plotby = "Project") {
  rawData = all_records
  rawData$Date = as.Date(rawData$Date)
  rawData$WordCount = as.numeric(rawData$WordCount)
#   if (minDate >= maxDate) {
#     minDate = maxDate - 30
#   }
  if (minDate == MinDate & maxDate - minDate > 700) {
    minDate = maxDate = 60
  }
  rawData = subset(rawData, Date >= minDate)
  rawData = subset(rawData, Date <= maxDate)
  
  if (plotby == "Writer") {
    ggplot(data = rawData, aes(x = Date, y = WordCount, group = Writer, colour = Writer)) + geom_point() + scale_colour_hue(l = 45)
  } else {
    ggplot(data = rawData, aes(x = Date, y = WordCount, group = Project, colour = Project)) + geom_point() + scale_colour_hue(l = 45)
  }
}

getWC_Summary <- function(minDate = MinDate, maxDate = Sys.Date()) {
  rawData = all_records
  rawData$Date = as.Date(rawData$Date)
  rawData$WordCount = as.numeric(rawData$WordCount)
  
#   if (minDate >= maxDate) {
#     minDate = maxDate - 30
#   }
  rawData = subset(rawData, Date >= minDate)
  rawData = subset(rawData, Date <= maxDate)
  
  avg_day = sum(rawData$WordCount) / length(unique(rawData$Date))
  max_wc = max(rawData$WordCount)
  topproj = dcast(rawData, Project + Writer ~., value.var = "WordCount", sum)
  topproj = topproj[topproj[, 3] == max(topproj[, 3]), ]
  topproj = topproj[1, ]
  summary = data.frame(AverageWCSubmission = avg_day,
                       MaxWCSubmission = max_wc,
                       TopWriter = topproj[1, 2],
                       TopProject = topproj[1, 1],
                       TopProjectWC = topproj[1, 3])
  return(summary)
}

getWC_Table <- function(minDate = MinDate, maxDate = Sys.Date()) {
  rawData = all_records
  rawData$Date = as.Date(rawData$Date)
  rawData$WordCount = as.numeric(rawData$WordCount)
#   if (minDate >= maxDate) {
#     minDate = maxDate - 30
#   }
  rawData = subset(rawData, Date >= minDate)
  rawData = subset(rawData, Date <= maxDate)
  
  projs = dcast(rawData, Project + Writer ~., value.var = "WordCount", sum, na.rm = T)
  names(projs)[3] = "Word Count"
  return(projs)
}