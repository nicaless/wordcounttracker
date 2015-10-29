library(ggplot2)
library(reshape2)

my_records = read.csv("data/tracker.csv", header = T, stringsAsFactors = F)
my_nic_records = read.csv("data/trackernicaless.csv", header = T, stringsAsFactors = F)

my_records$Date = as.Date(my_records$Date)
recordWC <- function(wordcount, date, project, writer) {
  date = as.Date(date)
  if (wordcount <= 0) {
    return()
  }
  if(grepl("recordNicaless", project) & writer == "recordNicaless.nicaless") {
    project = unlist(strsplit(project, "[.]"))[2]
    writer = unlist(strsplit(writer, "[.]"))[2]
    nic_record = data.frame(Date = date, Project = project, Writer = writer, Word.Count = wordcount)
    my_nic_records <<- rbind(my_nic_records, nic_record)
    write.csv(my_records, file = "data/trackernicaless.csv", row.names = F)
  }
  if (project == "Enter the name of your project...") {
    project = "Untitled"
  }
  if (writer == "Enter your name...") {
    writer = "anonymous"
  }
  new_record = data.frame(Date = date, Project = project, Writer = writer, Word.Count = wordcount)
  my_records <<- rbind(my_records, new_record)
  write.csv(my_records, file = "data/tracker.csv", row.names = F)
  paste(writer, " submitted ", 
        wordcount, " for ", 
        project, 
        " on ", date, 
        sep = "")
}

#NEED TABLE WRITER + PROJECT + CUM WORD COUNT
#NEED TABLE DISPLAYING PROJECTS/WRITERS MEETING 500 DAILY WORD COUNT AVERAGE, 2500 WEEKLY WORD COUNT AVERAGE

#NEED SEARCH OPTIONS FOR PROJECT WRITER 
#NEED CUMULATIVE OPTION

plotWC_proj <- function(minDate = Sys.Date(), maxDate = Sys.Date()) {
  rawData = my_records
  rawData$Date = as.Date(rawData$Date)
  rawData$Word.Count = as.numeric(rawData$Word.Count)
  if (minDate != Sys.Date() & maxDate != Sys.Date()) {
    rawData = subset(rawData, Date >= minDate)
    rawData = subset(rawData, Date <= maxDate)
  }
  
  ggplot(data = rawData, aes(x = Date, y = Word.Count, group = Project, colour = Project)) + geom_line()
}

plotWC_writer <- function(minDate = Sys.Date(), maxDate = Sys.Date()) {
  rawData = my_records
  rawData$Date = as.Date(rawData$Date)
  rawData$Word.Count = as.numeric(rawData$Word.Count)
  if (minDate != Sys.Date() & maxDate != Sys.Date()) {
    rawData = subset(rawData, Date >= minDate)
    rawData = subset(rawData, Date <= maxDate)
  }
  ggplot(data = rawData, aes(x = Date, y = Word.Count, group = Writer, colour = Writer)) + geom_line()
}
