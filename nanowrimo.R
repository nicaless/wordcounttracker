library(ggplot2)
library(XML)

#### Site Word Count History

getSiteSummary <- function() {
  xml.url.site = "http://nanowrimo.org/wordcount_api/wcstats"
  xmlfile.site = xmlTreeParse(xml.url.site)
  xmltop.site = xmlRoot(xmlfile.site)
  
  sitewordcount = as.numeric(unname(unlist(xmltop.site[[1]][[1]]))[2])
  numparticipants = as.numeric(unname(unlist(xmltop.site[[2]][[1]]))[2])
  
  sitesummary = data.frame(SiteWordCount = sitewordcount, TotalParticipants = numparticipants)
  return(sitesummary)
}

getSiteHistory <- function() {
  xml.url.site = "http://nanowrimo.org/wordcount_api/wcstats"
  xmlfile.site = xmlTreeParse(xml.url.site)
  xmltop.site = xmlRoot(xmlfile.site)
  
  sitehistory = xmlSApply(xmltop.site[[3]], function(x) xmlSApply(x, xmlValue))
  sitehistory = data.frame(t(sitehistory), row.names = NULL)
  names(sitehistory) = c("WordCount", "Date", "Min", "Max", "Average", "StDev", "Count")
  sitehistory = data.frame(Date = as.Date(sitehistory$Date), 
                         WordCount = as.numeric(sitehistory$WordCount),
                         Average = as.numeric(sitehistory$Average),
                         Min = as.numeric(sitehistory$Min),
                         Max = as.numeric(sitehistory$Max),
                         StDev = as.numeric(sitehistory$StDev),
                         Count = as.numeric(sitehistory$Count))
  return(sitehistory)
}

plotWC_Site <- function(val = "WordCount") {
  sitehistory = getSiteSummary()
  if (val == "WordCount") {
    ggplot(data = sitehistory, aes(x = Date, y = WordCount)) + geom_line()
  }
  if (val == "Average") {
    ggplot(data = sitehistory, aes(x = Date, y = Average)) + geom_line()
  }
  if (val == "Min") {
    ggplot(data = sitehistory, aes(x = Date, y = Min)) + geom_line()
  }
  if (val == "Max") {
    ggplot(data = sitehistory, aes(x = Date, y = Max)) + geom_line()
  }
  if (val == "StDev") {
    ggplot(data = sitehistory, aes(x = Date, y = StDev)) + geom_line()
  }
  if (val == "Count") {
    ggplot(data = sitehistory, aes(x = Date, y = Count)) + geom_line()
  }
}

printSummary_Site <- function() {
  sitesummary = getSiteSummary()
  print(sitesummary)
}


#### Individual Word Count History

getUserSummary <- function(username) {
  url.path = "http://nanowrimo.org/wordcount_api/wchistory/"
  user.path = paste(url.path, i, sep = "")
  
  user.path = "http://nanowrimo.org/wordcount_api/wchistory/nicaless"
  xmlfile.user = xmlTreeParse(user.path)
  xmltop.user = xmlRoot(xmlfile.user)
  username = unname(unlist(xmltop.user[[2]][[1]]))[2]
  totalwordcount = as.numeric(unname(unlist(xmltop.user[[3]][[1]]))[2])
  winner = unname(unlist(xmltop.user[[4]][[1]]))[2]
  usersummary = data.frame(User = username, TotalWordCount = totalwordcount, Winner = winner)
  usersummary[, 1] = as.character(usersummary[, 1])
  usersummary[, 3] = as.character(usersummary[, 3])
  return(usersummary)
}

###
getUserHistory <- function(username) {
  userhistory = xmlSApply(xmltop.user[[5]], function(x) xmlSApply(x, xmlValue))
  userhistory = data.frame(t(userhistory), row.names = NULL)
  return(userhistory)
}

plotSummary_Users <- function(user) {
  usernames = unlist(strsplit(user))
  for (i in usernames) {
    u = getUserSummary(i)
    if (exists("usersummaries")) {
      usersummaries = rbind(usersummaries, u)
    } else {
      usersummaries = u
    }
  }
  ggplot(data = usersummaries, aes(x = User, y = TotalWordCount, group = Winner, fill = Winner)) + geom_bar(stat = "identity") + coord_flip()
  
}

###
plotWC_Users <- function() {
  usernames = unlist(strsplit(user))
  for (i in usernames) {
    u = getUserHistory(i)
    if (exists("userhistories")) {
      userhistories = rbind(userhistories, u)
    } else {
      userhistories = u
    }
  }
  ggplot(data = userhistories, aes(x = Date, y = WordCount, group = Writer, colour = Writer)) + geom_line()
}

#### Regional Word Count Summary



