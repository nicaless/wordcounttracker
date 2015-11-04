library(ggplot2)
library(XML)

#### Color Palette
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00")

#### Site Word Count History

getXML <- function(site) {
  xml.url.site = site
  xmlfile.site = xmlParse(xml.url.site)
  xmltop.site = xmlRoot(xmlfile.site)
  return(xmltop.site)
}

xmltop.site <<- getXML("http://nanowrimo.org/wordcount_api/wcstats")

getSiteSummary <- function() {
  #sitewordcount = as.numeric(unname(unlist(xmltop.site[[1]][[1]]))[2])
  #numparticipants = as.numeric(unname(unlist(xmltop.site[[2]][[1]]))[2])
  sitewordcount = as.numeric(xmlValue(xmltop.site[[1]]))
  numparticipants = as.numeric(xmlValue(xmltop.site[[2]]))
  
  sitesummary = data.frame(SiteWordCount = sitewordcount, TotalParticipants = numparticipants)
  return(sitesummary)
}

getSiteHistory <- function() {
  sitehistory = xmlSApply(xmltop.site[[3]], function(x) xmlSApply(x, xmlValue))
  sitehistory = data.frame(t(sitehistory), row.names = NULL, stringsAsFactors = F)
  names(sitehistory) = c("WordCount", "Date", "Min", "Max", "Average", "StDev", "Count")
  sitehistory = data.frame(Date = as.Date(as.character(sitehistory$Date)), 
                         WordCount = as.numeric(as.character(sitehistory$WordCount)),
                         Average = as.numeric(as.character(sitehistory$Average)),
                         Min = as.numeric(as.character(sitehistory$Min)),
                         Max = as.numeric(as.character(sitehistory$Max)),
                         StDev = as.numeric(as.character(sitehistory$StDev)),
                         Count = as.numeric(as.character(sitehistory$Count)) )
  return(sitehistory)
}

plotWC_Site <- function(val = "WordCount") {
  sitehistory = getSiteHistory()
  if (val == "WordCount") {
    print(ggplot(data = sitehistory, aes(x = Date, y = WordCount)) + geom_line() + scale_colour_manual(values=cbPalette))
  }
  if (val == "Average") {
    print(ggplot(data = sitehistory, aes(x = Date, y = Average)) + geom_bar(stat = "identity") + scale_fill_manual(values=cbPalette))
  }
  if (val == "Min") {
    print(ggplot(data = sitehistory, aes(x = Date, y = Min)) + geom_bar(stat = "identity") + scale_fill_manual(values=cbPalette))
  }
  if (val == "Max") {
    print(ggplot(data = sitehistory, aes(x = Date, y = Max)) + geom_bar(stat = "identity") + scale_fill_manual(values=cbPalette))
  }
  if (val == "StDev") {
    print(ggplot(data = sitehistory, aes(x = Date, y = StDev)) + geom_bar(stat = "identity") + scale_fill_manual(values=cbPalette))
  }
  if (val == "Count") {
    print(ggplot(data = sitehistory, aes(x = Date, y = Count)) + geom_bar(stat = "identity") + scale_fill_manual(values=cbPalette))
  }
}

printSummary_Site <- function() {
  sitesummary = getSiteSummary()
  if (sitesummary$SiteWordCount <= 0) {
    HTML(print("NaNoWriMo not in session. Check back later!"))
  } else {
    HTML(paste(
      paste("Total Site Word Count: ", sitesummary$SiteWordCount, sep = ""),
      paste("Number of Participants: ", sitesummary$TotalParticipants, sep = ""),
      sep = "<br/>"))
  }
}


#### Individual Word Count History

getUserSummary <- function(usernames) {
  url.path = "http://nanowrimo.org/wordcount_api/wchistory/"
  if (usernames == "") {
    usernames = "nicaless"
  }
  
  usernames = unlist(strsplit(usernames, ","))

  for (i in usernames) {
    user.path = paste(url.path, i, sep = "")
    xmltop.user = getXML(user.path)
    user = xmlValue(xmltop.user[[2]])
    totalwordcount = as.integer(xmlValue(xmltop.user[[3]]))
    winner = xmlValue(xmltop.user[[4]])
    
    if (exists("usersummary")) {
      u = data.frame(User = user, TotalWordCount = totalwordcount, Winner = winner)
      usersummary = rbind(usersummary, u)
    } else {
      usersummary = data.frame(User = user, TotalWordCount = totalwordcount, Winner = winner)
    }
  }
  
  return(usersummary)
}

getUserHistory <- function(username) {
  url.path = "http://nanowrimo.org/wordcount_api/wchistory/"
  if (username == "") {
    username = "nicaless"
  }
  usernames = unlist(strsplit(username, ","))
  
  for (i in usernames) {
    user.path = paste(url.path, i, sep = "")
    xmltop.user = getXML(user.path)
    
    user = xmlValue(xmltop.user[[2]])
    
    userhistory = xmlSApply(xmltop.user[[5]], function(x) xmlSApply(x, xmlValue))
    userhistory = data.frame(t(userhistory), row.names = NULL)
    userhistory$Writer = user
    names(userhistory) = c("WordCount", "Date", "Writer")
    userhistory$Date = as.Date(userhistory$Date)
    userhistory$WordCount = as.numeric(as.character(userhistory$WordCount))
    userhistory$CumulativeWordCount = cumsum(userhistory$WordCount)
    
    if (exists("userhistories")) {
      userhistories = rbind(userhistories, userhistory)
    } else {
      userhistories = userhistory
    }
  }
  return(userhistories)
}

plotSummary_Users <- function(usernames) {
  usersummary = getUserSummary(usernames)
  ggplot(data = usersummary, aes(x = User, y = TotalWordCount, group = Winner, fill = Winner)) + geom_bar(stat = "identity", width = .5) + coord_flip() + scale_fill_hue(l = 45)
  
}

plotHistory_Users <- function(usernames) {
  userhistories = getUserHistory(usernames)
  ggplot(data = userhistories, aes(x = Date, y = CumulativeWordCount, group = Writer, colour = Writer)) + geom_line() + scale_colour_manual(values=cbPalette)
}


#### Regional Word Count Summary

getRegionSummary <- function(regions) {
  url.path = "http://nanowrimo.org/wordcount_api/wcregion/"
  if (regions == "") {
    regions = "usa-california-san-francisco"
  }
  regions = unlist(strsplit(regions, ","))
  
  for (i in regions) {
    region.path = paste(url.path, i, sep = "")
    xmltop.region = getXML(region.path)
    region = xmlValue(xmltop.region[[2]])
    numparticipants = as.integer(xmlValue(xmltop.region[[3]]))
    totalwordcount = as.integer(xmlValue(xmltop.region[[4]]))
    totaldonations = as.numeric(xmlValue(xmltop.region[[10]]))
    numdonors = as.integer(xmlValue(xmltop.region[[11]]))
    
    if (exists("regionsummary")) {
      r = data.frame(Region = region, Participants = numparticipants, WordCount = totalwordcount, Donations = totaldonations, Donors = numdonors)
      regionsummary = rbind(regionsummary, r)
    } else {
      regionsummary = data.frame(Region = region, Participants = numparticipants, WordCount = totalwordcount, Donations = totaldonations, Donors = numdonors)
    }
  }
  
  return(regionsummary)
}

getRegionHistory <- function(regions) {
  url.path = "http://nanowrimo.org/wordcount_api/wcregionhist/"
  if (regions == "") {
    regions = "usa-california-san-francisco"
  }
  
  regions = unlist(strsplit(regions, ","))
  
  for (i in regions) {
    region.path = paste(url.path, i, sep = "")
    xmltop.region = getXML(region.path)
    
    region = xmlValue(xmltop.region[[2]])
    
    regionhistory = xmlSApply(xmltop.region[[5]], function(x) xmlSApply(x, xmlValue))
    regionhistory = data.frame(t(regionhistory), row.names = NULL, stringsAsFactors = F)
    regionhistory$Region = region
    names(regionhistory) = c("Date", "Min", "Max", "WCAverage", "StDev", "Participants", "Donations", "Donors", "Region")
    regionhistory$Date = as.Date(regionhistory$Date)
    regionhistory$Min = as.numeric(regionhistory$Min)
    regionhistory$Max = as.numeric(regionhistory$Max)
    regionhistory$WCAverage = as.numeric(regionhistory$WCAverage)
    regionhistory$StDev = as.numeric(regionhistory$StDev)
    regionhistory$Participants = as.integer(regionhistory$Participants)
    regionhistory$Donations = as.numeric(regionhistory$Donations)
    regionhistory$Donors = as.numeric(regionhistory$Donors)

    
    if (exists("regionhistories")) {
      regionhistories = rbind(regionhistories, regionhistory)
    } else {
      regionhistories = regionhistory
    }
  }
  return(regionhistories)
}

plotHistory_Regions <- function(regions, val = "Participants") {
  regionhistory = getRegionHistory(regions)
  if (val == "Participants") {
    print(ggplot(data = regionhistory, aes(x = Date, y = Participants, group = Region, colour = Region)) + geom_line() + scale_colour_manual(values=cbPalette))
  }
  if (val == "Donations") {
    print(ggplot(data = regionhistory, aes(x = Date, y = Donations, group = Region, fill = Region)) + geom_bar(stat = "identity", width = .5, size = .5) + scale_fill_manual(values=cbPalette))
  }
  if (val == "Donors") {
    print(ggplot(data = regionhistory, aes(x = Date, y = Donors, group = Region, fill = Region)) + geom_bar(stat = "identity", width = .5) + scale_fill_manual(values=cbPalette))
  }
  if (val == "WCAverage") {
    print(ggplot(data = regionhistory, aes(x = Date, y = WCAverage, group = Region, colour = Region)) + geom_line() + scale_colour_manual(values=cbPalette))
  }
}
