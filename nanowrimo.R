library(ggplot2)
library(XML)
library(forecast)
library(xts)

#### Color Palette (might kill this palette later)
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00")

#### Site Word Count History

getXML <- function(site) {
  xml.url.site = site
  xmlfile.site = xmlParse(xml.url.site)
  xmltop.site = xmlRoot(xmlfile.site)
  return(xmltop.site)
}

xmltop.site <<- getXML("http://nanowrimo.org/wordcount_api/wcstats")
xmltop.sitesummary <<- getXML("http://nanowrimo.org/wordcount_api/wcstatssummary")

getSiteSummary <- function() {
  #sitewordcount = as.numeric(unname(unlist(xmltop.site[[1]][[1]]))[2])
  #numparticipants = as.numeric(unname(unlist(xmltop.site[[2]][[1]]))[2])
  sitewordcount = as.numeric(xmlValue(xmltop.sitesummary[[1]]))
  numparticipants = as.numeric(xmlValue(xmltop.sitesummary[[6]]))
  min = as.numeric(xmlValue(xmltop.sitesummary[[2]]))
  max = as.numeric(xmlValue(xmltop.sitesummary[[3]]))
  avg = as.numeric(xmlValue(xmltop.sitesummary[[4]]))
  stdev = as.numeric(xmlValue(xmltop.sitesummary[[5]]))
  
  sitesummary = data.frame(SiteWordCount = sitewordcount, TotalParticipants = numparticipants,
                           MinWordCount = min, MaxWordCount = max, WordCountAverage = avg,
                           WordCountStDev = stdev)
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


### Past WriMo Data
past_region_history_data <<- read.csv("data/regionhistory_11_2015.csv", stringsAsFactors = F)
past_region_history_data$Date = as.Date(past_region_history_data$Date)

past_site_summary <<- read.csv("data/sitesummary_11_2015.csv", stringsAsFactors = F)

past_region_summary <<- read.csv("data/regionsummary_11_2015.csv", stringsAsFactors = F)


printSiteTable <- function(wrimo){
  report = subset(past_site_summary, WriMo %in% wrimo)
  print(report)
}

printWinnerTable <- function(region_names, wrimo){
  rawData = subset(past_region_summary, WriMo %in% wrimo)
  WordCount = dcast(rawData, Region ~., value.var = "WordCount", sum)
  WordCount = WordCount[order(WordCount[, 2], decreasing = T), ][1, ]
  
  Donations = dcast(rawData, Region ~., value.var = "Donations", sum)[1,]
  Donations = Donations[order(Donations[, 2], decreasing = T), ][1, ]
  
  Donors = dcast(rawData, Region ~., value.var = "Donors", sum)[1,]
  Donors = Donors[order(Donors[, 2], decreasing = T), ][1, ]
  
  WordCountAverage = dcast(rawData, Region ~., value.var = "WordCountAverage", sum)[1,]
  WordCountAverage = WordCountAverage[order(WordCountAverage[, 2], decreasing = T), ][1, ]
  
  DonationParticipantRatio = dcast(rawData, Region ~., value.var = "DonationParticipantRatio", sum)[1,]
  DonationParticipantRatio = DonationParticipantRatio[order(DonationParticipantRatio[, 2], decreasing = T), ][1, ]
  
  DonorParticipantRatio = dcast(rawData, Region ~., value.var = "DonorParticipantRatio", sum)[1,]
  DonorParticipantRatio = DonorParticipantRatio[order(DonorParticipantRatio[, 2], decreasing = T), ][1, ]
  
  DonationWordCountRatio = dcast(rawData, Region ~., value.var = "DonationWordCountRatio", sum)[1,]
  DonationWordCountRatio = DonationWordCountRatio[order(DonationWordCountRatio[, 2], decreasing = T), ][1, ]
  
  Categories = c("WordCount","Donations", "Donors", "WordCountAverage", "DonationParticipantRatio", 
                 "DonorParticipantRatio","DonationWordCountRatio")
  TopRegion = c(WordCount[1,1], Donations[1,1], Donors[1,1], WordCountAverage[1,1],
                DonationParticipantRatio[1,1], DonorParticipantRatio[1,1],DonationWordCountRatio[1,1])
  Value = c(WordCount[1,2], Donations[1,2], Donors[1,2], WordCountAverage[1,2],
            DonationParticipantRatio[1,2], DonorParticipantRatio[1,2],DonationWordCountRatio[1,2])
  report = cbind(Categories, TopRegion, Value)
  print(report)
}

plotRegionHistoryCorr <- function(region_names, wrimo, x_val, y_val, type) {
  rawData = subset(past_region_history_data, WriMo == wrimo)
  rawData = subset(rawData, Region %in% region_names, select = c(x_val, y_val, "Region"))
  report = data.frame(X_Val = rawData[, x_val], Y_Val = rawData[, y_val], Region = as.character(rawData[, "Region"]))
  report$Region = as.character(report$Region)
  if (type == "bar") {
    print(ggplot(data = report, aes(x = X_Val, y = Y_Val, group = Region, fill = Region)) + geom_bar(stat = "identity") + labs(x = x_val, y = y_val) + theme(legend.position="bottom") + guides(col=guide_legend(ncol = 3)))
  }
  if (type == "line") {
    print(ggplot(data = report, aes(x = X_Val, y = Y_Val, group = Region, colour = Region)) + geom_line() + labs(x = x_val, y = y_val) + theme(legend.position="bottom") + guides(col=guide_legend(ncol = 3))) 
  }
  if (type == "scatter") {
    print(ggplot(data = report, aes(x = X_Val, y = Y_Val, group = Region, colour = Region)) + geom_point() + labs(x = x_val, y = y_val) + geom_smooth(method = lm) + theme(legend.position="bottom") + guides(col=guide_legend(ncol = 3)))
  }
}

forecastCompletion <- function(id) {
  report = getUserHistory(id[1])
  my_df = data.frame(x = report$Date, y = cumsum(report$WordCount))
  plot_label = paste("Forecast Wordcount for ", id[1], sep = "")
 
  ts = xts(my_df$y, my_df$x)
  fit = ets(ts)
  plot(forecast(fit), 
       main = plot_label, 
       xlab = "Days Since NaNoWriMo Start", ylab = "WordCount")
}

