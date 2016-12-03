library(XML)
library(httr)
library(forecast)
library(xts)
library(wordcloud2)
library(lubridate)


#### Main function to parse XML files

getXML <- function(site) {
  xml.url.site = site
  xmlfile.site = xmlParse(xml.url.site)
  xmltop.site = xmlRoot(xmlfile.site)
  return(xmltop.site)
}

##### Site, Region, and User level parsing to Data Frames

#### Site Word Count History

xmltop.site <<- getXML("http://nanowrimo.org/wordcount_api/wcstats")
xmltop.sitesummary <<- getXML("http://nanowrimo.org/wordcount_api/wcstatssummary")

getSiteSummary <- function() {
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

#### Regional Word Count History

#need to implement pre-parsing region names before feeding into getRegionX functions 
#NEED ERROR CATCHING FOR INVALID REGION NAMES FIRST
getRegionSummary <- function(regions = "usa-california-san-francisco") {
  url.path = "http://nanowrimo.org/wordcount_api/wcregion/"
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

getRegionHistory <- function(regions = "usa-california-san-francisco") {
  url.path = "http://nanowrimo.org/wordcount_api/wcregionhist/"
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

#### Individual Word Count History

getUserSummary <- function(usernames = "nicaless") {
  url.path = "http://nanowrimo.org/wordcount_api/wchistory/"
  usernames = unlist(strsplit(usernames, ","))
  
  for (i in usernames) {
    user.path = paste(url.path, i, sep = "")
    xmltop.user = getXML(user.path)
    
    if(xmlName(xmltop.user[[1]]) == "error"){
      user = xmlName(xmltop.user[[1]])
      totalwordcount = xmlValue(xmltop.user[[1]])
      winner = NA
    } else {
      user = xmlValue(xmltop.user[[2]])
      totalwordcount = as.integer(xmlValue(xmltop.user[[3]]))
      winner = xmlValue(xmltop.user[[4]])
    }
    
    if (exists("usersummary")) {
      u = data.frame(User = user, TotalWordCount = totalwordcount, Winner = winner)
      usersummary = rbind(usersummary, u)
    } else {
      usersummary = data.frame(User = user, TotalWordCount = totalwordcount, Winner = winner)
    }
  }
  
  return(usersummary)
}

getUserHistory <- function(username = "nicaless") {
  url.path = "http://nanowrimo.org/wordcount_api/wchistory/"
  usernames = unlist(strsplit(username, ","))
  
  for (i in usernames) {
    user.path = paste(url.path, i, sep = "")
    xmltop.user = getXML(user.path)
    
    if(xmlName(xmltop.user[[1]]) == "error"){
      wordcount = NA
      date = xmlName(xmltop.user[[1]])
      writer = xmlValue(xmltop.user[[1]])
      cumul = NA
      userhistory = data.frame(WordCount = wordcount, Date = date, Writer = writer, CumulativeWordCount = cumul)
    } else {
      user = xmlValue(xmltop.user[[2]])
      
      userhistory = xmlSApply(xmltop.user[[5]], function(x) xmlSApply(x, xmlValue))
      userhistory = data.frame(t(userhistory), row.names = NULL)
      userhistory$Writer = user
      names(userhistory) = c("WordCount", "Date", "Writer")
      userhistory$Date = as.Date(userhistory$Date)
      userhistory$WordCount = as.numeric(as.character(userhistory$WordCount))
      userhistory$CumulativeWordCount = cumsum(userhistory$WordCount)
    }
    
    if (exists("userhistories")) {
      userhistories = rbind(userhistories, userhistory)
    } else {
      userhistories = userhistory
    }
  }
  return(userhistories)
}

### Word Count Suggestions 
# Not everyone can write 50,000 words in 30 days
# Given your own word count goal, current word count(if applicable), and days left in contest (if applicable), get Words per Day/Writing Session you need to write to meet your goal 
# Give your current word count(if applicable), days left in contest (if applicable), writing frequency (average per day, if applicable), and rating (can/will you write more/less/the same), get an optimized word count goal 
# Words per Writing Session Goal - It's said professional writers can write 1000 wph.  Optimize a single writing session given WC goal, hourly/daily/weekly commitment or past writing frequency, steady or graduated option

# V2 Option to uplaod CSV of writing progress

getDayRate <- function(user = "", wc = 0, goal = 50000, days_left = NULL) {
  # Another func can handle if wc and days_left can be taken from NaNoWriMo
  
  ### Move these calculation to front end later?
  
  # User NaNoWriMo wc and current NaNo contest if username given
  if(user != "") {
    nanoWC = getUserSummary(user)$TotalWordCount
    # if input wc is still the default 0 value and nanoWC NOT NA
    if(wc == 0 & !(is.na(nanoWC))) {
      wc = nanoWC
    }
  }
  if(!is.null(days_left)){
    if(month(Sys.Date()) == 11) {
      days_left = (30 - day(Sys.Date())) + 1 #+1 to include current day
    } else {
      # default is 30 if NaNo contest and days_left not given
      days_left = 30
    }
  }
  
  words_per_day = (goal - wc ) / days_left
  return(words_per_day)
}

#self_rating logic KIND OF COMPLETELY ARBITRATY
# if less agressive -> -75% of writing frequency or 500 words per day if no writing frequency
# if more aggressive -> +50% of writing frequency or 2000 words per day if no writing frequency
# if same intensity/medium intensity -> 1000 words per day
getOptimizedGoal <- function(user = "", wc = 0, days_left = NULL, writing_frequency = 1, self_rating = "Same/Medium Intensity") {
  # Another func can handle if writing frequency can be taken from NaNoWriMo and given as a vector
  #if writing frequency a vector (words written per day) 
  #forecast forward by days_left +- the self_rating (forecast next 3 sessions +- 500? more words to result, then forecast again)
  #else
  ## if wc > 0
  ## calculate a writing frequency vector from wc and writing_frequency
  ## forecast forward by days_left +- the self_rating (forecast next 3 sessions +- 500? more words to result, then forecast again)
  ## else 
  # days_left * X (x determined by self_rating)
  
  ### Move these calculation to front end later?
  
  if(!is.null(days_left)){
    if(month(Sys.Date()) == 11) {
      days_left = (30 - day(Sys.Date())) + 1 #+1 to include current day
    } else {
      # default is 30 if no current NaNo contest and days_left not given
      days_left = 30
    }
  }
  
  # User NaNoWriMo wc and current NaNo contest if username given
  if(user != "") {
    nanoFreq = getUserHistory(user)[, c("WordCount", "Date")]
    # populate empty days in nanoFreq from days_left
    # goalChart created by a forecast?
    
    # if nanoFreq has no history (aka WordCount is na), calculate a writing frequency vector from wc and writing_frequency
    if(is.na(nanoWC)) {
  
      # if no word count at all (hasn't started project yet)
      if(wc == 0) {
        goalChart = data.frame(Day = 1:days_left)
        if(self_rating == "Same/Medium Intensity"){
          goalChart$AvgWordsPerDay = 1000 * writing_frequency
        } else if(self_rating = "More Aggressive") {
          goalChart$AvgWordsPerDay = 2000 * writing_frequency
        } else if(self_rating = "Less Aggressive") {
          goalChart$AvgWordsPerDay = 500 * writing_frequency
        }
        goalChart$CumulativeWords = cumsum(goalChart$AvgWordsPerDay)
      # if has started project
      } else {
        avgW = wc / writing_frequency
        goalChart = data.frame(Day = 0:days_left)
        goalChart$AvgWordsPerDay = 0
        if(self_rating == "Same/Medium Intensity"){
          goalChart$AvgWordsPerDay[2:days_left] = avgW
        } else if(self_rating = "More Aggressive") {
          goalChart$AvgWordsPerDay[2:days_left] = avgW * 1.5
        } else if(self_rating = "Less Aggressive") {
          goalChart$AvgWordsPerDay[2:days_left] = avgW * .75
        }
        goalChart$CumulativeWords = cumsum(goalChart$AvgWordsPerDay) + wc
        goalChartg$AvgWordsPerDay[1] = avgW

      }
    
    }
    # if no username given but has started project
    # see code above
  }

  # return goalChart  
}

#getSessionRate <- 



### Submit Word Count

submitCount <- function(uhash, username, wc) {
  PUT("http://nanowrimo.org/api/wordcount",
      body = list(hash = uhash, name = username, wordcount = wc))
}