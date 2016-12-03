source("nanowrimo.R")
library(XML)
library(xts)
library(forecast)

#bars percentage of total top 5 wc,donations,wcavg etc. over time
# more interesting scatters????
#final top 10 tables
# fixing tracker app to use static data

# Read in Data
# Region Summaries
summary15 = read.csv("NaNoRegionSummary_11_15_2015", stringsAsFactors = F)
names(summary15)[1] = "Date"
summary15[1] = as.Date("2015-11-15")

summary20 = read.csv("NaNoRegionSummary_11_20_2015", stringsAsFactors = F)
names(summary20)[1] = "Date"
summary20[1] = as.Date("2015-11-20")

summary25 = read.csv("NaNoRegionSummary_11_25_2015", stringsAsFactors = F)
names(summary25)[1] = "Date"
summary25[1] = as.Date("2015-11-25")

summary30 = read.csv("NaNoRegionSummary_12_1_2015", stringsAsFactors = F)
names(summary30)[1] = "Date"
summary30[1] = as.Date("2015-11-30")

summary = rbind(summary15, summary20, summary25, summary30)
summary$WordCountAverage = summary$WordCount/summary$Participants
summary$DonationParticipantRatio = summary$Donations/summary$Participants
summary$DonorParticipantRatio = summary$Donors/summary$Participants
summary$DonationWordCountRatio = summary$Donors/summary$WordCount
summary = data.frame(WriMo = "November 2015", summary)

#Site Summary
sitesummary = getSiteSummary()
sitesummary = data.frame(WriMo = "November 2015", sitesummary)
sitehistory = getSiteHistory()
sitehistory = data.frame(WriMo = "November 2015", sitehistory)

# Region History 
regionhistory = read.csv("NaNoRegionHistories_12_1_2015", stringsAsFactors = F)
names(regionhistory)[1] = "WriMo"
regionhistory[1] = "November 2015"
regionhistory$DonationParticipantRatio = regionhistory$Donations/regionhistory$Participants
regionhistory$DonorParticipantRatio = regionhistory$Donors/regionhistory$Participants
regionhistory$DonationWordCountRatio = regionhistory$Donors/regionhistory$WordCount

#Write data
write.csv(summary, file = "regionsummary_11_2015.csv")
write.csv(sitesummary, file = "sitesummary_11_2015.csv")
write.csv(sitehistory, file = "sitehistory_11_2015.csv")
write.csv(regionhistory, file = "regionhistory_11_2015.csv")





