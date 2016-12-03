source("nanowrimo.R")
library(XML)
library(xts)
library(forecast)

# cities chosen by http://flavorwire.com/416836/20-great-american-cities-for-writers-that-arent-new-york/
# exceptions: Saint Paul, MN incorporated in Twin Cities
# excluded great barrington, MA
# excluded Portland, ME
# excluded Jersey City, NJ
# excluded Portsmouth, NH
# excluded Cambridge, MA, i'm not sure why
# also chosen by http://travel.nationalgeographic.com/travel/top-10/literary-cities/
#http://jameystegmaier.com/2009/04/the-five-best-cities-in-the-world-for-writers/
# 38 delibrately chosen, 12 selected randomly

my_regions = c("usa-california-san-francisco", 
               "usa-california-los-angeles", 
               "usa-massachusetts-boston",
               "usa-new-york-new-york-city",
               "usa-illinois-chicago",
               "usa-texas-austin",
               "usa-south-carolina-charleston",
               "usa-washington-bellingham",
               "usa-north-carolina-asheville",
               "usa-district-of-columbia",
               "usa-minnesota-twin-cities",
               "usa-washington-seattle",
               "usa-louisiana-new-orleans",
               "usa-florida-miami",
               "usa-michigan-ann-arbor",
               "usa-georgia-savannah",
               "usa-pennsylvania-pittsburgh",
               "usa-oregon-portland",
               "usa-iowa-iowa-city",
               #"usa-massachusetts-cambridge",
               "usa-massachusetts-elsewhere",
               "europe-scotland-edinburgh",
               "europe-ireland-dublin",
               "europe-england-london",
               "europe-france-paris",
               "australia-melbourne",
               "central-south-america-chile",
               "usa-missouri-st-louis",
               "africa-egypt",
               "asia-china",
               "asia-japan",
               "asia-india",
               "russia-moscow",
               "asia-philippines",
               "asia-south-korea-seoul",
               "canada-ontario-ottawa",
               "canada-quebec-montreal",
               "canada-ontario-toronto",
               "central-south-america-brazil",
               "usa-utah-salt-lake-county",
               "usa-california-elsewhere",
               "usa-california-lompoc",
               "europe-france-elsewhere",
               "europe-switzerland",
               "usa-virginia-northern",
               "usa-new-york-plattsburgh",
               "usa-florida-jacksonville",
               "usa-louisiana-elsewhere",
               "usa-illinois-naperville",
               "australia-sydney",
               "usa-texas-amarillo")

regionsummary = getRegionSummary(my_regions)
regionhistories = getRegionHistory(my_regions)

write.csv(regionsummary, file = "NaNoRegionSummary_12_1_2015")
write.csv(regionhistories, file = "NaNoRegionHistories_12_1_2015")

png("participants_v_wordcount")
ggplot(data = regionsummary, aes(x = Participants, y = WordCount, group = 1, colour = Region)) + 
  geom_point() + 
  labs(x = "Participants", y="WordCount") + 
  geom_smooth(method = lm)
dev.off()

png("participants_v_donations")
ggplot(data = regionsummary, aes(x = Participants, y = Donations, group = 1, colour = Region)) + 
  geom_point() + 
  labs(x = "Participants", y="Donations") + 
  geom_smooth(method = lm)
dev.off()

png("participants_v_donors")
ggplot(data = regionsummary, aes(x = Participants, y = Donors, group = 1, colour = Region)) + 
  geom_point() + 
  labs(x = "Participants", y="Donors") + 
  geom_smooth(method = lm)
dev.off()

png("wordcount_v_donations")
ggplot(data = regionsummary, aes(x = WordCount, y = Donations, group = 1, colour = Region)) + 
  geom_point() + 
  labs(x = "WordCount", y="Donations") + 
  geom_smooth(method = lm)
dev.off()

png("wordcount_v_donors")
ggplot(data = regionsummary, aes(x = WordCount, y = Donors, group = 1, colour = Region)) + 
  geom_point() + 
  labs(x = "WordCount", y="Donors") + 
  geom_smooth(method = lm)
dev.off()

png("donations_v_donors")
ggplot(data = regionsummary, aes(x = Donations, y = Donors, group = 1, colour = Region)) + 
  geom_point() + 
  labs(x = "Donations", y="Donors") + 
  geom_smooth(method = lm)
dev.off()

## All positive linear correlations as expected.
## looked at as avg wordcount submissions over time
## only looking at "historically top writer" us cities
## also seattle washington was crazy... excluding them wcaverage is about the same for all these cities
## that makes sense, site average is about 2000 as well, exception being the first few days when WriMo's 
## are being most productive

top_us_regions = c("usa-california-san-francisco", 
                   "usa-california-los-angeles", 
                   "usa-massachusetts-boston",
                   "usa-new-york-new-york-city",
                   "usa-illinois-chicago",
                   "usa-texas-austin",
                   "usa-south-carolina-charleston",
                   "usa-washington-bellingham",
                   "usa-north-carolina-asheville",
                   "usa-district-of-columbia",
                   "usa-minnesota-twin-cities",
                   #"usa-washington-seattle",
                   "usa-louisiana-new-orleans",
                   "usa-florida-miami",
                   "usa-michigan-ann-arbor",
                   "usa-georgia-savannah",
                   "usa-pennsylvania-pittsburgh",
                   "usa-oregon-portland",
                   "usa-iowa-iowa-city")

usregionhistories = getRegionHistory(top_us_regions)

ggplot(data = usregionhistories, aes(x = Date, y = WCAverage, group = Region, fill = Region)) +
  geom_bar(stat="identity", position="dodge")

## Forecast when the "average" person will finish their novel in each region
# dcast each region
# cumsum on WCAverage
# forecast

# just sfregion

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

sfregionhistory$CumulAvg = cumsum(sfregionhistory$WCAverage)
my_df = data.frame(x = sfregionhistory$Date, y = sfregionhistory$CumulAvg)
ggplot(data = my_df, aes(x = x, y = y)) +
  geom_pointpre() + geom_smooth(method = lm) + geom_text(x = 25, y = 25000, label = lm_eqn(my_df), parse = TRUE)

## extend it to forecast when an individual will complete their novel



