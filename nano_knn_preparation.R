#Gathering data for KNN
# 150 NaNoWriMo users, their wc average, and the number of times they made a non-zero word count entry

source("nanowrimo.R")
library(reshape2)

users = c("rachel-b-moore", "abookishbabe", "alexabexis", "AllYellowFlowers", "animalrza",
           "Cafenuit", "Carmelon", "carozy", "chrisk0", "ComeOnAndWriteThisNovel",
          "cretsi", "dazart", "dgiuliani", "FaerieRogue", "fallingleaf", 
          "FeliciaFredlund", "grenouille", "hbt003", "imtrisprior", "Jamesalicious",
          "jen-gambale", "K-R-Smith", "Kelvarus", "khmore", "kp17",
          "kristenrudd", "lavenderblue", "Liberate", "liteindigold", "Lunar-Daddy",
          "MamaHen40", "MermaidMaddie", "motherless-child", "nelle81", "nemodos",
          "nicole1967", "Rebecca-Stern", "rienkarrot", "s-writes56", "schappy",
          "Shaun-Parker", "sninkychan", "SquirrelScribe", "UltraCelestial", "unagicat",
          "Uriel238", "ValkyrieKS", "Vehka", "honeywell", "Allioup", "amelia-marshall",
          "AngerAndAgony", "Aralonia", "bexdragon", "Bustella", "Crodentia",
          "DarylDarko", "Derek-Wallace", "dkdailey", "ebonstorm", "EllenMulholland",
          "escabatum_rip3", "ethibeaux", "fiegellan", "fireun", "G-M-Luna", "Grant-Faulkner",
          "greenphoenixrain", "hsparks", "Ixy-Pixy", "jjfid", "jodimt", "jonbritton",
          "J_Hill", "laceywilson", "Laconic-Lad", "lassen86", "lenichi", "LetterpressLibrarian",
          "lil-bone", "lokelani86", "Mancha", "MKent1", "MusicalMe", "mwalker", "naseem-sage",
          "NLettis", "Nytshaed", "poise", "QuinnWhittaker", "RagingFem", "revdolphin",
          "Robertjm", "Sophia-Salazar", "Space-Jack", "sushimustwrite", "The-Big-Bad-Firewing",
          "wildflowerlens", "wittycassiehere", "wordweaver", "WSpinale", "Alexandris",
          "ambrella", "DinosaurDooDoo", "gakernes", "irlangel", "jenniferkate", "Jessbear84",
          "JWhiteFang", "KDSUNFLOWERS", "kicksandgiggles", "krkausen", "lynxzpanther",
          "melissacroce", "MsHannaBSmith", "New-Apollo", "pbtoejam05", "sniccolls",
          "StardustStorm", "Torturedwriting", "erin_havel", "allireapandsow", "amberkell",
          "Ashley-Daniele", "astridsdream", "authorperson", "Blushingwriter", "Boupie",
          "CairennRhys", "Caisidorhpa", "CAPav", "darkotter-s-kosher-bacon", 
          "dlindle", "ElizaWyatt", "Frozen_in_Tyme", "fyca", "Hype","ilonaho",
          "JaxFost", "jelliott93", "Jonah-ER-Loeb", "Judydawn", "juliawester", 
          "kalekim", "KinoFou", "kittypan", "KlaraKim", "KnottedCord21", 
          "LadyJustice", "lunespark", "MDBrannan", "Miss-Apple", "morvegan",
          "MrLasers", "NW_Rose", "otakubookworm123")

user_history = getUserHistory(users)

#Find number of times a user updated their word count, aka any row that isn't 0.  

user_history$Submitted = 0
user_history$Submitted[user_history$WordCount != 0] = 1

# First Half of November

first_half = subset(user_history, Date <= as.Date("2015-11-15"))

submissions1 = dcast(first_half, Writer ~., value.var = "Submitted", sum)
wc_avg1 = dcast(first_half, Writer ~., value.var = "WordCount", mean)

data_first_half = data.frame(Writer = submissions1[, 1], 
                             WCSubmissions = submissions1[, 2],
                             WCAvg = wc_avg1[, 2])

# Second Half of November

second_half = subset(user_history, Date > as.Date("2015-11-15"))

submissions2 = dcast(second_half, Writer ~., value.var = "Submitted", sum)
wc_avg2 = dcast(second_half, Writer ~., value.var = "WordCount", mean)

data_second_half = data.frame(Writer = submissions2[, 1], 
                             WCSubmissions = submissions2[, 2],
                             WCAvg = wc_avg2[, 2])


# Merge with Winner or Not
user_summary = getUserSummary(users)
names(user_summary) = c("Writer", "TotalWordCount", "Winner")
user_summary$Writer = as.character(user_summary$Writer)
user_summary$Winner = as.character(user_summary$Winner)

data_first_half = merge(data_first_half, user_summary, by = "Writer")
data_first_half = data_first_half[-4]
data_first_half$Writer = as.character(data_first_half$Writer)

data_second_half = merge(data_second_half, user_summary, by = "Writer")
data_second_half = data_second_half[-4]
data_second_half$Writer = as.character(data_second_half$Writer)

#Write to csv.

write.csv(data_first_half, file = "NaNo_Writers_first_half.csv")
write.csv(data_second_half, file = "NaNo_Writers_second_half.csv")

