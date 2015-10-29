library(XML)

xml.urlpath = "http://nanowrimo.org/wordcount_api/wchistory/"

xml.url = "http://nanowrimo.org/wordcount_api/wcstats"
xmlfile = xmlTreeParse(xml.url)
xmltop = xmlRoot(xmlfile)

sitewordcount = as.numeric(unname(unlist(xmltop[[1]][[1]]))[2])
numparticipants = as.numeric(unname(unlist(xmltop[[2]][[1]]))[2])

sitehistory = xmlSApply(xmltop[[3]], function(x) xmlSApply(x, xmlValue))
sitehistory = data.frame(t(sitehistory), row.names = NULL)

#plotWC_Site <- function
