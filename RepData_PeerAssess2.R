##Loading data into R

setwd("~/DataScienceCoursera/ReproRes/RepData_PeerAssessment2")
fileURL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, destfile="stormdata.csv.bz2")
stormdata <- read.csv(bzfile("stormdata.csv.bz2"))
print(date())

names(stormdata) <- tolower(names(stormdata))

#filter to include only needed fields
stormdata <- select(stormdata, evtype, fatalities, injuries, propdmg, 
                     propdmgexp, cropdmg, cropdmgexp)

#convert propdmg and cropdmg exponents to a # and multiply value
stormdata$propdmgexp <- gsub("K", 1000, stormdata$propdmgexp)
stormdata$propdmgexp <- gsub("M", 1000000, stormdata$propdmgexp)
stormdata$propdmgexp <- gsub("B", 1000000000, stormdata$propdmgexp)

stormdata$propdmgexp <- as.numeric(stormdata$propdmgexp)
stormdata$totpropdmg <- stormdata$propdmg * stormdata$propdmgexp

stormdata$cropdmgexp <- as.numeric(stormdata$cropdmgexp)
stormdata$totcropdmg <- stormdata$cropdmg * stormdata$cropdmgexp

##Exploratory analysis
library(dplyr)
summary <- stormdata %>%
      group_by(evtype) %>%
      summarise(instance = n(),
                sumfatality = sum(fatalities, na.rm=T),
                suminjur = sum(injuries, na.rm=T),
                sumpropdmg = sum(totpropdmg, na.rm=T),
                sumcropdmg = sum(totcropdmg, na.rm=T))

#only take rows that have at least one fatality or injury or cropdmg or propdmg
summary <- filter(summary, sumfatality > 0 | suminjur > 0 | sumpropdmg > 0 | sumcropdmg >0)

#because those with fewer than 100 instances have very few injuries 
#or fatalaties we'll remove them as well.
summary <- filter(summary, sumfatality > 10 & suminjur > 10 |
                   sumpropdmg > 1000 & sumcropdmg > 1000)

#create a class variable to collate related evtypes must be done manually
class <- c("avalanche", "snow", "snow", "cold", "cold", "fog", "drought",
           "dust storm", "dust storm", "heat", "cold", "cold", "heat",
           "flood", "flood", "flood", "flood", "flood", "flood", "flood",
           "fog", "wildfire", "cold", "cold", "cold", "wind", "hail",
           "heat", "heat", "heavy rain", "heavy rain", "flood", "snow",
           "high surf", "high surf", "wind", "wind", "wind", "hurr/trop storm",
           "hurr/trop storm", "hurr/trop storm", "hurr/trop storm", "ice",
           "landslide", "lightning", "wind", "other", "heavy rain", "surf",
           "surf", "flood", "flood", "hail", "surf", "surf", "wind", "tstorm",
           "tstorm", "tornado", "hurr/trop storm", "hurr/trop storm", "tstorm",
           "tstorm", "tsunami", "hurr/trop storm", "heat", "flood", "flood",
           "flood", "wildfire", "wildfire", "wildfire", "wind", "snow",
           "snow", "snow", "snow")

summary$class <- class

#summarize data by class
analysis <- summary %>%
      group_by(class) %>%
      summarise(instance = sum(instance),
                sumfatality = sum(sumfatality, na.rm=T),
                suminjur = sum(suminjur, na.rm=T),
                sumpropdmg = sum(sumpropdmg, na.rm=T),
                sumcropdmg = sum(sumcropdmg, na.rm=T)) %>%
      arrange(desc(sumfatality))