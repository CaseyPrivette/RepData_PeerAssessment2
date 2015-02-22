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

stormdata$cropdmgexp <- gsub("K", 1000, stormdata$cropdmgexp)
stormdata$cropdmgexp <- gsub("M", 1000000, stormdata$cropdmgexp)
stormdata$cropdmgexp <- gsub("B", 1000000000, stormdata$cropdmgexp)

stormdata$cropdmgexp <- as.numeric(stormdata$cropdmgexp)
stormdata$totcropdmg <- stormdata$cropdmg * stormdata$cropdmgexp

##group and summarize by evtype
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
class <- c("avalanche", "snow", "surf", "cold", "cold", "cold", "fog", 
           "drought", "wind", "wind", "wind", "heat", "cold", "cold", 
           "heat", "cold", "flood", "flood", "flood", "flood", "flood", 
           "flood", "flood", "flood", "fog", "wildfire", "cold", "cold", 
           "cold", "cold", "wind", "wind", "wind", "hail", "hail", "hail", 
           "heat", "heat", "heat", "heavy rain", "surf", "heavy rain", 
           "flood", "snow", "flood", "surf", "surf", "wind", "wind", 
           "wind", "cold", "tropicalsystem", "tropicalsystem", 
           "tropicalsystem", "tropicalsystem", "tropicalsystem", 
           "tropicalsystem", "flood", "ice", "landslide", "lightning", 
           "wind", "tstorm", "other", "heavy rain", "surf", "surf", "flood", 
           "flood", "flood", "tstorm", "tstorm", "tstorm", "hail", "snow", 
           "surf", "surf", "wind", "wind", "tstorm", "tstorm", "hail", 
           "tstorm", "tstorm", "hail", "tstorm", "flood", "hail", "tstorm", 
           "tstorm", "tstorm", "tornado", "tornado", "tornado", 
           "tropicalsystem", "tropicalsystem", "tropicalsystem", 
           "tropicalsystem", "tstorm", "hail", "tsunami", "tropicalsystem", 
           "heat", "flood", "flood", "flood", "wildfire", "wildfire", 
           "wildfire", "wildfire", "wind", "wind", "wind", "snow", "wind", 
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
      mutate(totalcasualty = sumfatality + suminjur,
             totaldamage = sumpropdmg + sumcropdmg)

##analysis
casualty <- filter(analysis, totalcasualty > 4641)

require(ggplot2)
health <- ggplot(casualty, aes(class, totalcasualty))
health + geom_bar(stat="identity", fill="steelblue") + 
      labs(title="Top 5 Weather Events by Total Casualties", 
           y="Fatalaties + Injuries", x="Weather Event")

damage <- analysis %>%
      filter(totaldamage > 15990000000) %>%
      mutate(damageinbillions = totaldamage / 1000000000)

econ <- ggplot(damage, aes(class, damageinbillions))
econ + geom_bar(stat="identity", fill="purple") +
      labs(title="Top 5 Weather Events in Dollars of Damage", 
           x="Weather Event", 
           y="Total Property & Crop Damage (in billions of $)")
