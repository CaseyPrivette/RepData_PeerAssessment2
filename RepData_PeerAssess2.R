##Loading data into R

setwd("~/DataScienceCoursera/ReproRes/RepData_PeerAssessment2")
fileURL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, destfile="stormdata.csv.bz2")
stormdata <- read.csv(bzfile("stormdata.csv.bz2"))
print(date())

names(stormdata) <- tolower(names(stormdata))

##Exploratory analysis
library(dlyr)
healththreat <- data.frame()
healththreat <- stormdata %>%
      group_by(evtype) %>%
      summarise(instance = n(),
                avgfatality = mean(fatalities, na.rm=T),
                sumfatality = sum(fatalities, na.rm=T),
                avginjur = mean(injuries, na.rm=T),
                suminjur = sum(injuries, na.rm=T)) %>%
      arrange(desc(avgfatality), desc(avginjur))

ht <- filter(healththreat, instance > 10 & sumfatality > 0 | suminjur > 0)
      