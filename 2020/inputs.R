library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

catch <- read.csv('data/catch.csv') %>% rename(year=YEAR, catch=TONS) %>%
  group_by(year) %>%
  summarize(catch=sum(catch), .groups='drop')
write.csv(file='inputs/catch.csv', catch, row.names=FALSE)


## ### Process index (old Carey code to be updated in 2021)
## sum.biom <- aggregate(biom[,-1],by=list(YEAR=biom$YEAR),FUN=sum)
## sum.biom$CV<-sqrt(sum.biom$BIOMVAR)/sum.biom$BIOM #edited line 38, removed sum.biom() 'function'
## sum.biom$RickSE<-sqrt(log(1+sum.biom$CV^2))
## data<-sum.biom
## data$CV<-sqrt(data$BIOMVAR)/data$BIOM
## data$RickSE<-sqrt(log(1+data$CV^2))
## data<-data[order(data$YEAR),]
## data$index<-2
