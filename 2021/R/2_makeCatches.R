## code to extrapolate catches for future years
## based on C. Monnohan's file from the 2020 update entitled report.R

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
require(here)

GOAt <- read.csv(here("data","2021-10-28-catch.csv")) %>% 
  group_by(YEAR) %>%
  summarise(GOA_total = sum(TONS)) %>%
  select(YEAR, GOA_total)

## Inputs for Projections/spp_catch.dat ----

#* catches 2017-2020 ----
round(tail(GOAt)) 

#* catches 2021 ----
## note this value will change depending when you run the code given the call to this_week
## (generally it will increase the later in the year you run it)
this_year= 2021
files <- list.files(here('data','weekly_catches'), full.names=TRUE)
test <- lapply(1:length(files), function(i){
  skip <- grep('ACCOUNT.NAME', readLines(files[i]))-1
  data.frame(read.table(files[i], skip=skip, header=TRUE, sep=',',
                        stringsAsFactors=FALSE))
})
weekly_catches <- do.call(rbind, test)
names(weekly_catches) <- c('species', 'date', 'catch')
weekly_catches <- weekly_catches %>%
  ## The species is split by area still
  filter(grepl("Flathead", x=species)) %>%
  mutate(date=lubridate::mdy(date), week=week(date),  year=year(date))
catch_this_year <- weekly_catches %>% 
  filter(year==this_year) %>%
  pull(catch) %>% sum
## years
catch_to_add <- weekly_catches %>% filter(year>=this_year-5 & week > week(today())) %>%
  group_by(year) %>% summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean
message("Precited ", this_year, " catch= ", round(catch_this_year + catch_to_add,0))


## for table - catch by area in 2021
tmp0 <- catch %>% 
  # filter(year==this_year) %>%
  filter(YEAR > 2015) 

tmp0$ZONE <- ifelse(tmp0$ZONE =="EY"|tmp0$ZONE == "SE" |tmp0$ZONE == "WY",
                      'EGOA',tmp0$ZONE)

tmp0 %>% group_by(YEAR,ZONE) %>%
  summarise(sc=sum(TONS)) %>%
  tidyr::pivot_wider(., id_cols = YEAR, names_from = ZONE, values_from = sc) %>%
  mutate(total = CG+EGOA+WG) %>%
  select(YEAR,total, WG, CG, EGOA)



#* catches 2022/2023 ----
##  use last 5 years' real data average
GOAt %>% 
  filter(YEAR < 2021 & YEAR > 2015) %>% 
  summarise(mean(GOA_total)) ## 2251, quite similar to before,



