library(dplyr)
library(tidyr)
library(ggplot2)
this_year <- 2020


### Catches by area. Need to be very careful to exclude the
### confidential ones and just use total. This is done in the
### report.xlsx file.
catch <- read.csv('data/catch.csv') %>% select(year=YEAR, gear=GEAR1, zone=ZONE, catch=TONS)
catch <- catch %>% mutate(zone=case_when(zone=='CG'~'Central Gulf',
                                    zone=='WG'~"Western Gulf",
                                    zone %in% c('SE', 'EY','WY')~'Eastern Gulf'))
stopifnot(all(!is.na(catch$zone)))
catch <- catch %>% group_by(year, zone) %>%
  summarize(catch=sum(catch), .groups='drop') %>%
  pivot_wider(names_from='zone', values_from='catch', values_fill=0)
write.csv(file='report/catch.csv', catch, row.names=FALSE)


## Table 8.2 survey
index_total <- read.csv('data/index.csv') %>%
  transmute(year=YEAR, Bio_total=BIOM, CV_total=sqrt(BIOMVAR)/BIOM)
index_area <- read.csv('data/index_by_area.csv') %>%
  mutate(AREA=case_when(AREA=='WESTERN GOA'~"western",
                   AREA=='EASTERN GOA'~"eastern",
                   AREA=='CENTRAL GOA'~"central",
                   TRUE~NA_character_))
stopifnot(!any(is.na(index_area$AREA)))
index_area <- index_area %>% group_by(AREA) %>%
  transmute(year=YEAR, Bio=BIOM, CV=sqrt(BIOMVAR)/BIOM) %>%
  pivot_wider(names_from=AREA, values_from=c(Bio, CV),
              values_fill=0)
stopifnot(all(index_total$year==index_area$year))
index_table <- cbind(index_total, index_area[,-1]) %>%
  select(year,Bio_total, CV_total,
         Bio_western, CV_western,
         Bio_central, CV_central,
         Bio_eastern, CV_eastern)
write.csv(index_table, 'report/index_area.csv', row.names=FALSE)


### --------------------------------------------------
## For projection model need to predict total catches in this
## year. Use weekly catches from from previous years to get
## proportion of catch by week to estimate terminal year catch.
##
files <- list.files('data/weekly_catches/', full.names=TRUE)
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
  mutate(date=mdy(date), week=week(date),  year=year(date))
## ## Explore cumulative catches by week for previous years after
## ## summing across zones.
## cumulative_catches <- weekly_catches %>%
##   filter(year!=this_year) %>% group_by(year, week) %>%
##   summarize(catch=sum(catch), .groups='drop') %>% # sum zones
##   arrange(year, week) %>% group_by(year) %>%
##   mutate(cumulative_catch=cumsum(catch),
##          cumulative_proportion=cumulative_catch/sum(catch)) %>% ungroup()
## ggplot(cumulative_catches, aes(week, cumulative_proportion,
##                            color=factor(year))) + geom_line() +
##   labs(title='Cumulative proportion of catch by week',
##        x='Week #', y='Proportion', color='year') +
##   geom_vline(xintercept=week(today()))
## avg_proportion <- cumulative_catches %>%
##   filter(week==week(today())) %>%
##   pull(cumulative_proportion) %>% mean
## message('Average catch proportion for this week= ',
##         100*round(avg_proportion,2), '%')
catch_this_year <- weekly_catches %>% filter(year==this_year) %>%
  pull(catch) %>% sum
## Get average catch between now and end of year for previous 5
## years
catch_to_add <- weekly_catches %>% filter(year>=this_year-5 & week > week(today())) %>%
  group_by(year) %>% summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean
message("Precited ", this_year, " catch= ", round(catch_this_year + catch_to_add,0))
## message('Predicted ',this_year, ' catch= ', round(catch_this_year/avg_proportion,0))
##
## The averages for this_year+1 and +2 are calculated in the
## report spreadsheet under catch

### --------------------------------------------------
### The projection model results
## ## Use R to process output into easy file to create the harvest
## ## table in report.xlsx.
rec_table1 <-
  read.table('projection/Projections/percentdb.out') %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year+1:2) & scenario==1 &
         metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  pivot_wider(names_from=year, values_from=value)
rec_table2 <-
  read.table('projection/Projections/alt2_proj.out', header=TRUE) %>%
  filter(Year %in% (this_year+1:2)) %>%
  pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table <- bind_rows(rec_table1, rec_table2)
write.csv(rec_table, 'report/rec_table.csv', row.names=FALSE)

## ### Jim says to use the means from the bigfile. But I don't
## think this works it's missing a bunch of stuff.
## bigout <- read.table('projection/2019_Projections/bigfile.out', header=TRUE) %>%
##   filter(Alternative==1 & Yr %in% (this_year+1:2)) %>%
##   select(-Spp, -Alternative) %>% group_by(Yr) %>%
##   summarize_all(mean) %>% pivot_longer(c(-Yr), names_to='metric') %>% pivot_wider(names_from=Yr)
## ## It's missing B0/B40/B35 so get that from this file. I think if
## ## if I update proj this will not be necessary. Try that next year
## B0 <- strsplit(readLines('projection/2019_Projections/percentiles.out',
##   n=3)[3], ' ')[[1]][1:3] %>% as.numeric()
## rec_table <- rbind(bigout, data.frame(metric=c('SB0', 'SB40', 'SB35'),
##                                    '2020'=B0, '2021'=B0, check.names=FALSE))

## Projection plots
pdt <- data.frame(read.table("projection/Projections/bigfile.out", header=TRUE))
pdt.long <- pivot_longer(pdt, cols=c(-Alternative, -Spp, -Yr), names_to='metric') %>%
  mutate(Alternative=factor(Alternative)) %>% group_by(Yr, Alternative, metric) %>%
  summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')
g <- ggplot(pdt.long, aes(Yr,  med, ymin=lwr, ymax=upr, fill=Alternative, color=Alternative)) +
  facet_wrap('metric', scales='free_y') + ylim(0,NA) +
  geom_ribbon(alpha=.4) + theme_bw() +
  labs(x='Year', y='Estimated 80% CI')


### --------------------------------------------------
### Area apportionment
## Run the RE model for each area-specific survey to get this
## year's estimates and use that to get proportions. I didn't
## actually run this in 2020 becuase there was no survey. For
## 2021 need to rework this chunk. CCM -10/2020
index_by_area <- read.csv('data/index_by_area.csv') %>%
  mutate(CV=sqrt(POPVAR)/POP)
