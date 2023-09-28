## GOA FHS Harvest Projection
## Standalone script to download data, build & run projections and apportionment,
## and process outputs leveraging afscassess/afscdata packages

## M Kapur Fall 2024, with code from B Williams, C Monnahan, C McGilliard

## Packages and setup
require(dplyr)
require(here)
require(ggplot2)
require(reshape2)
require(afscassess)
require(afscdata)

 
year <- 2023
species <- 10130
# afscdata::setup_folders(year) ## run one time
ggplot2::theme_set(afscassess::theme_report()) 
pull_date <- lubridate::as_date('2023-09-28')

## Data pull ----
afscdata::goa_fhs(year,off_yr = TRUE)

## Projections ----
### Estimate Catches----
## checked by eye that these matched, though EGOA values are off for many
catches_area <- read.csv(here::here(year,'data','raw','fsh_catch_data.csv')) %>% 
  dplyr::group_by(year,fmp_subarea) %>%
  dplyr::summarise(total = sum(weight_posted)) %>%
  select(year, fmp_subarea, total) %>% 
  tidyr::pivot_wider(., id_cols = year, names_from = fmp_subarea, values_from = total) %>%
  mutate(EGOA = sum(EY,WY,SE, na.rm=TRUE),
         total = sum(WG,CG,EGOA, na.rm=TRUE)) %>%
  select(year, total, WGOA = WG, CGOA=CG, EGOA) 


## manually downloaded this and last year's weekly catches from AKFIN
# https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish

files <- list.files(here::here(year,'data','raw','weekly_catches'), full.names=TRUE)
test <- lapply(1:length(files), function(i){
  skip <- grep('ACCOUNT.NAME', readLines(files[i]))-1
  data.frame(read.table(files[i], skip=skip, header=TRUE, sep=',',
                        stringsAsFactors=FALSE))
})

weekly_catches <- do.call(rbind, test) %>%
  dplyr::rename(species=ACCOUNT.NAME, date = WEEK.END.DATE, catch = METRIC.TONS) %>% 
  filter(grepl('Flathead',x = species)) %>%   ## The species is split by area still
  mutate(date = lubridate::mdy(date), 
         week=lubridate::week(date),  
         year=lubridate::year(date)) 

## true catch observed this year so far. The current catch
## is not consistent with what was pulled above; likely the weekly tables are lagging

catch_this_year <- weekly_catches %>% 
  filter(year==2023) %>%
  pull(catch) %>% sum

weekly_catches %>%
  group_by(year) %>% summarise(sum(catch))

## years
catch_to_add <- weekly_catches %>% filter(year>=year-5 & week > week(today())) %>%
  group_by(year) %>% summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean
message("Precited ", year, " catch= ", round(catch_this_year + catch_to_add,0))
projc <- SS_catch %>% 
  filter(year  < year & year  > (year-6)) %>% 
  summarise(mean(catch )) %>% as.numeric()

yrs_spcat <- 2020:(year+2) ## years to infill catches
catchvec <- data.frame('year' = yrs_spcat, 'catches' = NA) 
## fill in known catches (complete years)
catchvec$catches[catchvec$year < year] <- SS_catch$catch[SS_catch$year < year & SS_catch$year %in% yrs_spcat]
## fill in estimated & projected catches
catchvec$catches[catchvec$year == year] <- round(catch_this_year + catch_to_add,0)
catchvec$catches[catchvec$year > year] <- round(projc,0)
catchvec <- as.matrix(catchvec)


### Populate & Run Proj
## Tables ----
## Figures ----


