## GOA FHS Harvest Projection
## Standalone script to download data, build & run projections and apportionment,
## and process outputs leveraging afscassess/afscdata packages

## M Kapur Fall 2023, with code from B Williams, C Monnahan, C McGilliard

## Packages and setup ----
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

## load last year's model
base_model <- r4ss::SS_output(here::here(2022,'model_runs','m0_8-newMI-biasAdj'), covar=TRUE, verbose=FALSE, printstats=FALSE) 

## Data pull ----
## only run this once or when you want to update data
# afscdata::goa_fhs(year,off_yr = TRUE)

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

write.csv(catches_area,file = here::here(year,'data','output','catches_by_area.csv'),row.names = FALSE)

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

write.csv(weekly_catches,file = here::here(year,'data','raw','weekly_catches','weekly_catches.csv'),row.names = FALSE)

weekly_catches <- read.csv(here::here(year,'data','raw','weekly_catches','weekly_catches.csv'))
## true catch observed this year so far. The current catch
## is not consistent with what was pulled above; likely the weekly tables are lagging
## therefore use these data to get the estimated expansion factor and add to the TRUE
## observed catch from akfin, above

catch_this_year <- catches_area %>% 
  filter(year==2023) %>%
  pull(total) 

weekly_catches %>%
  group_by(year) %>% summarise(sum(catch))

## calculate in-year expansion (mean catch for previous complete years)
catch_to_add <- weekly_catches %>% filter(year>=year-5 & week > lubridate::week(lubridate::today())) %>%
  group_by(year) %>% summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean

message("Precited ", year, " catch = ", round(catch_this_year + catch_to_add,0))
message("Expansion Factor = ", round((catch_this_year+catch_to_add)/catch_this_year,2))

## calculate catch for next two projection years
projc <- catches_area %>% 
  filter(year  < 2023 & year  > (2023-6)) %>% 
  ungroup()%>%
  summarise(mean(total)) %>% 
  as.numeric

yrs_spcat <- 2022:(year+2) ## years to infill catches (since lass full assessment)
catchvec <- data.frame('year' = yrs_spcat, 'catches' = NA) 
## fill in known catches (complete years)
catchvec$catches[catchvec$year < year] <- catches_area$total[catches_area$year < year & catches_area$year %in% yrs_spcat]
## fill in estimated & projected catches
catchvec$catches[catchvec$year == year] <- catch_this_year + catch_to_add
catchvec$catches[catchvec$year > year] <- projc
catchvec <- as.matrix(catchvec)

writeLines(as.character(catchvec), con =  here::here(year,'data','output','catches_for_proj.txt'))
save(catchvec,file = here::here(year,'data','output','catches_for_proj.rdata'))

### Populate & Run Proj ----
## load the functions (stored globally)
## since the model hasn't changed, simply copy everything over from last year

file.copy(list.files(here::here(2022,'projection'), full.names = TRUE),
          here::here(year,'projection'),overwrite = TRUE)
 
## load catchvec
load(here::here(year,'data','output','catches_for_proj.rdata'))

## load sppcatch
sppcatch <- readLines(here::here(year,'projection','spp_catch.dat'))
sppcatch[3] <- as.numeric(sppcatch[3])+1 ## increment year up one
sppcatch<-sppcatch[-c((grep('future\tyear',sppcatch)+1):length(sppcatch))] ## delete old values
## overwite with new years
write(sppcatch,file=here::here(year,'projection','spp_catch.dat'),append=FALSE)
## overwrite with new catches
write.table(catchvec, file=here::here(year,'projection','spp_catch.dat'), 
            row.names=FALSE, col.names=FALSE, append = TRUE)

## make sure that the spp_catch is also in the /data folder
file.copy(here::here(year,'projection','spp_catch.dat'),
          here::here(year,'projection','data','spp_catch.dat'),overwrite = TRUE)
## run projection
setwd(here::here(year,'projection'))
shell('main')


## Tables ----


## Figures ----


