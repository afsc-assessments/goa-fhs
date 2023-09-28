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

## make sure that the newest spp_catch is also in the /data folder
file.copy(here::here(year,'projection','spp_catch.dat'),
          here::here(year,'projection','data','spp_catch.dat'),overwrite = TRUE)
## run projection
setwd(here::here(year,'projection'))
shell('main')


## Tables ----
### Main SAFE Table ----
rec_table1 <-
  read.table(here::here(year,'projection','percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% c(2024,2025) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  tidyr::pivot_wider(names_from=year, values_from=value)

rec_table2 <-
  read.table(here::here(year,'projection','alt2_proj.out'), header=TRUE) %>%
  filter(Year %in% c(2024,2025)) %>%
  tidyr::pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  tidyr::pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table <- bind_rows(rec_table1, rec_table2)
## change order to match SAFE format & magnitudes
rec_table <- rec_table[c(11,6,3,4,5,2,1,1,9,8,8),] 
rec_table[c(6:8),2:3] <- round(rec_table[c(6:8),2:3],2)
rec_table[c(1:5,9:11),2:3] <- round(rec_table[c(1:5,9:11),2:3]*1000)


previous_rec_table <- read.csv(here::here(2022,'projection',"REC_TABLE.CSV"))
names(previous_rec_table) <- c('metric','2022','2023','X2023','2024')
previous_rec_table[,c('X2023','2024')] <- apply(previous_rec_table[,c('X2023','2024')],2,
                                                 FUN = function(x) as.numeric(gsub(",","",x)))

safe0 <- rbind(c(rep(0.2,4)),
               c(rep('3a',4)),
               cbind(previous_rec_table[,c('X2023','2024')], 
                     rec_table[,2:3]) )


rownames(safe0) <-c('M', 
                    'Tier',
                    "Projected total (3+) biomass (t)",
                    "Projected Female spawning biomass (t)",
                    "B100%",
                    "B40%",
                    "B35%",
                    "FOFL",
                    "maxFABC",
                    "FABC",
                    "OFL (t)",
                    "maxABC (t)",
                    "ABC (t)"
)


safe1 = as.matrix(noquote(apply(safe0, 2, function(x) prettyNum(x, big.mark = ",")))) 
# safe1 <- data.frame(safe1);names(safe1) <- names(safe0)


status = matrix(NA, nrow = 5, ncol = 4)
# colnames(status) <- c(2020,2021,2021,2022)
rownames(status) <- c('blank','Status','Overfishing','Overfished','Approaching Overfished')
status[2,] <- c(this_year-1,this_year,this_year,this_year+1)
status[1,c(1,3)] <- status[2,c(2,4)] <- status[3,c(2,4)] <- 'no'
status = data.frame(status)
names(status) = names(safe0)mes(status) <-names(data.frame(safe1)) <- c(this_year,this_year+1, this_year+1, this_year+2)

safe <- data.frame(rbind(safe1,status) ) %>% mutate(item = rownames(.)) %>%
  select(item,y1 = X2023, y2 = X2024, y3 = X2024.1, y4=X2025)

c1 = round(as.numeric(catchvec[2,2]))
c2 = round(as.numeric(catchvec[3,2]))
c3 = round(as.numeric(catchvec[4,2]))

safe::main_table(data = safe, year = 2023, tier = '3', c1,c2,c3)

write.csv(safe, file = here::here(year,'tables',paste0(Sys.Date(),'-safe_table.csv')), row.names=TRUE)
write.csv(rec_table, here::here(year, 'projection','rec_table.csv'), row.names=FALSE)


## Figures ----


