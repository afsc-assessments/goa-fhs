## Standalone script for 2024 GOA_FHS Harvest Projection
## This will download & generate everything used to populate the SAFE
## M Kapur maia.kapur@noaa.gov Jun 2024

# Packages and setup ---- 
require(dplyr)
require(tidyverse)
require(here)
require(ggplot2); require(ggsidekick)
require(r4ss);
require(lubridate)
require(reshape2)
require(data.table) 
require(afscassess)
require(afscdata)
require(rema)

year <- 2024
## load previous full assessment
mod <- r4ss::SS_output(here::here('2022','model_runs','m0_8-newMI-biasAdj'))

## only need to do the following once:
# afscdata::setup_folders(year) 
# afscdata::goa_fhs(year,off_yr = TRUE)
## manually download any needed years of weekly catches from 
## https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish


# Catches ---- 

## For projection model need to predict total catches in this
## year. Use weekly catches from from previous years to get
## proportion of catch by week to estimate terminal year catch.


#* format observed catches ---- 
catch0 <- read.csv(here::here(year,'data','raw','fsh_catch_data.csv')) 
names(catch0) <- toupper(names(catch0))
this_year <- lubridate::year(Sys.Date())
last_yr <- this_year-1
catch <- catch0 %>% 
  select(YEAR, FMP_SUBAREA, WEIGHT_POSTED) %>%
  summarise(STONS = sum(WEIGHT_POSTED),.by = c(YEAR,FMP_SUBAREA)) %>%
  tidyr::pivot_wider(., id_cols = YEAR, 
                     names_from = FMP_SUBAREA,
                     values_from = STONS) %>%
  ## calculate totals - reformatting happens in .Rmd
  summarise(EGOA = sum(WY,EY,SE,na.rm = TRUE), 
            WGOA = WG,
            CGOA=CG,
            TTONS = sum(WGOA, CG, EGOA), .by = YEAR) %>%
  select(year = YEAR, TTONS, WGOA, CGOA, EGOA) 

write.csv(catch, file=here(year, 'data','output',
                           paste0(Sys.Date(),'-catch_observed.csv') ), row.names=FALSE)
#* projected catches ----
files <- list.files(here::here(year,'data','raw','weekly_catches'), full.names=TRUE)
test <- lapply(1:length(files), function(i){
  skip <- grep('ACCOUNT.NAME', readLines(files[i]))-1
  data.frame(read.table(files[i], skip=skip, header=TRUE, sep=',',
                        stringsAsFactors=FALSE))
})
weekly_catches <- do.call(rbind, test)
names(weekly_catches) <- c('species', 'date', 'catch')
weekly_catches <- weekly_catches %>%
  ## No species for Bering flounder, probably in FHS already
  filter(grepl("Flathead", x=species)) %>%
  mutate(date=mdy(date), week=week(date),  year=year(date))
catch_this_year <- weekly_catches %>% filter(year==this_year) %>%
  pull(catch) %>% sum
## Get average catch between now and end of year for previous 5 years.
## add to what came from AKFIN cause slight inconsistency with weekly catches to date.
catch_to_add <- weekly_catches %>% filter(year>=this_year-5 & week > week(today())) %>%
  group_by(year) %>% summarize(catch=sum(catch), .groups='drop') %>%
  pull(catch) %>% mean
message("Predicted ", this_year, " catch = ", round(catch$catch_mt[catch$yr == this_year] + catch_to_add,0)) ##9272


mean_catch <- catch %>% 
  filter(YEAR %in% c((this_year-5):this_year)) %>% 
  summarise(mean(TTONS)) %>%
  as.numeric() %>%
  round()
catch_projection <- cbind(YEAR = this_year+c(-2:2),
                          CATCH_MT =   c(round(catch$TTONS[catch$YEAR == this_year-2]),
                                         round(catch$TTONS[catch$YEAR == this_year-1]),
                                         round(catch$TTONS[catch$YEAR == this_year] + catch_to_add,0),
                                         rep(mean_catch,2)))

write.csv(catch_projection, file=here(year,'data','output',
                                      paste0(Sys.Date(),'-catch_for_spm.csv') ), row.names=FALSE)

# Survey Data----
## Years < 1990 should be dropped from the assessment/proj model
## but because the 2022 model was an update I kept them
## dropping them from the REMA model leads to convergence issues so they are retained.

afsc <- afscdata::connect(db = 'afsc')

BIOM <-  dplyr::tbl(afsc, dplyr::sql('gap_products.area')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% 47, 
                area_type == 'REGULATORY AREA', 
                design_year < 2024) %>% 
  dplyr::left_join(dplyr::tbl(afsc, dplyr::sql('gap_products.biomass')) %>% 
                     dplyr::rename_all(tolower) %>% 
                     dplyr::filter(species_code %in% c(10130, 10140))) %>% 
  dplyr::collect()  

write.csv(BIOM, here::here(year,'data','raw',paste0(Sys.Date(),'-goa_area_bts_biomass_data.csv') ), row.names=FALSE)

## formatting for REMA 
## need dataframe with strata, year, biomass, cv
names(BIOM) <- toupper(names(BIOM))
BIOM %>%
  select(YEAR, BIOMASS_MT, BIOMASS_VAR,AREA_NAME) %>%
  mutate(CV = sqrt(BIOMASS_VAR)/BIOMASS_MT) %>%
  rename(strata = AREA_NAME, year = YEAR, biomass = BIOMASS_MT, cv = CV) %>%
  select(strata, year, biomass, cv, -BIOMASS_VAR) %>%
  write.csv(., here::here(year,'data','output',
                          paste0(Sys.Date(),'-index_byArea-REMA.csv')), 
            row.names=FALSE)

## formatting for SAFE
index_by_area <- BIOM %>%
  select(YEAR, BIOMASS_MT, BIOMASS_VAR,AREA_NAME) %>%
  mutate(CV = sqrt(BIOMASS_VAR)/BIOMASS_MT) %>%
  tidyr::pivot_wider(names_from = AREA_NAME,
                     values_from = c(BIOMASS_MT, CV, BIOMASS_VAR), 
                     id_cols = YEAR) %>%
  
  mutate(total_biomass = sum(`BIOMASS_MT_Eastern GOA`, `BIOMASS_MT_Central GOA`,
                             `BIOMASS_MT_Western GOA`, na.rm = TRUE), 
         total_biomass_var = sum(`BIOMASS_VAR_Eastern GOA`, `BIOMASS_VAR_Central GOA`,
                                 `BIOMASS_VAR_Western GOA`, na.rm = TRUE), 
         total_biomass_cv = sqrt(total_biomass_var)/total_biomass, .by = YEAR) %>%
  select(YEAR, total_biomass, 
         total_biomass_cv, 
         `BIOMASS_MT_Western GOA`,
         `CV_Western GOA`,
         `BIOMASS_MT_Central GOA`,
         `CV_Central GOA`,
         `BIOMASS_MT_Eastern GOA`,
         `CV_Eastern GOA`) 
write.csv(index_by_area, here::here(year,'data','output',
                                    paste0(Sys.Date(),'-index_byArea.csv')), 
          row.names=FALSE)

# Projections & SAFE Table----
## just ensure the values at the bottom of spm.dat match the 
## catches in catch_projection made above
## no change to other inputs
#* run SPM ----
setwd(here(year,'projection_spm')) 
shell('spm')

#* render SAFE table ---- 
rec_table1 <-
  read.table(here::here(year,'projection_spm','percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% c(2025,2026) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  tidyr::pivot_wider(names_from=year, values_from=value)
rec_table1[3:6,3:4] <- rec_table1[3:6,3:4]

rec_table2 <-
  read.table(here::here(year,'projection_spm','alt_proj.out'), header=TRUE) %>%
  filter(Year %in% c(2025,2026) & Alt==1) %>%
  tidyr::pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  tidyr::pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table2[,2:3] <- rec_table2[,2:3]
rec_table <- bind_rows(rec_table1, rec_table2)

## There's an error in spm.tpl where the sex ratio calcs didn't happen for the 
## biomass reference points. Here I'm manually dividing them by two.
rec_table[3:5,2:3]<-rec_table[3:5,2:3]/2

## change order to match SAFE format & magnitudes
rec_table <-rec_table[c(12,6,3,4,5,2,1,1,10,9,9),] 

# rec_table[c(1:5,9:11),2:3] <-formatC(rec_table[c(1:5,9:11),2:3] , format="d", big.mark=",") 
write.csv(rec_table, 
          file = here::here(year,'projection_spm',paste0(Sys.Date(),'-exec_summ.csv')), row.names=FALSE)

#* run PROJ ----
setwd(here(year,'projection')) 
shell('main')
rec_table1 <-
  read.table('percentdb.out') %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year+1:2) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  pivot_wider(names_from=year, values_from=value)
rec_table2 <-
  read.table('alt2_proj.out', header=TRUE) %>%
  filter(Year %in% (this_year+1:2)) %>%
  pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table <- bind_rows(rec_table1, rec_table2)
## change order to match SAFE format & magnitudes
rec_table <- rec_table[c(11,6,3,5,4,2,1,1,9,8,8),] 
rec_table[c(1:5,9:11),2:3] <- round(rec_table[c(1:5,9:11),2:3]*1000)
write.csv(rec_table, 
          file = here::here(year,'projection',paste0(Sys.Date(),'-exec_summ.csv')), row.names=FALSE)

# REMA apportionment ----
## With Jane I discovered that dropping the data before 1990s
## removes contrast to the point that process error goes to zero
## in the Western GOA and the model fails to converge
## so here I bind in the old survey data as was used in the 2023 REMA projections
## also we showed there is no need to fit things individually

#* Run REMA ----
load(here::here('2023','apportionment',"rema_output.rdata")) ## loads as "output"
biomass_dat <- read.csv(here::here(year, 'data','output','2024-06-05-index_byArea-REMA.csv')) %>%
  select(year, strata,biomass, cv) %>%
  bind_rows(., output$biomass_by_strata %>%
              select(year, strata, biomass=obs, cv = obs_cv) %>%
              filter(year < 1990)) %>%

  mutate(strata = case_when(strata == 'CENTRAL GOA'~'Central GOA',
                            strata == 'WESTERN GOA'~'Western GOA',
                            strata == 'EASTERN GOA'~'Eastern GOA',
                            TRUE~strata)) %>%
  filter(year != 2025 & !is.na(biomass)) %>%
  arrange(year) 
rm(output) ## delete the 2023 outputs

write.csv(biomass_dat,
          here::here(year,'data','output','biomass_dat_with_1984.csv'), row.names = FALSE)


input <- rema::prepare_rema_input(model_name = paste0("TMB: GOA FHS MULTIVAR"), 
                            biomass_dat=biomass_dat,
                            ## alphabetical mirroring of pe independence
                            PE_options = list(pointer_PE_biomass=c(1,2,3)))
## fit REMA, save outputs and plots
m <- fit_rema(input);save(m, file = here::here(year,'apportionment','rema_model.rdata'))
output <- tidy_rema(m);save(output, file = here::here(year,'apportionment','rema_output.rdata'))
plots <- plot_rema(tidy_rema = output, biomass_ylab = 'Biomass (t)') # optional y-axis label
plots$biomass_by_strata +ggsidekick::theme_sleek()
ggsave(plots$biomass_by_strata, file = here::here(year,'apportionment','rema_outs.png'), width = 12, height = 10, unit = 'in', dpi = 520)

#* Calculate Apportionment ----
## The biomass fractions for 2024 are the same as in 2023 (I checked)
## These come from the AKFIN Dashboard > scroll to "fractional biomass..."
egfrac <- read.csv(here::here(year, 'apportionment','biomass_fractions_egoa.csv'))
props <- output$proportion_biomass_by_strata %>% 
  filter(year == 2023) %>% 
  mutate(WestYakutat = `Eastern GOA`*egfrac$Western.Fraction,
         Southeast = `Eastern GOA`*egfrac$Eastern.Fraction) %>%
  select(`Western GOA`, `Central GOA`, WestYakutat,Southeast)

sum(props)==1

rec_table <- read.csv(here::here(year,'projection',
                                 '2024-06-06-exec_summ.csv'))

abc_y1 <- as.numeric( rec_table[10,2]) 
abc_y2 <- as.numeric( rec_table[10,3]) 
apportionment2 <- apply(props, 2, FUN = function(x) round(x*c(abc_y1,abc_y2) )) %>%
  rbind( round(props*100,2) ,.) %>%
  data.frame() %>%
  mutate(Total = c("",abc_y1,abc_y2),
         Year = noquote(c("",year(Sys.Date())+1,year(Sys.Date())+2)),
         Quantity = c("Area Apportionment %", 
                      "ABC (t)",
                      "ABC (t)")) %>% select(Quantity, Year, everything())

## because the rounded totals don't perfectly sum to the ABC, locate the discrepancy and add to the highest area (per Chris)
diff23 <- abc_y1 - sum(apportionment2[2,3:6])
diff24 <- abc_y2 - sum(apportionment2[3,3:6])
apportionment2[2,4] <- apportionment2[2,4]+diff23
apportionment2[3,4] <- apportionment2[3,4]+diff24

abc_y1 - sum(apportionment2[2,3:6])==0
abc_y2 - sum(apportionment2[3,3:6]) ==0

write.csv(apportionment2,
          file = here::here(year,'apportionment',
                            paste0(Sys.Date(),"-AreaApportionment.csv")))


# Make Catch Figure ----
fig1a <- mod$timeseries %>% select(Yr, Bio_smry) %>%
  merge(.,mod$catch %>% select(Yr, Obs), by = 'Yr') %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)


#* SPM version ----
## projection catch values in Stock_catch, use Tot_biom from pdt

## summarise values in pdt - bigfile is now alt_proj.out
pdt <- data.frame(read.table(here::here(year,
                                        'projection_spm',
                                        "alt_proj.out"), 
                             header=TRUE))
pdt.long <- pivot_longer(pdt, cols=c(-Alt, -Stock, -Year), names_to='metric') %>%
  mutate(Alt=factor(Alt)) %>% group_by(Year, Alt, metric) %>%
  summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')
g <- ggplot(pdt.long, aes(Year,  med, ymin=lwr, ymax=upr, fill=Alt, color=Alt)) +
  facet_wrap('metric', scales='free_y') + ylim(0,NA) +
  geom_ribbon(alpha=.4) + theme_bw() +
  labs(x='Year', y='Estimated 80% CI')

fig1a <- mod$timeseries %>% select(Yr, Bio_smry) %>%
  merge(.,mod$catch %>% select(Yr, Obs), by = 'Yr') %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)


fig1b <- data.frame(Yr = year+c(-1:2),
                    Bio_smry = pdt%>% 
                      filter(Year %in% (year+c(-1:2))) %>% 
                      group_by(Year) %>%
                      summarise(Bio_smry =  round(mean(TotBiom),2)) %>% 
                      select(Bio_smry) ,
                    Obs = catch_projection$CATCH_MT[catch_projection$YEAR %in%
                                                      (year+c(-1:2))]) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)


fig1 <- rbind(fig1a, fig1b) 

#* proj version ----
pdt <- data.frame(read.table(here::here(year,'projection',"bigfile.out"), header=TRUE))
pdt.long <- pivot_longer(pdt, cols=c(-Alternative, -Spp, -Yr), names_to='metric') %>%
  mutate(Alternative=factor(Alternative)) %>% group_by(Yr, Alternative, metric) %>%
  summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')

fig1b <- data.frame(Yr =  (year+c(-1:2)),
                    Bio_smry = pdt %>% 
                      filter(Yr %in%  (year+c(-1:2))) %>% 
                      group_by(Yr) %>%
                      summarise(Bio_smry = 1000*round(mean(Tot_biom),2)) %>% 
                      select(Bio_smry) ,
                    Obs = catch_projection$CATCH_MT[catch_projection$YEAR %in%
                                                      (year+c(-1:2))]) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)
fig1 <- rbind(fig1a, fig1b)


#* render plot ----
ggplot(subset(fig1), 
       aes(x = Yr, y = catch_over_biomass)) +
  geom_line(lwd = 1, col = 'grey77') + 
  geom_point(data = subset(fig1, Yr == (year-(1:2))),
             col = 'blue', pch = 16) +
  geom_point(data = subset(fig1, Yr >= this_year),
             col = 'blue', pch = 1) +
  scale_x_continuous(labels = seq(1960,(year+2),5), 
                     breaks = seq(1960,(year+2),5))+
  scale_y_continuous(limits = c(0,0.02),
                     breaks = seq(0,0.02,0.01), 
                     labels = seq(0,0.02,0.01))+
  ggsidekick::theme_sleek()+
  labs(x = 'Year', y = 'Catch/Summary Biomass (age 3+)')
ggsave(last_plot(), height = 5, width = 8, dpi = 520,
       file = here::here(year,'projection',paste0('Fig1_catchvsbio.png')))
