## M Sosa Kapur 
# maia.kapur@noaa.gov
# Summer 2024

## NOTES ##
## B Williams Indicated that GOA FHS is now in the gfdatapull package.
## We will pull these data for use in this partial assessment; recommend comparison
## with 2022 values at next full assessment.

# Packages and RODBC setup ----
# require(RODBC)
require(dplyr)
require(tidyverse)
require(here)
require(ggplot2); require(ggsidekick)
require(r4ss);
require(lubridate)
require(reshape2)
require(data.table)
# require(rstudioapi) ## enables masking of RODBC name, password
require(afscassess)
require(afscdata)

year <- 2024
## load previous full assessment
mod <- r4ss::SS_output(here::here('2022','model_runs','m0_8-newMI-biasAdj'))
# afscdata::setup_folders(year) ## run one time
# afscdata::goa_fhs(year,off_yr = TRUE)
## manually download any needed years of weekly catches from 
## https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish


# Catches ---- 

## For projection model need to predict total catches in this
## year. Use weekly catches from from previous years to get
## proportion of catch by week to estimate terminal year catch.


#* estimate & format catch for SS ----
## data from 1978-1990 seem to be from a different source. 
## paste from assessment.
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
  summarise(EGULF = sum(WY,EY,SE,na.rm = TRUE), 
            WGULF = WG,
            CG,
            TTONS = sum(WGULF, CG, EGULF), .by = YEAR) %>%
  select(YEAR, TTONS, WGULF, CG, EGULF) 

write.csv(catch, file=here(year, 'data','output',
                           paste0(Sys.Date(),'-catch_observed.csv') ), row.names=FALSE)

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



## survey data by area and depth (for REMA) ----
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

# #* plot catch in mt ----
# catch %>%  
#   ggplot(., aes(x = yr, y = catch_mt))+
#   theme_sleek(base_size = 14) +
#   theme(legend.position = c(0.9,0.9),
#         axis.line = element_line(colour = "black"), 
#         panel.border = element_blank())+
#   geom_line(lwd = 1, col = 'grey44') +
#   scale_x_continuous(limits = c(1975, 2025), 
#                      breaks = seq(1980,2020,10),
#                      labels = seq(1980,2020,10)) +
#   labs(y = 'Landings (mt)', x= 'Year')
# ggsave(last_plot(),
#        file = here('figs','landings.png'),
#        height = 4, width = 6, dpi = 520)
# 
# #* misc catch plots/eda ----
# 
# ## trawl fishery dwarfs all others; so they're likely just left in for ease
# catch0 %>% group_by(GEAR, YEAR) %>% summarise(mt = mean(TONS)) %>%
#   ggplot(., aes(x = YEAR, y = mt, fill = GEAR)) +
#   geom_bar(position = 'dodge', stat = 'identity')
# 
# # Survey Biomass ----
# #* dwnld srv ----
# #* 
# 
# 
# 
# test <- paste0("SELECT GOA.BIOMASS_TOTAL.YEAR as YEAR,\n ",
#                "GOA.BIOMASS_TOTAL.TOTAL_BIOMASS as BIOM,\n ",
#                "GOA.BIOMASS_TOTAL.TOTAL_POP as POP,\n ",
#                "GOA.BIOMASS_TOTAL.BIOMASS_VAR as BIOMVAR,\n ",
#                "GOA.BIOMASS_TOTAL.POP_VAR as POPVAR,\n ",
#                "GOA.BIOMASS_TOTAL.HAUL_COUNT as NUMHAULS,\n ",
#                "GOA.BIOMASS_TOTAL.CATCH_COUNT as NUMCAUGHT\n ",
#                "FROM GOA.BIOMASS_TOTAL\n ",
#                "WHERE GOA.BIOMASS_TOTAL.SPECIES_CODE in (",species,")\n ",
#                "ORDER BY GOA.BIOMASS_TOTAL.YEAR")
# index0 <- sqlQuery(AFSC, test)
# if(!is.data.frame(index)) stop("Failed to query GOA survey data")
# write.csv(index0, here('data','survey',paste0(Sys.Date(),'-index_raw.csv') ),row.names=FALSE)
# 
# ## Survey data by area (for viz) 
# 
# ## survey data by area and depth (for table)
# dd <- sqlColumns(AFSC, "GOA.BIOMASS_INPFC_DEPTH")
# message("Querying survey biomass data by area x depth...")
# test <- paste0("SELECT GOA.BIOMASS_INPFC_DEPTH.YEAR as YEAR,\n ",
#                "GOA.BIOMASS_INPFC_DEPTH.SUMMARY_AREA_DEPTH as DEPTH,\n",
#                # "GOA.BIOMASS_INPFC_DEPTH.REGULATORY_AREA_NAME as AREA,\n",
#                "GOA.BIOMASS_INPFC_DEPTH.AREA_BIOMASS as BIOM,\n ",
#                "GOA.BIOMASS_INPFC_DEPTH.AREA_POP as POP,\n ",
#                "GOA.BIOMASS_INPFC_DEPTH.BIOMASS_VAR as BIOMVAR,\n ",
#                "GOA.BIOMASS_INPFC_DEPTH.POP_VAR as POPVAR,\n ",
#                "GOA.BIOMASS_INPFC_DEPTH.HAUL_COUNT as NUMHAULS,\n ",
#                "GOA.BIOMASS_INPFC_DEPTH.CATCH_COUNT as NUMCAUGHT\n ",
#                "FROM GOA.BIOMASS_INPFC_DEPTH\n ",
#                "WHERE GOA.BIOMASS_INPFC_DEPTH.SPECIES_CODE in (",species,")\n ",
#                "ORDER BY GOA.BIOMASS_INPFC_DEPTH.YEAR")
# index_by_depth_area <- sqlQuery(AFSC, test)
# if(!is.data.frame(index_by_area))
#   stop("Failed to query GOA survey data by area")
# write.csv(index_by_area, here('data','survey',paste0(Sys.Date(),'-index_byArea.csv') ), row.names=FALSE)
# 
# #* format srv for SS ----
# ## this should be in Tons
# index <- index0 %>% group_by(YEAR) %>%
#   summarise(BIOM_T = BIOM,
#             CV_total=sqrt(BIOMVAR)/BIOM,
#             uci = BIOM_T +CV_total*BIOM_T, 
#             lci = BIOM_T -CV_total*BIOM_T ) %>%
#   mutate(month =1, fleet = 2) %>%
#   select(yr = YEAR, month, fleet, obs = BIOM_T, sterr  = CV_total)
# write.csv(index, here('data','survey',paste0(Sys.Date(),'-index_forSS.csv') ),row.names=FALSE)
# 
# #* survey biomass spot check ----
# subset(mod17$cpue, Yr == 2015) %>% select(Obs);subset(index, yr == 2015) %>% select(obs)
# 
# subset(mod17$cpue, Yr == 2011) %>% select(Obs);subset(index, yr == 2011) %>% select(obs)
# 
# subset(mod17$cpue, Yr == 2001) %>% select(Obs);subset(index, yr == 2001) %>% select(obs)
# #* plot srv in mt ----
# ## design-based
# ggplot(index, aes(x = yr, y = obs/1000)) +
#   theme_sleek(base_size = 14) +
#   theme(legend.position = c(0.9,0.9),
#         axis.line = element_line(colour = "black"), 
#         panel.border = element_blank())+
#   geom_line(lwd = 1, col = 'grey77') +
#   geom_point() +
#   scale_y_continuous(limits = c(0,350) ) +
#   scale_x_continuous(limits = c(1984, 2025),
#                      breaks = seq(1980,2020,10),
#                      labels = seq(1980,2020,10)) +
#   labs(x = 'Year', y = 'Survey Biomass (mt)')+
#   geom_ribbon(aes(ymin = (obs-sterr*obs)/1000, ymax = (obs+sterr*obs)/1000), alpha = 0.2)
# ggsave(last_plot(),
#        file = here('figs','design-based-indices.png'),
#        height = 4, width = 6, dpi = 520)
# 
# ## design based with VAST
# vast <- read.csv(here('data','survey','hippoglossoides_elassodon',
#                       'table_for_ss3.csv')) %>%
#   mutate(src = 'Model-Based (VAST)',
#          value=Estimate_metric_tons,
#          lci = (Estimate_metric_tons -SD_log*Estimate_metric_tons) ,
#          uci =  (Estimate_metric_tons +SD_log*Estimate_metric_tons) ) %>%
#   select(Year, value, lci, uci, src) %>%
#   rbind(., index %>% 
#           mutate(src = 'Design-Based',
#                  value = obs,
#                  lci = (obs-sterr*obs),
#                  uci = (obs+sterr*obs)) %>%   
#           select(Year=yr, value  , lci, uci, src)) %>%
#   filter(value > 0)
# 
# 
# ggplot(vast, aes(x = Year, y = value, color = src, fill = src, group = src)) +
#   geom_line(lwd = 1) +
#   theme_sleek(base_size = 14) +
#   theme(legend.position = c(0.75,0.9),
#         axis.line = element_line(colour = "black"), 
#         panel.border = element_blank())+
#   scale_color_manual(values = c('#015b58','goldenrod'))+
#   scale_fill_manual(values = c('#015b58','goldenrod'))+
#   scale_x_continuous(labels = seq(1980,2025,5),breaks = seq(1980,2025,5))+
#   scale_y_continuous(limits = c(0,350*1000) ) +
#   labs(x = 'Year', y = 'Survey Biomass (t)', color = '',fill = '')+
#   geom_ribbon(aes(ymin = lci, ymax = uci), color = NA, alpha = 0.2)
# 
# ggsave(last_plot(), height = 4, width = 6, dpi = 520,
#        file = here('figs',paste0(Sys.Date(),'-index_VASTvsDesign.png')))
# 
# ## design based survey in mt by area
# iba <- index_by_area  %>%
#   group_by(YEAR,AREA) %>%
#   summarise(BIOM_T = BIOM,
#             CV_total=sqrt(BIOMVAR)/BIOM )  
# 
# ggplot(iba, aes(x = YEAR, y= BIOM_T/1000, color = AREA, fill = AREA)) +
#   geom_line(lwd = 0.8) +
#   theme_sleek(base_size = 14) +
#   theme(legend.position = c(0.9,0.9),
#         axis.line = element_line(colour = "black"), 
#         panel.border = element_blank())+
#   geom_ribbon(aes(ymin = (BIOM_T-BIOM_T*CV_total)/1000,
#                   ymax= (BIOM_T+BIOM_T*CV_total)/1000), 
#               alpha = 0.2, color = NA)+
#   scale_color_manual(labels = c('CGOA','EGOA','WGOA'),
#                      values = c('#015b58','#984e73','goldenrod'))+
#   scale_fill_manual(labels = c('CGOA','EGOA','WGOA'),
#                     values = c('#015b58','#984e73','goldenrod'))+
#   scale_x_continuous(limits = c(1984, 2025), 
#                      breaks = seq(1980,2020,10),
#                      labels = seq(1980,2020,10)) +
#   labs(x = 'Year', y = 'Survey Biomass (mt)',
#        fill = 'Survey Area', col = 'Survey Area')
# 
# ggsave(last_plot(), height = 4, width = 6, dpi = 520,
#        file = here('figs',paste0(Sys.Date(),'-index_byArea_wCVs.png')))
# 
# #* misc srv plots/eda ----
# ## what's up with year 2001?
# 
# ggplot(index, aes(x = yr, y = obs))+
#   geom_line(col = 'red') +
#   geom_line(data = mod17$cpue, aes(x = Yr, y = Obs))
# 
# 
# # Comps ----
# length_bins <- seq(6,70,2) 
# age_bins <- seq(1,29,1);max_age <- max(age_bins)
# ## read in various functions from Carey's scripts
# source(paste0(newsbssdir,"functions/BIN_LEN_DATA.R"))
# source(paste0(newsbssdir,"functions/BIN_AGE_DATA.R"))
# source(paste0(newsbssdir,"inputs_piecebypiece/get_fishery_lengths/GET_CATCH_AT_SIZE_PIECES.R"))
# source(paste0(newsbssdir,"inputs_piecebypiece/get_fishery_lengths/GET_DOM_SPCOMP_LEN_COMBO.R"))
# source(paste0(newsbssdir,"inputs_piecebypiece/get_fishery_lengths/GET_FOR_SPCOMP_LEN_COMBO.R"))
# 
# #* survey nhauls ----
# 
# ## nsamp query
# ## The survey sample size (nhauls) was derived using W Paullson's code.
# ## Carey indicates that lengths only come from "good" hauls anyway,
# ## though I notice these values vary slightly (about 5 hauls) from what was used in 2017.
# ## source("C:\\GitProjects\\newsbss\\Get_GOA_Survey_Length_and_Age_Comp\\Get_Survey_Length_Age_Stats.R")
# hauljoin_query <-paste0("SELECT c.YEAR,\n ",
#                         "l.SPECIES_CODE,\n ",
#                         "c.HAULJOIN,\n ",
#                         "l.LENGTH,\n ",
#                         "l.FREQUENCY,\n ",
#                         "l.SEX\n ",
#                         "FROM racebase.length l,\n ",
#                         "goa.cpue c\n ",
#                         "WHERE l.HAULJOIN     = c.HAULJOIN\n ",
#                         "AND l.CATCHJOIN      = c.CATCHJOIN\n ",
#                         "AND l.REGION         = c.SURVEY\n ",
#                         "AND (l.SPECIES_CODE IN (",species,")\n ",
#                         "AND l.REGION         = ",sp_area,")\n ",
#                         "GROUP BY c.YEAR,\n ",
#                         "  l.SPECIES_CODE,\n ",
#                         "  c.HAULJOIN,\n ",
#                         "  l.LENGTH,\n ",
#                         "  l.FREQUENCY,\n ",
#                         "  l.SEX\n ",
#                         "ORDER BY l.species_code,\n ",
#                         "  c.YEAR")
# 
# hauljoin <- sqlQuery(AFSC,hauljoin_query)
# 
# ## for each year, sum the nhauls. Wayne's code also does this for males and females, but this is all we need for SS.
# nhauls <- hauljoin %>% 
#   group_by(YEAR) %>%
#   summarise(nhaul = n_distinct(HAULJOIN))
# 
# write.csv(nhauls, here('data','comp',paste0(Sys.Date(),"-survey_nhauls.csv")), row.names = FALSE)
# #* length comps---- 
# #** fishery length comps ----
# # data ommitted in 1982:1988,2000, 2008 because less than 15 hauls    
# ## these are presented as straight up numbers (though unclear how these are decimals); the nhauls are a separate value
# ## Get_Fishery_Lengths_With_Extrapolated_Number.R
# ## Go query domestic (DLCOMP) and foreign (FLCOMP) fishery length information from OBSINT and NORPAQ
# ## Using extrapolated number (and the equivalent from foreign data), calculate population-level
# ## length comps in terms of numbers return a dataframe with those comps (comps) as well as the total population numbers 
# ## summed over males, females, and length bins for later use (totals)
# ## it does not appear that unsexed values were used, nor was gear considered in the frequency calculation
# fish_lcomp0 <- rbind(GET_CATCH_AT_SIZE_PIECES(type="D",fsh_sp_str=103,sp_area,len_bins=length_bins,bin=TRUE),
#                      GET_CATCH_AT_SIZE_PIECES(type="F",fsh_sp_str=103,sp_area,len_bins=length_bins,bin=TRUE)) %>%
#   filter(SEX != 'U')
# 
# write.csv(fish_lcomp0,file = here('data','comp',paste0(Sys.Date(),'-fishery_lengthcomp_raw.csv')),row.names = FALSE)
# 
# #Nsamp for Fishery lengths (and tables for the SAFE):
# #Get_Fishery_Length_HaulStats_With_NORPAQ.R"
# low.nmfs.area = "'600'" #"'600'" #500 
# hi.nmfs.area = "'699'" #"'699'"  #544
# ## foreign fishery nhauls from NORPAQ (>= 1989)
# Fnhaul_query <-paste0("SELECT NORPAC.FOREIGN_LENGTH.SPECIES,\n ",
#                       "NORPAC.FOREIGN_LENGTH.SEX,\n ",
#                       "NORPAC.FOREIGN_LENGTH.YEAR,\n ",
#                       "NORPAC.FOREIGN_LENGTH.FREQUENCY,\n ",
#                       "NORPAC.FOREIGN_HAUL.GENERIC_AREA,\n ",
#                       "NORPAC.FOREIGN_LENGTH.SIZE_GROUP,\n ",
#                       "NORPAC.FOREIGN_HAUL.HAUL_JOIN,\n ",
#                       "NORPAC.FOREIGN_HAUL.HOOKS_PER_SKATE,\n ",
#                       "NORPAC.FOREIGN_HAUL.NUMBER_OF_POTS\n ",
#                       "FROM NORPAC.FOREIGN_LENGTH\n ",
#                       "INNER JOIN NORPAC.FOREIGN_HAUL\n ",
#                       "ON NORPAC.FOREIGN_HAUL.HAUL_JOIN    = NORPAC.FOREIGN_LENGTH.HAUL_JOIN\n ",
#                       "WHERE NORPAC.FOREIGN_LENGTH.SPECIES = ",103,"\n ",
#                       "AND NORPAC.FOREIGN_HAUL.GENERIC_AREA BETWEEN ",low.nmfs.area," AND ",hi.nmfs.area)
# 
# hauljoin <- sqlQuery(AFSC,Fnhaul_query)
# hauljoin <-hauljoin[hauljoin$GENERIC_AREA!=670,]
# hauljoin$ID<-hauljoin$HAUL_JOIN
# ## for each year, sum the nhauls. Wayne's code also does this for males and females, but this is all  we need for SS.
# nhauls_for <- hauljoin %>% 
#   group_by(YEAR) %>%
#   summarise(nhaul = n_distinct(ID))
# 
# ## domestic fishery nhauls from NORPAQ (>= 1989)
# Dnhaul_query <-paste0("SELECT OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.SPECIES,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.SEX,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.LENGTH,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.FREQUENCY,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.CRUISE,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.PERMIT,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.GEAR_PERFORMANCE,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.YEAR,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.GEAR,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.NMFS_AREA,\n ",
#                       "OBSINT.DEBRIEFED_LENGTH.HAUL_OFFLOAD,\n ",
#                       "OBSINT.DEBRIEFED_HAUL.HAUL_SEQ,\n ",
#                       "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),9,19) AS LAST1,\n ",
#                       "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),1,8) AS FIRST1\n ",
#                       "FROM OBSINT.DEBRIEFED_LENGTH\n ",
#                       "INNER JOIN OBSINT.DEBRIEFED_HAUL\n ",
#                       "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN    = OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN\n ",
#                       "WHERE OBSINT.DEBRIEFED_LENGTH.SPECIES = ",103,"\n ",
#                       "AND OBSINT.DEBRIEFED_LENGTH.NMFS_AREA BETWEEN ",low.nmfs.area," AND ",hi.nmfs.area)
# 
# 
# hauljoin <- sqlQuery(AFSC,Dnhaul_query)
# hauljoin <-hauljoin[hauljoin$NMFS_AREA!=670,]
# hauljoin$ID<-paste0(hauljoin$FIRST1,hauljoin$LAST1)
# ## for each year, sum the nhauls. Wayne's code also does this for males and females, but this is all  we need for SS.
# nhauls_dom <- hauljoin %>% 
#   group_by(YEAR) %>%
#   summarise(nhaul = n_distinct(ID))
# 
# 
# 
# # fish_lcomp0 %>% 
# #   filter(LNUMBERS!=0) %>%
# #   group_by(YEAR, SEX) %>%
# #   summarise(n = n()) %>% View()
# 
# fish_lcomp <- fish_lcomp0 %>%
#   group_by(YEAR, LENGTH, SEX) %>%
#   #1988-1990 are in both foreign and domestic and represent different fisheries at that time
#   #Add length frequencies for years where both foreign and domestic fisheries were operating:
#   summarise(nobs = sum(LNUMBERS))  
# 
# ## get total number of samples in each year/sex/bin combo
# length_df_long1 <- merge(fish_lcomp,
#                          fish_lcomp %>% 
#                            group_by(YEAR) %>% 
#                            ## denominator is total number of samples this year
#                            summarise(nsamp = sum(nobs)),
#                          by = c('YEAR')) %>%
#   ## calculate frequencies 
#   mutate(freq = nobs/nsamp,
#          variable = ifelse(SEX == 'F', 1, 2)) %>%
#   select(-nobs, -nsamp, -SEX) %>% 
#   # ## fill missing year combos
#   tidyr::complete(., YEAR, variable, LENGTH ) %>%
#   arrange(., YEAR, LENGTH, variable) %>%
#   mutate(freq = ifelse(is.na(freq),0,freq))  
# 
# frontmatter <- data.frame(yr = unique(length_df_long1$YEAR), month = 7, fleet = 1,
#                           sex = 3, part = 0, Nsamp = c(nhauls_for$nhaul, nhauls_dom$nhaul))
# ## turn off years with fewer than 15 hauls
# # frontmatter$fleet[frontmatter$yr %in% c(1982:1988,2000,2008)] <- -1
# frontmatter$fleet[frontmatter$Nsamp<15] <- -1
# # turn off 2022 lengths
# # frontmatter$fleet[frontmatter$yr == 2022 ] <- -1
# 
# fishery_length_comps <- cbind(frontmatter,
#                               length_df_long1 %>%
#                                 filter(variable == 1) %>%
#                                 pivot_wider(., id_cols = YEAR, names_from = LENGTH, values_from = freq) %>%
#                                 select(-YEAR),
#                               length_df_long1 %>%
#                                 filter(variable == 2) %>%
#                                 pivot_wider(., id_cols = YEAR, names_from = LENGTH,values_from = freq) %>%
#                                 select(-YEAR))
# 
# #* spot check fishery lcomps ----
# ## should be within rounding range. Note that the values in the dat file are in full integers, whereas
# ## what gets reformatted via SS_output is scaled to 1. So long as these match at the same scale it's OK.
# mod17$lendbase %>% 
#   filter(Fleet == 1 & Yr == 2001 & Bin == 46 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(fishery_length_comps[19,'46'],4) 
# 
# fishery_length_comps[32,'Nsamp'] == 106
# 
# mod17$lendbase %>% 
#   filter(Fleet == 1 & Yr == 2007 & Bin == 26 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(fishery_length_comps[25,'26'],4)
# 
# 
# write.csv(fishery_length_comps,file = here('data','comp',paste0(Sys.Date(),'-fishery_lengthcomp_forSS.csv')),row.names = FALSE)
# 
# 
# #** survey length comps ----
# ## In the .dat file these are presented as run-on frequencies for males and females separately. Based extraction code
# ## on source(paste0(newsbssdir,"inputs_piecebypiece/Get_GOA_Survey_Length_and_Age_Comp/Get_GOA_Length_Comps.R"))
# 
# ## comp data query
# l.query<-paste0("SELECT GOA.SIZECOMP_TOTAL.SURVEY,\n ",
#                 "GOA.SIZECOMP_TOTAL.YEAR,\n ",
#                 "GOA.SIZECOMP_TOTAL.SPECIES_CODE,\n ",
#                 "GOA.SIZECOMP_TOTAL.SUMMARY_AREA,\n ",
#                 "GOA.SIZECOMP_TOTAL.LENGTH,\n ",
#                 "GOA.SIZECOMP_TOTAL.MALES,\n ",
#                 "GOA.SIZECOMP_TOTAL.FEMALES,\n ",
#                 "GOA.SIZECOMP_TOTAL.UNSEXED,\n ",
#                 "GOA.SIZECOMP_TOTAL.TOTAL\n ",
#                 "FROM GOA.SIZECOMP_TOTAL\n ",
#                 "WHERE GOA.SIZECOMP_TOTAL.SURVEY     = 'GOA'\n ",
#                 "AND GOA.SIZECOMP_TOTAL.SPECIES_CODE = ",species)
# 
# goa_lcomp_raw <- sqlQuery(AFSC,l.query) 
# 
# write.csv(goa_lcomp_raw,file = here('data','comp','raw',paste0(Sys.Date(),'-goa_lengthcomp_raw.csv')),row.names = FALSE)
# 
# goa_lcomp_long <- goa_lcomp_raw %>%
#   mutate(LENGTH = LENGTH/10)  %>%
#   BIN_LEN_DATA(.,length_bins) %>%
#   select(YEAR,BIN,MALES,FEMALES) %>%
#   melt(., id = c("YEAR","BIN")) # %>%
# 
# goa_lcomp_expand  <-  goa_lcomp_long %>%
#   group_by(YEAR, variable) %>%
#   ## ensure full range of LBINS are included (there are no obs between 62:68,
#   ## so complete() won't fill those in unless included)
#   expand(BIN = full_seq(BIN, 2)) %>% 
#   left_join(.,goa_lcomp_long) %>%
#   group_by(YEAR, variable, BIN) %>%
#   summarise(nobs = sum(value))
# ## fill NAs with zero
# goa_lcomp_expand$nobs[is.na(goa_lcomp_expand$nobs)] <- 0
# ## get total number of samples in each year/sex/bin combo
# goa_lcomp_freq <- merge(goa_lcomp_expand,
#                          goa_lcomp_expand %>% 
#                            group_by(YEAR) %>% 
#                            ## denominator is total number of samples this year
#                            summarise(nsamp = sum(nobs)),
#                          by = c('YEAR')) %>%
#   ## calculate frequencies 
#   mutate(freq = nobs/nsamp,
#          variable = ifelse(variable == 'FEMALES', 1, 2)) %>%
#   select(-nobs, -nsamp) %>% 
#   ## fill with all extant data combinations, using zeros for unobserved
#   tidyr::complete(., YEAR, variable, BIN, fill = list(freq =0)) %>%
#   arrange(., YEAR, BIN, variable) %>%
#   mutate(freq = ifelse(is.na(freq),0,freq)) 
#   
# 
# goa_lcomp_freq %>% 
#   group_by(YEAR, variable, BIN ) %>%
#   filter(YEAR == 2015)  %>% 
#   summarise(n())
# 
# goa_lcomp_freq 
# ## confirm we have all bins  
# sort(unique(goa_lcomp_freq$BIN))
# 
# ## there are no observations between 62 and 69
# unique(goa_lcomp_expand$LENGTH/10) %>% sort()
# 
# frontmatter <- data.frame(yr = unique(goa_lcomp_freq$YEAR), month = 7, fleet = 2, sex = 3, part = 0, Nsamp = nhauls$nhaul)
# 
# survey_length_comps <- cbind(frontmatter,
#                              goa_lcomp_freq %>%
#                                filter(variable == 1) %>%
#                                pivot_wider(., id_cols = YEAR, names_from = BIN, values_from = freq) %>%
#                                select(-YEAR),
#                              goa_lcomp_freq %>%
#                                filter(variable == 2) %>%
#                                pivot_wider(., id_cols = YEAR, names_from = BIN, values_from = freq) %>%
#                                select(-YEAR))
# 
# #* spot check survey lcomps ----
# 
# ## should be within rounding range
# mod17$lendbase %>% 
#   filter(Fleet == 2 & Yr == 2003 & Bin == 20 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(survey_length_comps[8,'20'],4)
# 
# mod17$lendbase %>% 
#   filter(Fleet == 2 & Yr == 2015  & Bin == 36 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(survey_length_comps[14,'36'],4)
# 
# mod17$lendbase %>% 
#   filter(Fleet == 2 & Yr == 2001 & Bin == 10 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(survey_length_comps[7,'10'],4)
# 
# mod17$lendbase %>% 
#   filter(Fleet == 2 & Yr == 2001 & Bin == 60 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(survey_length_comps[7,'60'],4)
# 
# mod17$lendbase %>% 
#   filter(Fleet == 2 & Yr == 2007 & Bin == 26 & Sex == 1) %>% 
#   select(Obs) %>% round(.,4) ==
#   round(survey_length_comps[10,'26'],4)
# 
# write.csv(survey_length_comps,file = here('data','comp',paste0(Sys.Date(),'-goa_lengthcomp_forSS.csv')),row.names = FALSE)
# 
# #* age comps----
# #** survey CAALs ----
# #Survey conditional age-at-lengths
# #do the SQL query and write the data necessary to read in to the next step here (which will also plot a bunch of age-length ggplots):
# # source("C:\\GitProjects\\newsbss\\Inputs_PieceByPiece\\Get_GOA_Survey_Length_and_Age_Comp\\Get_Survey_Length_Age_and_Plot.R")
# ## data repeat for both sexes; bins are uneven (don't have complete dimensions)
# ## note that in data males are 1, where as in SS females are 1
# 
# ALQuery<-paste0("SELECT RACEBASE.SPECIMEN.HAULJOIN,\n ",
#                 "RACEBASE.SPECIMEN.REGION,\n ",
#                 "RACEBASE.SPECIMEN.SPECIMENID,\n ",
#                 "RACEBASE.SPECIMEN.BIOSTRATUM,\n ",
#                 "RACEBASE.SPECIMEN.SPECIES_CODE,\n ",
#                 "RACEBASE.SPECIMEN.LENGTH,\n ",
#                 "RACEBASE.SPECIMEN.WEIGHT,\n ",
#                 "RACEBASE.SPECIMEN.SEX,\n ",
#                 "RACEBASE.SPECIMEN.AGE,\n ",
#                 "RACEBASE.HAUL.START_TIME,\n ",
#                 "RACEBASE.HAUL.BOTTOM_DEPTH,\n ",
#                 "RACEBASE.HAUL.STRATUM,\n ",
#                 "RACEBASE.HAUL.GEAR_TEMPERATURE,\n ",
#                 "RACEBASE.HAUL.BOTTOM_TYPE,\n ",
#                 "RACEBASE.HAUL.GEAR_DEPTH,\n ",
#                 "RACEBASE.HAUL.PERFORMANCE,\n ",
#                 "RACEBASE.HAUL.DURATION,\n ",
#                 "RACEBASE.HAUL.DISTANCE_FISHED,\n ",
#                 "RACEBASE.HAUL.NET_WIDTH,\n ",
#                 "RACEBASE.HAUL.NET_HEIGHT,\n ",
#                 "RACEBASE.HAUL.NET_MEASURED,\n ",
#                 "RACEBASE.HAUL.START_LATITUDE,\n ",
#                 "RACEBASE.HAUL.END_LATITUDE,\n ",
#                 "RACEBASE.HAUL.START_LONGITUDE,\n ",
#                 "RACEBASE.HAUL.END_LONGITUDE,\n ",
#                 "RACEBASE.HAUL.SURFACE_TEMPERATURE,\n ",
#                 "RACEBASE.HAUL.GEAR,\n ",
#                 "RACEBASE.HAUL.HAULJOIN,\n ",
#                 "RACEBASE.HAUL.BOTTOM_DEPTH,\n ",
#                 "RACEBASE.HAUL.ABUNDANCE_HAUL,\n ",
#                 "GOA.GOA_STRATA.INPFC_AREA,\n ",
#                 "GOA.GOA_STRATA.MIN_DEPTH,\n ",
#                 "GOA.GOA_STRATA.MAX_DEPTH,\n ",
#                 "GOA.GOA_STRATA.DESCRIPTION,\n ",
#                 "GOA.GOA_STRATA.REGULATORY_AREA_NAME,\n ",
#                 "GOA.GOA_STRATA.STRATUM_TYPE\n ",
#                 "FROM RACEBASE.SPECIMEN\n ",
#                 "INNER JOIN RACEBASE.HAUL\n ",
#                 "ON RACEBASE.SPECIMEN.HAULJOIN = RACEBASE.HAUL.HAULJOIN\n ",
#                 "INNER JOIN GOA.GOA_STRATA\n ",
#                 "ON RACEBASE.HAUL.STRATUM           = GOA.GOA_STRATA.STRATUM\n ",
#                 "WHERE RACEBASE.SPECIMEN.REGION     = 'GOA'\n ",
#                 "AND GOA.GOA_STRATA.SURVEY = 'GOA'\n ",
#                 # "AND RACEBASE.SPECIMEN.SPECIES_CODE = ",10180,"\n ",
#                 "AND RACEBASE.SPECIMEN.SPECIES_CODE = ",species,"\n ",
#                 "AND RACEBASE.HAUL.ABUNDANCE_HAUL   = 'Y'")
# 
# 
# # tt <- AL_df %>%
# #   filter(SEX != 3) %>%
# #   
# #   select(AGE, LENGTH, SEX, WEIGHT) %>%
# #   group_by(AGE, LENGTH, SEX) %>%
# #   summarise(!is.na(AGE) & !is.na(LENGTH) & is.na(WEIGHT))
# # 
# # with(tt, sum(!is.na(AGE) & !is.na(LENGTH) & is.na(WEIGHT)) )
# 
# AL_df<- sqlQuery(AFSC,ALQuery)  %>% 
#   filter(SEX != 3 
#          & !is.na(AGE) 
#          # & !is.na(LENGTH) 
#          & !is.na(WEIGHT)) %>% #drop unsexed
#   mutate(  LENGTH = round(LENGTH/10,0),  #bin the length obs as cm
#            YEAR = as.numeric(substr(START_TIME, 1, 4)),
#            Months = months(as.Date(START_TIME)),
#            Quarters = quarters(as.Date(START_TIME)),
#            Cohort = YEAR-AGE,
#            GrowthMorph = ifelse(REGULATORY_AREA_NAME == "EASTERN GOA", "EASTERN",'NOT_EASTERN')) %>%
#   ## Carey's code looks at complete cases via age and weight , but her notes indicate length - changing this
#   filter(!is.na(AGE) & !is.na(LENGTH)  ) %>%
#   BIN_AGE_DATA(.,age_bins) 
# 
# AL_df %>% filter(YEAR == 2001 & LENGTH %in% c(14,15) )
# AL_df %>% filter(YEAR == 1990 &  LENGTH > 140 & LENGTH < 150)
# 
# 
# write.csv(AL_df,file = here('data','comp',paste0(Sys.Date(),'-goa_CAAL_raw.csv')),row.names = FALSE)
# #Run this to get the conditional age-at-length data roughly formatted for assessment input
# # source("C:\\GitProjects\\newsbss\\Inputs_PieceByPiece\\Get_GOA_Survey_Length_and_Age_Comp\\GET_GOA_CONDITIONAL_AGE_AT_LENGTH.R")
# #run it for eastern == 0 (this will write output combined over areas)
# ## now count the number of observations in each age bin
# AL_df_long <- AL_df %>% 
#   BIN_LEN_DATA(.,len_bins = length_bins)   %>%
#   mutate(lBIN = BIN) %>%
#   select(-BIN) %>%
#   filter(!is.na(lBIN) & !is.na(aBIN)) %>%
#   select(YEAR,  WEIGHT, SEX,AGE, aBIN, LENGTH, lBIN) %>%
#   fill(aBIN) %>%
#   # filter(YEAR == 1990 & lBIN <22) %>%
#   group_by(YEAR, SEX, lBIN, aBIN) %>%
#   ## this sums records across sex
#   summarise(nobs = n()) %>% 
#   ungroup() %>%
#   # ## fill missing year-length-age combos
#   complete(YEAR, SEX, lBIN, aBIN) %>%
#   arrange(., as.numeric(aBIN)) 
# 
# AL_df_wide_F <-     AL_df_long %>%
#   filter(SEX == 2) %>% select(-SEX) %>%
#   pivot_wider(id_cols = c(YEAR,lBIN), names_from =  aBIN, values_from = nobs) %>%
#   arrange(YEAR, lBIN)
# 
# AL_df_wide_M <-  AL_df_long %>%
#   filter(SEX == 1) %>% select(-SEX) %>%
#   pivot_wider(id_cols = c(YEAR,lBIN), names_from =  aBIN, values_from = nobs) %>%
#   arrange(YEAR, lBIN)
# 
# goa_caal_obs0 <- rbind(AL_df_wide_F,AL_df_wide_M) %>% data.frame() 
# 
# ## fill front matter
# AL_df_wide <- goa_caal_obs0 %>%
#   mutate(month = 7, fleet = 2,
#          sex = c(rep(1, nrow(AL_df_wide_F)), 
#                  rep(2,nrow(AL_df_wide_M))),  part = 0, ageerr = 1,
#          Lbin_lo = lBIN,  Lbin_hi = lBIN,
#          Nsamp = c(rowSums(AL_df_wide_F[3:ncol(AL_df_wide_F)],na.rm = T),
#                    rowSums(AL_df_wide_M[3:ncol(AL_df_wide_M)],na.rm = T))) %>%
#   select(yr=YEAR,month, fleet, sex, part, ageerr,  Lbin_lo, Lbin_hi, Nsamp, everything(), -lBIN  ) %>%
#   ## duplicate columns x sex
#   cbind(., goa_caal_obs0[,3:ncol(goa_caal_obs0)])
# ## drop totally NA rows
# 
# AL_df_wide <- AL_df_wide[rowSums(is.na(AL_df_wide[,10:67]))!=58,]
# ## overwrite sporadic NAs
# AL_df_wide[is.na(AL_df_wide)] <- 0
# 
# #* spot check survey CAALs ----
# 
# ## females year 2007 Lbin 8, Age Bin 1 should have 1 obs and a total nsamp of 3
# mod17$condbase %>% 
#   filter(Fleet == 2 & Sex == 1 & Yr == 2007 & Lbin_lo  == 8 & Bin == 1) %>%
#   # mutate(obs2 = round(Obs*Nsamp_adj   ,0)) %>%
#   select(Yr, Sex, Nsamp_in      , Lbin_lo, Bin)
# subset(AL_df_wide,yr == 2007 & sex == 1 &  Lbin_lo  == 8) %>%
#   select(yr, sex, Nsamp, Lbin_lo, X1)
# 
# 
# ## males year 2007 36cm age 9 should have 1 obs and a total nsamp of 8
# mod17$condbase %>% filter(Fleet == 2 & Sex == 1 & Yr == 1990 
#                           # & Bin == 9
#                           & Lbin_lo  == 36 
# ) %>%
#   # mutate(obs2 = round(Obs*Nsamp_adj   ,0)) %>%
#   select(Yr, Sex, Nsamp_in   , Lbin_lo, Bin)
# subset(AL_df_wide,yr == 1990 & sex == 1 &  Lbin_lo  == 36) %>%
#   select(yr, sex, Nsamp, Lbin_lo, X9)
# 
# 
# mod17$condbase %>% filter(Fleet == 2 & Sex == 2 & Yr == 2017 
#                           # & Bin == 5
#                           # & Lbin_lo  == 24
# ) %>%
#   mutate(obs2 = round(Obs*Nsamp_in,0)) %>%
#   select(Yr, Sex, Nsamp_in, Lbin_lo, Bin, obs2)
# subset(AL_df_wide,yr == 2015 & sex == 2 &  Lbin_lo  == 24)
# 
# ## disable data before 1990;
# # the 1984 and 1987 GOA surveys used a different sampling scheme
# ## and data from those years have been taken out of lots of assessments at this point
# AL_df_wide$fleet[AL_df_wide$yr < 1990] <- -2
# 
# write.csv(AL_df_wide,file = here('data','comp',paste0(Sys.Date(),'-goa_CAAL_forSS.csv')),row.names = FALSE)
# 
# 
# 
# 
# 
# #** survey marginal ages [as ghost] ----
# #GOA marginal age comps (include so that you can see the ghosted fits in r4ss output)
# # source("C:\\GitProjects\\newsbss\\Get_GOA_Survey_AgeComposition.R")
# MyQuery<-paste0("SELECT GOA.AGECOMP_TOTAL.SURVEY,\n ",
#                 "GOA.AGECOMP_TOTAL.SURVEY_YEAR,\n ",
#                 "GOA.AGECOMP_TOTAL.SPECIES_CODE,\n ",
#                 "GOA.AGECOMP_TOTAL.AGE,\n ",
#                 "GOA.AGECOMP_TOTAL.SEX,\n ",
#                 "GOA.AGECOMP_TOTAL.AGEPOP\n",
#                 "FROM GOA.AGECOMP_TOTAL\n ",
#                 "WHERE GOA.AGECOMP_TOTAL.SURVEY     =", sp_area,"\n ",
#                 "AND GOA.AGECOMP_TOTAL.SPECIES_CODE = ",species)
# 
# 
# goa_marginal_age_raw <-sqlQuery(AFSC,MyQuery)
# # goa_marginal_age_raw <- goa_marginal_age_raw[goa_marginal_age_raw$AGE >=0 & goa_marginal_age_raw$SEX <3,] %>%
# #   rbind(., 
# #         ## load 2017, 2019 years from aging group (storm and ocean explorer are separate for 2019)
# #         
# #         rbind(read.csv(here('data','comp','raw','148201701103.csv')),
# #               read.csv(here('data','comp','raw','148201901103.csv')),
# #               read.csv(here('data','comp','raw','143201901103.csv'))) %>%
# #           mutate(SURVEY = 'GOA',SURVEY_YEAR = lubridate::year(date_collected), AGEPOP = NA) %>%
# #           select(SURVEY, SURVEY_YEAR, SPECIES_CODE = species, AGE = age, SEX = sex, AGEPOP) %>%
# #           filter(SEX != 3 & AGE >= 0))
# 
# write.csv(goa_marginal_age_raw,file = here('data','comp','raw',paste0(Sys.Date(),"-goa_marginal_agecomp_raw.csv")),row.names = FALSE)
# 
# #Bin ages
# goa_marginal_age0<-BIN_AGE_DATA(goa_marginal_age_raw,age_bins)
# 
# #Make into proportions that sum to 1 over males + females
# Ages.df<-aggregate(AGEPOP ~ SURVEY_YEAR + SEX + aBIN,goa_marginal_age0,sum)
# SumAges.df<-aggregate(AGEPOP ~ SURVEY_YEAR,goa_marginal_age0,sum)
# SumAges.df$SumOverA<-SumAges.df$AGEPOP
# SumAges.df<-subset(SumAges.df,select = -c(AGEPOP))
# 
# Ages.df<-merge(Ages.df,SumAges.df,all=TRUE)
# Ages.df$Proportion<-Ages.df$AGEPOP/Ages.df$SumOverA
# Ages.df<-subset(Ages.df,select = -c(SumOverA,AGEPOP))
# 
# #Flip to SS format
# Ages.df$SexNum<-as.numeric(Ages.df$SEX==1) #males
# Ages.df$SexAge<-as.numeric(paste0(Ages.df$SexNum,0,Ages.df$aBIN))
# 
# FlipComp1.df<-dcast(Ages.df,SURVEY_YEAR ~ SexAge,sum,value.var = "Proportion") #this isn't really summing anything, or shouldn't be. Just transposing. Function is not at all straightforward
# 
# nms<-c(age_bins,paste0("10",age_bins))
# Missing<-setdiff(nms,names(FlipComp1.df))
# FlipComp1.df[Missing]<-0
# FlipComp1.df<-FlipComp1.df[,c("SURVEY_YEAR",nms)]
# 
# 
# #Hauls come separately from above, make sure you're using the right ones
# goa_marginals <- merge(FlipComp1.df, nhauls, by.x = 'SURVEY_YEAR', by.y = 'YEAR') %>%
#   mutate(month = 7, fleet = -2, sex = 3, part = 0, ageerr = 1, Lbin_lo = -1, Lbin_hi = -1 ) %>%
#   select(yr = SURVEY_YEAR, month, fleet, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp= nhaul, everything())
# 
# # ## disable data before 1990;
# # # the 1984 and 1987 GOA surveys used a different sampling scheme
# # ## and data from those years have been taken out of lots of assessments at this point
# # goa_marginals$fleet[goa_marginals$yr < 1990] <- -2
# 
# write.csv(goa_marginals,file = here('data','comp',paste0(Sys.Date(),"-goa_marginal_agecomp_forSS.csv")),row.names = FALSE)


