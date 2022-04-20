
## M Sosa Kapur building upon C McGilliard and C Monnahan
# maia.kapur@noaa.gov
# Spring 2022

## NOTES ##
## This script is to extract and clean data for 2022 GOA FHS Update assessment.
## Since this is my first time doing this assessment any extraneous data sources
## will be used as sensitivities only.
## Includes relevant visualizations for SAFE report.
## All extractions are saved as a "raw" format (what comes out of the SQLquery, dated)
## as well as in the finalized SS3v3.30+ format, which is made using code [suffix '_forSS'].
## Using newsbss package from M:\Monnahan\newsbss; this hasn't been updated since 15 Sep 2021
## Guidance from the GOA_Flathead_readme.md provided by Carey. She indicates which newsbss function
## applies for each data component. I did not find those functions to be standalone (they've been modified
## for dover/rex sole) so I largely copied and pasted into here, and streamlined where applicable.

# Packages and RODBC setup ----
require(RODBC)
require(dplyr)
require(tidyverse)
require(here)
require(ggplot2); require(ggsidekick)
require(r4ss)
require(reshape2)
require(rstudioapi) ## enables masking of RODBC name, password

newsbssdir <- "C:/Users/maia.kapur/Work/assessments/newsbss/"

username_AFSC <- showPrompt(title="Username", message="Enter your AFSC username:", default="")
password_AFSC <- askForPassword(prompt="Enter your AFSC password:")
AFSC <- odbcConnect("AFSC",username_AFSC,password_AFSC,  believeNRows = FALSE)

username_AKFIN <- showPrompt(title="Username", message="Enter your AKFIN username:", default="")
password_AKFIN <- askForPassword(prompt="Enter your AKFIN password:")
AKFIN <- odbcConnect("AKFIN",username_AKFIN,password_AKFIN,  believeNRows = FALSE)

species <- 10130 #throughout make sure the SpeciesCode = 10130 for the survey, 103 for observer data
sp_area <- "'GOA'"
fyear <- 2022

## load last model
mod17 <-  SS_output(here('model_runs','2017-mod'))
SSplotData(mod17) ## we need one fishery of catches, one survey, 2x Lcomps, and 1x CAALS

# Catches ----


## manually download any needed years of weekly catches from 
## https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish
#* dwnld catch ----
source(paste0(newsbssdir,"functions/get_catch.R"))
fsh_sp_area <- "'CG','SE','WG','WY','EY'"
message("Querying AKFIN to get catch..")
catch0 <- GET_CATCH(fsh_sp_area=fsh_sp_area,
                   fsh_sp_label="'FSOL'",
                   final_year=fyear,
                   ADD_OLD_FILE=FALSE)$CATCH
catch0 <- arrange(catch0, YEAR, ZONE, GEAR1)
write.csv(catch0, file=here('data','catch',paste0(Sys.Date(),'-catch-raw.csv') ), row.names=FALSE)

catchA <- catch0 %>%
  select(YEAR, ZONE, TONS) %>%
  group_by(YEAR, ZONE) %>%
  summarise(STONS = sum(TONS)) %>%
  tidyr::pivot_wider(., id_cols = YEAR, names_from = ZONE, values_from = STONS) %>%
  mutate(EGULF = EY+SE, 
         WGULF = WG +WY,
         TTONS = sum(WGULF, CG, EGULF)) %>%
  select(YEAR, TTONS, WGULF, CG, EGULF) 

write.csv(catchA, file=here('data','catch',paste0(Sys.Date(),'-catch-byArea.csv') ), row.names=FALSE)

#* spot check catch ----
## even though the document mentions Trawl gear, we are using the sum of all gears.
subset(mod17$catch, Yr == 2016) %>% select(Obs);with(subset( catch0, YEAR == 2016), sum(TONS))
subset(mod17$catch, Yr == 2011) %>% select(Obs);with(subset( catch0, YEAR == 2011), sum(TONS))
subset(mod17$catch, Yr == 2000) %>% select(Obs);with(subset( catch0, YEAR == 2000), sum(TONS))

#* format catch for SS ----
## data from 1978-1990 seem to be from a different source. paste from assessment.
catch <- catch0 %>% 
  group_by(YEAR) %>% 
  summarise(catch = sum(TONS)) %>%
  merge(., 
  data.frame(cbind(YEAR = mod17$catch$Yr,
                          TONS = mod17$catch$Obs)) %>%
  filter(YEAR < 1991),
  by = 'YEAR', all = T) %>%
  mutate(catch_mt = ifelse(is.na(catch), round(TONS,2), round(catch,2)),
         catch_se = 0.01,
         seas = 1,
         fleet = 1) %>%
  select( -catch   , -TONS) %>%
  select(yr = YEAR, seas , fleet , catch_mt , catch_se )
write.csv(catch, file=here('data','catch',paste0(Sys.Date(),'-catch_forSS.csv') ), row.names=FALSE)


#* plot catch in mt ----
catch %>%  
  ggplot(., aes(x = yr, y = catch_mt))+
  theme_sleek(base_size = 14) +
  theme(legend.position = c(0.9,0.9),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank())+
  geom_line(lwd = 1, col = 'grey44') +
  scale_x_continuous(limits = c(1975, 2025), 
                     breaks = seq(1980,2020,10),
                     labels = seq(1980,2020,10)) +
  labs(y = 'Landings (mt)', x= 'Year')
ggsave(last_plot(),
       file = here('figs','landings.png'),
       height = 4, width = 6, dpi = 520)

#* misc catch plots/eda ----

## trawl fishery dwarfs all others; so they're likely just left in for ease
catch0 %>% group_by(GEAR, YEAR) %>% summarise(mt = mean(TONS)) %>%
  ggplot(., aes(x = YEAR, y = mt, fill = GEAR)) +
  geom_bar(position = 'dodge', stat = 'identity')

# Survey Biomass ----
#* dwnld srv ----
test <- paste0("SELECT GOA.BIOMASS_TOTAL.YEAR as YEAR,\n ",
               "GOA.BIOMASS_TOTAL.TOTAL_BIOMASS as BIOM,\n ",
               "GOA.BIOMASS_TOTAL.TOTAL_POP as POP,\n ",
               "GOA.BIOMASS_TOTAL.BIOMASS_VAR as BIOMVAR,\n ",
               "GOA.BIOMASS_TOTAL.POP_VAR as POPVAR,\n ",
               "GOA.BIOMASS_TOTAL.HAUL_COUNT as NUMHAULS,\n ",
               "GOA.BIOMASS_TOTAL.CATCH_COUNT as NUMCAUGHT\n ",
               "FROM GOA.BIOMASS_TOTAL\n ",
               "WHERE GOA.BIOMASS_TOTAL.SPECIES_CODE in (",species,")\n ",
               "ORDER BY GOA.BIOMASS_TOTAL.YEAR")
index0 <- sqlQuery(AFSC, test)
if(!is.data.frame(index)) stop("Failed to query GOA survey data")
write.csv(index0, here('data','survey',paste0(Sys.Date(),'-index_raw.csv') ),row.names=FALSE)

## Survey data by area (for viz) 
message("Querying survey biomass data...")
test <- paste0("SELECT GOA.BIOMASS_AREA.YEAR as YEAR,\n ",
               "GOA.BIOMASS_AREA.REGULATORY_AREA_NAME as AREA,\n",
               "GOA.BIOMASS_AREA.AREA_BIOMASS as BIOM,\n ",
               "GOA.BIOMASS_AREA.AREA_POP as POP,\n ",
               "GOA.BIOMASS_AREA.BIOMASS_VAR as BIOMVAR,\n ",
               "GOA.BIOMASS_AREA.POP_VAR as POPVAR,\n ",
               "GOA.BIOMASS_AREA.HAUL_COUNT as NUMHAULS,\n ",
               "GOA.BIOMASS_AREA.CATCH_COUNT as NUMCAUGHT\n ",
               "FROM GOA.BIOMASS_AREA\n ",
               "WHERE GOA.BIOMASS_AREA.SPECIES_CODE in (",species,")\n ",
               "ORDER BY GOA.BIOMASS_AREA.YEAR")
index_by_area <- sqlQuery(AFSC, test)
if(!is.data.frame(index_by_area))
  stop("Failed to query GOA survey data by area")
write.csv(index_by_area, here('data','survey',paste0(Sys.Date(),'-index_byArea.csv') ), row.names=FALSE)

#* format srv for SS ----
## this should be in Tons
index <- index0 %>% group_by(YEAR) %>%
  summarise(BIOM_T = BIOM,
            CV_total=sqrt(BIOMVAR)/BIOM,
            uci = BIOM_T +CV_total*BIOM_T, 
            lci = BIOM_T -CV_total*BIOM_T ) %>%
  mutate(month =1, fleet = 2) %>%
  select(yr = YEAR, month, fleet, obs = BIOM_T, sterr  = CV_total)
write.csv(index, here('data','survey',paste0(Sys.Date(),'-index_forSS.csv') ),row.names=FALSE)

#* survey biomass spot check ----
subset(mod17$cpue, Yr == 2015) %>% select(Obs);subset(index, yr == 2015) %>% select(obs)

subset(mod17$cpue, Yr == 2011) %>% select(Obs);subset(index, yr == 2011) %>% select(obs)

subset(mod17$cpue, Yr == 2001) %>% select(Obs);subset(index, yr == 2001) %>% select(obs)
#* plot srv in mt ----
## design-based
ggplot(index, aes(x = yr, y = obs/1000)) +
  theme_sleek(base_size = 14) +
  theme(legend.position = c(0.9,0.9),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank())+
  geom_line(lwd = 1, col = 'grey77') +
  geom_point() +
  scale_y_continuous(limits = c(0,350) ) +
  scale_x_continuous(limits = c(1984, 2025),
                     breaks = seq(1980,2020,10),
                     labels = seq(1980,2020,10)) +
  labs(x = 'Year', y = 'Survey Biomass (mt)')+
  geom_ribbon(aes(ymin = (obs-sterr*obs)/1000, ymax = (obs+sterr*obs)/1000), alpha = 0.2)
ggsave(last_plot(),
       file = here('figs','design-based-indices.png'),
       height = 4, width = 6, dpi = 520)

## design based with VAST
vast <- read.csv(here('data','survey','hippoglossoides_elassodon',
'table_for_ss3.csv')) %>%
  mutate(src = 'Model-Based (VAST)',
         value=Estimate_metric_tons/1000,
         lci = (Estimate_metric_tons -1.96*SD_mt)/1000 ,
         uci =  (Estimate_metric_tons +1.96*SD_mt)/1000 ) %>%
  select(Year, value, lci, uci, src) %>%
  rbind(., index %>% 
          mutate(src = 'Design-Based',
                 value = obs/1000,
                 lci = (obs-sterr*obs)/1000,
                 uci = (obs+sterr*obs)/1000) %>%   
          select(Year=yr, value  , lci, uci, src)) %>%
  filter(value > 0)


ggplot(vast, aes(x = Year, y = value, color = src, fill = src, group = src)) +
  geom_line(lwd = 1) +
  theme_sleek(base_size = 14) +
  theme(legend.position = c(0.75,0.9),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank())+
  scale_color_manual(values = c('#015b58','goldenrod'))+
  scale_fill_manual(values = c('#015b58','goldenrod'))+
  scale_x_continuous(labels = seq(1980,2025,5),breaks = seq(1980,2025,5))+
  scale_y_continuous(limits = c(0,350) ) +
  labs(x = 'Year', y = 'Survey Biomass (mt)', color = '',fill = '')+
  geom_ribbon(aes(ymin = lci, ymax = uci), color = NA, alpha = 0.2)

ggsave(last_plot(), height = 4, width = 6, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_VASTvsDesign.png')))

## design based survey in mt by area
iba <- index_by_area  %>%
  group_by(YEAR,AREA) %>%
  summarise(BIOM_T = BIOM,
            CV_total=sqrt(BIOMVAR)/BIOM )  

ggplot(iba, aes(x = YEAR, y= BIOM_T/1000, color = AREA, fill = AREA)) +
  geom_line(lwd = 0.8) +
  theme_sleek(base_size = 14) +
  theme(legend.position = c(0.9,0.9),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank())+
  geom_ribbon(aes(ymin = (BIOM_T-BIOM_T*CV_total)/1000,
                  ymax= (BIOM_T+BIOM_T*CV_total)/1000), 
              alpha = 0.2, color = NA)+
  scale_color_manual(labels = c('CGOA','EGOA','WGOA'),
                     values = c('#015b58','#984e73','goldenrod'))+
  scale_fill_manual(labels = c('CGOA','EGOA','WGOA'),
                    values = c('#015b58','#984e73','goldenrod'))+
  scale_x_continuous(limits = c(1984, 2025), 
                     breaks = seq(1980,2020,10),
                     labels = seq(1980,2020,10)) +
  labs(x = 'Year', y = 'Survey Biomass (mt)',
       fill = 'Survey Area', col = 'Survey Area')

ggsave(last_plot(), height = 4, width = 6, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_byArea_wCVs.png')))

#* misc srv plots/eda ----
## what's up with year 2001?

ggplot(index, aes(x = yr, y = obs))+
  geom_line(col = 'red') +
  geom_line(data = mod17$cpue, aes(x = Yr, y = Obs))


# Comps ----
length.bins <- seq(6,70,2) 
source(paste0(newsbssdir,"functions/BIN_LEN_DATA.R"))
#* length comps----
#** fishery length comps [1982-1988 disabled] ----
#** survey length comps ----
## In the .dat file these are presented as run-on frequencies for males and females separately. Based extraction code
## on source(paste0(newsbssdir,"inputs_piecebypiece/Get_GOA_Survey_Length_and_Age_Comp/Get_GOA_Length_Comps.R"))

## comp data query
l.query<-paste0("SELECT GOA.SIZECOMP_TOTAL.SURVEY,\n ",
                "GOA.SIZECOMP_TOTAL.YEAR,\n ",
                "GOA.SIZECOMP_TOTAL.SPECIES_CODE,\n ",
                "GOA.SIZECOMP_TOTAL.SUMMARY_AREA,\n ",
                "GOA.SIZECOMP_TOTAL.LENGTH,\n ",
                "GOA.SIZECOMP_TOTAL.MALES,\n ",
                "GOA.SIZECOMP_TOTAL.FEMALES,\n ",
                "GOA.SIZECOMP_TOTAL.UNSEXED,\n ",
                "GOA.SIZECOMP_TOTAL.TOTAL\n ",
                "FROM GOA.SIZECOMP_TOTAL\n ",
                "WHERE GOA.SIZECOMP_TOTAL.SURVEY     = 'GOA'\n ",
                "AND GOA.SIZECOMP_TOTAL.SPECIES_CODE = ",species)
Length_df<- sqlQuery(AFSC,l.query)
write.csv(Length_df,file = here('data','comp',paste0(Sys.Date(),'-goa_lengthcomp_raw.csv')),row.names = FALSE)

## nsamp query
## The survey sample size (nhauls) was derived using W Paullson's code.
## Carey indicates that lengths only come from "good" hauls anyway,
## though I notice these values vary slightly (about 5 hauls) from what was used in 2017.
## source("C:\\GitProjects\\newsbss\\Get_GOA_Survey_Length_and_Age_Comp\\Get_Survey_Length_Age_Stats.R")
hauljoin_query <-paste0("SELECT c.YEAR,\n ",
                    "l.SPECIES_CODE,\n ",
                    "c.HAULJOIN,\n ",
                    "l.LENGTH,\n ",
                    "l.FREQUENCY,\n ",
                    "l.SEX\n ",
                    "FROM racebase.length l,\n ",
                    "goa.cpue c\n ",
                    "WHERE l.HAULJOIN     = c.HAULJOIN\n ",
                    "AND l.CATCHJOIN      = c.CATCHJOIN\n ",
                    "AND l.REGION         = c.SURVEY\n ",
                    "AND (l.SPECIES_CODE IN (",species,")\n ",
                    "AND l.REGION         = ",sp_area,")\n ",
                    "GROUP BY c.YEAR,\n ",
                    "  l.SPECIES_CODE,\n ",
                    "  c.HAULJOIN,\n ",
                    "  l.LENGTH,\n ",
                    "  l.FREQUENCY,\n ",
                    "  l.SEX\n ",
                    "ORDER BY l.species_code,\n ",
                    "  c.YEAR")

hauljoin <- sqlQuery(AFSC,hauljoin_query)

## for each year, sum the nhauls. Wayne's code also does this for males and females, but this is all  we need for SS.
nhauls <- hauljoin %>% 
  group_by(YEAR) %>%
  summarise(nhaul = n_distinct(ID))


## In Carey's code line 78 she aggregates (sums) within sex-year-bins, and uses the total annual number of samples
## as the denominator (should it not be the total number of samples of that sex?) 

length_df_long <- Length_df %>%
  mutate(LENGTH = LENGTH/10) %>% ## convert to cm
  select(YEAR,LENGTH,MALES,FEMALES) %>%
  melt(., id = c("YEAR","LENGTH")) %>%
  BIN_LEN_DATA(.,length.bins) %>%
  group_by(YEAR, variable, BIN) %>% 
  ## combine observations within length bins
  summarise(nobs = sum(value))

## get total number of samples in each year/sex/bin combo
length_df_long1 <- merge(length_df_long,
                         length_df_long %>% 
                           group_by(YEAR) %>% 
                           ## denominator is total number of samples this year
                           summarise(nsamp = sum(nobs)),
                         by = c('YEAR')) %>%
  # filter(YEAR == 1984 & BIN == 24)
  ## calculate frequencies 
  mutate(freq = nobs/nsamp,
         variable = ifelse(variable == 'FEMALES', 1, 2)) %>%
  select(-nobs, -nsamp) %>% 
  # ## fill missing year combos
  tidyr::complete(., YEAR, variable, BIN) %>%
  arrange(., YEAR, BIN, variable) %>%
    mutate(freq = ifelse(is.na(freq),0,freq)) 

frontmatter <- data.frame(yr = unique(length_df_long1$YEAR), month = 7, fleet = 2, sex = 3, part = 0, Nsamp = nhauls$nhaul)

survey_length_comps <- cbind(frontmatter,
      length_df_long1 %>%
  filter(variable == 1) %>%
  pivot_wider(., id_cols = YEAR, names_from = BIN, values_from = freq) %>%
  select(-YEAR),
  length_df_long1 %>%
    filter(variable == 2) %>%
    pivot_wider(., id_cols = YEAR, names_from = BIN, values_from = freq) %>%
    select(-YEAR))

#* spot check survey lcomps ----
## should be within rounding range
mod17$lendbase %>% 
  filter(Fleet == 2 & Yr == 2001 & Bin == 10 & Sex == 1) %>% 
  select(Obs) %>% round(.,4) ==
  round(survey_length_comps[7,'10'],4)

mod17$lendbase %>% 
  filter(Fleet == 2 & Yr == 2007 & Bin == 26 & Sex == 1) %>% 
  select(Obs) %>% round(.,4) ==
  round(survey_length_comps[10,'26'],4)

write.csv(survey_length_comps,file = here('data','comp',paste0(Sys.Date(),'-goa_lengthcomp_forSS.csv')),row.names = FALSE)
 
#* age comps----
#** survey CAALs ----
#** survey marginal ages [not used] ----
