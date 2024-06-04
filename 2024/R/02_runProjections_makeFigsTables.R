## Code to run SPM projections
## and REMA apportionment
require(dplyr)
require(tidyr)
require(ggplot2)
require(here)
require(tidyr)
require(rema)


## Execute spm module ----
## just ensure the values at the bottom of spm.dat match the catches you want
## no change to other inputs
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

## REMA apportionment ----
idx = 1; biomass_dat <- p <- list()
## admb/tmb bridge: fit each model individually 
for(i in c('Eastern','Western','Central')){
  ## 1 load data: could use `rwout.rep` from the ADMB version of this model.
  
  tempdir <- here::here('re',i)
  admb_re <- read_admb_re(filename = paste0(tempdir,'/rwout.rep'),
                          # optional label for the single biomass survey stratum
                          biomass_strata_names = i,
                          model_name = paste0("ADMB: BSAI FHS ",i))
  ## save the formatted biomass_dat for each region
  biomass_dat[[idx]] <- admb_re$biomass_dat 
  # 
  input <- prepare_rema_input(model_name = paste0("TMB: BSAI FHS ",i), 
                              admb_re  = admb_re)
  ## fit REMA
  m <- fit_rema(input) ##save each M separately
  compare <- compare_rema_models(rema_models = list(m),
                                 admb_re = admb_re,
                                 biomass_ylab = 'Biomass (t)')
  p[[idx]] <- compare$plots$biomass_by_strata+ggsidekick::theme_sleek()+theme(legend.position = 'bottom') +
    scale_y_continuous(limits = c(0,250000))
  idx = idx+1
}
p## shows that bridges are identical
require(patchwork)
ggsave(p[[2]] |p[[3]]  |  p[[1]]  , file = here::here('re','admb_re_bridge.png'), width = 12, height = 10, unit = 'in', dpi = 520)
## now fit them all at once (defaults to univariate structure, no info leakage among them)
input2 <- prepare_rema_input(model_name = paste0("TMB: BSAI FHS MULTIVAR"),
                             biomass_dat  = bind_rows(biomass_dat))
m2 <- fit_rema(input2) ##save each M separately
output <- tidy_rema(rema_model = m2)
# save(output, file = here('re',paste0(Sys.Date(),'-rema_output.rdata')))
# kableExtra::kable(output$parameter_estimates) 
# plots <- plot_rema(tidy_rema = output, biomass_ylab = 'Biomass (t)') # optional y-axis label
# plots$biomass_by_strata
# ggsave(plots$biomass_by_strata, file = here::here('re','rema_outs.png'), width = 12, height = 10, unit = 'in', dpi = 520)

# compare <- compare_rema_models(rema_models = list(m),
#                                admb_re = admb_re,
#                                biomass_ylab = 'Biomass (t)')
# compare$plots$biomass_by_strata
## final apportionment qtties; still need to include Eastern downscaling and ABCs
# kableExtra::kable(tail(output$proportion_biomass_by_strata, 3)) 


load("~/assessments/2022/goa-fhs-2022/RE/2022-10-03-rema_output.rdata") ## output

egfrac <- read.csv(here::here('data','survey','2021-10-01_Biomass Fractions in Eastern GOA.csv'))
props <- output$proportion_biomass_by_strata %>% 
  filter(year == 2021) %>% 
  mutate(WestYakutat = Eastern*egfrac$Western.Fraction,
         Southeast = Eastern*egfrac$Eastern.Fraction) %>%
  select(Western, Central, WestYakutat,Southeast)

sum(props)==1



rec_table <- read.csv(here::here('projection','rec_table.csv'))
abc23 <- as.numeric( rec_table[10,2]) 
abc24 <- as.numeric( rec_table[10,3]) 
apportionment2 <- apply(props, 2, FUN = function(x) round(x*c(abc23,abc24) )) %>%
  rbind( round(props*100,2) ,.) %>%
  data.frame() %>%
  mutate(Total = c("",abc23,abc24),
         Year = noquote(c("",year(Sys.Date())+1,year(Sys.Date())+2)),
         Quantity = c("Area Apportionment %", 
                      "ABC (t)",
                      "ABC (t)")) %>% select(Quantity, Year, everything())

## because the rounded totals don't perfectly sum to the ABC, locate the discrepancy and add to the highest area (per Chris)

diff23 <- abc23 - sum(apportionment2[2,3:6])
diff24 <- abc24 - sum(apportionment2[3,3:6])
apportionment2[2,4] <- apportionment2[2,4]+diff23
apportionment2[3,4] <- apportionment2[3,4]+diff24


abc23 - sum(apportionment2[2,3:6])==0
abc24 - sum(apportionment2[3,3:6]) ==0

write.csv(apportionment2,file = here::here('re',paste0(Sys.Date(),"-AreaAppportionment.csv")))




## Table 1 total Catch by area ----


## update these from 2018-previous year (values beforehand should show what's in base mod)
t12020 <- read.csv(here("data","2021-10-28-catch.csv")) %>% 
  group_by(YEAR,ZONE ) %>%
  summarise(zone_total = sum(TONS)) %>%
  select(YEAR, ZONE ,zone_total) %>%
  filter(YEAR > 2017) %>%
  tidyr::pivot_wider(., id_cols = 'YEAR', names_from = ZONE, values_from = zone_total) %>%
  mutate(total_catch = sum(CG,SE,WG,WY),
         EG = WY+SE) %>%
  select(YEAR, total_catch, WG,CG,EG) 

## get current by-area catches from weekly catches CSV

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
# catch_this_year <- 
weekly_catches %>% 
  filter(year==this_year) %>%
  mutate(ZONE = gsub(" ","",gsub(" Flathead Sole","",species))) %>%
  group_by(year, ZONE) %>%
  summarise(zone_total=sum(catch)) %>%
  tidyr::pivot_wider(., id_cols = 'year', 
                     names_from = ZONE, 
                     values_from = zone_total) %>%
  mutate(total_catch = sum(CGOA,GOASEOutside,GOAWestYakutat,WGOA),
         EG = GOAWestYakutat+GOASEOutside ) %>%
  select(YEAR = year, total_catch, WG=WGOA,CG=CGOA ,EG) %>% 
  rbind(t12020,.,) %>%
  write.csv(., file = here('data',paste0(Sys.Date(),"Table1Catch.csv")), row.names = FALSE)

#* Area apportionment ----

##* RE.dat inputs ----
## Run the RE model for each area-specific survey to get this
## year's estimates and use that to get proportions. I didn't
## actually run this in 2020 because there was no survey. For
## 2021 need to rework this chunk. CCM -10/2020
# index_by_area <- read.csv('data/index_by_area.csv') %>%
#   mutate(CV=sqrt(POPVAR)/POP)

iba <- read.csv(here('data','2021-09-30-index_by_area.csv')) %>%
  group_by(YEAR,AREA) %>%
  summarise(BIOM_MT = BIOM,
            CV_total=sqrt(BIOMVAR)/BIOM ) %>%
  filter(YEAR > 2010)

iba %>% filter(AREA == 'CENTRAL GOA') %>%  View()
write.csv(., 
          here('re','Central',paste0(Sys.Date(),'-values_for_redat.csv') ),
          row.names=FALSE)

iba %>% filter(AREA == 'EASTERN GOA') %>%
  write.csv(., 
            here('re','Eastern',paste0(Sys.Date(),'-values_for_redat.csv') ),
            row.names=FALSE)

iba %>% filter(AREA == 'WESTERN GOA') %>% View()
write.csv(., 
          here('re','WESTERN',paste0(Sys.Date(),'-values_for_redat.csv') ),
          row.names=FALSE)


iba %>% 
  tidyr::pivot_wider(.,names_from = AREA, values_from = c(BIOM_MT, CV_total))

##* wrangle RE outputs ----

rwout0 <- list.files(here('re'), pattern = paste('rwout'), full.names = T, recursive = T)
rwout <- rwout0[!grepl('2019',rwout0 )] ## grab new runs (not 2019)


read_rwout <- function(x){

  rawl <-  scan(x,
       skip = 7,
       what = as.list(rep("",length(1984:this_year)+1)), 
       flush = TRUE) %>% 
    data.frame()

  ## label columns and rows and drop
  rownames(rawl) <- c('Year',as.matrix(rawl[,ncol(rawl)]))[1:8]
  rawl<-rawl[1:(ncol(rawl)-1)]
  names(rawl) <- as.matrix(rawl[1, ])
  rawl <- rawl[-1, ]
  rawl <- rawl[1:6,]
  ## biomA = estimated biomass (natural scale)
  currBio <-  rawl %>% tibble::rownames_to_column() %>%  
    pivot_longer(-rowname) %>% 
    pivot_wider(names_from=rowname, values_from=value) %>%
    filter(name == this_year) %>%
    select(biomA)
  currBio <- as.numeric(currBio)
  
  names(currBio) = paste(basename(dirname(x)))
  
  
  return(currBio)
}

## by hand inspection, biomA should actually be
# CENTRAL = 94280.3
# EASTERN = 14492.6
# WESTERN = 82104.9

dt0 <- lapply(rwout, read_rwout) %>% 
  unlist() %>%
  data.frame() 

names(dt0) <- 'cb'

## these are the apportionments by 3 areas; still need to dwnslale EGOA
mgmtf <- dt0 %>% 
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  mutate(sv = sum(value)) %>%
  group_by(rowname) %>%
  summarise(propBio = value/sv) %>% 
  pivot_wider(names_from = rowname, values_from = c(propBio)) 



## EGOA: Eastern goes to SE, Western goes to Yakutat
egfrac <- read.csv(here('data','2021-10-01_Biomass Fractions in Eastern GOA.csv'))

apportionment <- mgmtf %>% 
  mutate(WestYakutat = Eastern*egfrac$Western.Fraction,
         Southeast = Eastern*egfrac$Eastern.Fraction) %>%
  select(Western, Central, WestYakutat,Southeast)

sum(apportionment)==1


abc22 <- as.numeric( rec_table[10,2])*apportionment
abc23 <- as.numeric( rec_table[10,3])*apportionment

abc22 <- as.numeric( 40175)*apportionment
abc23 <- as.numeric(40046)*apportionment

apportionment2 <- rbind(apportionment,abc22,abc23)
write.csv(apportionment2,file = here('re',paste0(Sys.Date(),"-AreaAppportionment.csv")))


## table of indices x area ----
iba <- read.csv(here('data','2021-09-30-index_by_area.csv')) %>%
  group_by(YEAR,AREA) %>%
  summarise(BIOM_MT = BIOM,
            CV=sqrt(BIOMVAR)/BIOM ) 

index <- read.csv(here('data','2021-09-30-index.csv')) %>%
  group_by(YEAR) %>%
  summarise(BIOM_MT = BIOM,
            CV_total=sqrt(BIOMVAR)/BIOM)

iba %>% 
  tidyr::pivot_wider(.,names_from = AREA, values_from = c(BIOM_MT, CV)) %>%
  merge(., index, by = 'YEAR') %>%
  select(YEAR, BIOM_MT, CV_total, everything()) %>%
  write.csv(., 
            here('DATA',paste0(Sys.Date(),'-table2_indices_area.csv') ),
            row.names=FALSE)




## plots ----

#* Projection plots ----
## notes from CMM
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
pdt <- data.frame(read.table("bigfile.out", header=TRUE))
pdt.long <- pivot_longer(pdt, cols=c(-Alternative, -Spp, -Yr), names_to='metric') %>%
  mutate(Alternative=factor(Alternative)) %>% group_by(Yr, Alternative, metric) %>%
  summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')
g <- ggplot(pdt.long, aes(Yr,  med, ymin=lwr, ymax=upr, fill=Alternative, color=Alternative)) +
  facet_wrap('metric', scales='free_y') + ylim(0,NA) +
  geom_ribbon(alpha=.4) + theme_bw() +
  labs(x='Year', y='Estimated 80% CI')


## load previous BASE model 
require(r4ss)
# base17 <- SS_output(here('model_runs','run01_original_ss324'))
base17mod <- SS_output(here('model_runs','run01_original_ss324_smryage=3'))
# SSplotComparisons(SSsummarize(list(base17,base17mod) ) )


#* Fig 1 catch/totbio plot ----
## per report.xlsx/Fig1, looks like biomass is from the assessment thru 2016 then values from proj
## the figure caption indicates these are for 3+ but the model was run using age 0 as the summary biomass
## had to rerun with the summary age updated
base17mod$timeseries %>% select(Yr, Bio_smry) %>% filter(Yr == 1978) ## should be 141306
base17mod$timeseries %>% select(Yr, Bio_smry) %>% filter(Yr == 1995) ## should be 216101


fig1a <- base17mod$timeseries %>% select(Yr, Bio_smry) %>%
  merge(.,base17mod$catch %>% select(Yr, Obs), by = 'Yr') %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)

## projection catch values in spp_catch, use Tot_biom from pdt

## summarise values in pdt
sppcatch <- read.table(here('projection','projections','spp_catch.dat'),
                       skip = 24) %>%
  data.frame() %>% select(Yr = V1, catch = V2)


fig1b <- data.frame(Yr = seq(2017,2023,1),
                    Bio_smry = pdt %>% filter(Yr < 2024) %>% group_by(Yr) %>%
                      summarise(Bio_smry = 1000*round(mean(Tot_biom),2)) %>% 
                      select(Bio_smry) ,
                    Obs = sppcatch$catch) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)


fig1 <- rbind(fig1a, fig1b)


## plot with diff colors for extrapolated and forecasted catches
ggplot(subset(fig1, Yr < 2018), aes(x = Yr, y = catch_over_biomass)) +
  geom_line(lwd = 1, col = 'dodgerblue2') +
  geom_line(data = subset(fig1, Yr > 2016 & Yr < 2021),
            lwd = 1, col = 'grey') +
  geom_line(data = subset(fig1, Yr > 2019),
            lwd = 1, linetype = 'dotted',  col = 'grey') +
  scale_x_continuous(labels = seq(1978,2020,10), 
                     breaks = seq(1978,2021,10))+
  labs(x = 'Year', y = 'Catch/Summary Biomass (age 3+)')+
  ggsidekick::theme_sleek()

ggsave(last_plot(), height = 4, width = 6, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-Fig1_catchvsbio.png')))

#* Design-based index plot ----
index <- read.csv(here('data','2021-09-30-index.csv')) %>%
  group_by(YEAR) %>%
  summarise(BIOM_MT = BIOM/1000,
            CV_total=sqrt(BIOMVAR)/BIOM,
            uci = BIOM_MT +CV_total*BIOM_MT, 
            lci = BIOM_MT -CV_total*BIOM_MT )

index %>% filter(  YEAR != 2019) %>% 
  summarise(mb=mean(BIOM), sdb = sd(BIOM),
            CV_total=sqrt(BIOMVAR)/BIOM
            ) %>% 
  mutate(mb+sdb)


ggplot(index, aes(x = YEAR, y = BIOM_MT)) +
  geom_line(lwd = 1, col = 'grey77') +
  geom_point() +
  geom_point(data = subset(index, YEAR >2018), color = 'blue') +
  # scale_x_continuous(labels = seq(1984,2021,5), 
  #                    breaks = seq(1984,2021,5))+
  scale_x_continuous(labels = seq(1983,2021,2),
                     breaks = seq(1983,2021,2))+
  scale_y_continuous(limits = c(0,350) ) +
  labs(x = 'Year', y = 'Survey Biomass (1000 mt)')+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  # geom_hline(yintercept =154030.8/1000, linetype = 'dashed') +
  # geom_text(x = 2018.5, y = 180, label = '-1SD', color = 'blue') +
  # geom_hline(yintercept =186832.7/1000, linetype = 'dotted', color = 'blue')+
  # geom_text(x = 2018.5, y = 260, label = '+1SD', color = 'blue') +
  # geom_hline(yintercept =252436.7/1000, linetype = 'dotted', color = 'blue')+
  ggsidekick::theme_sleek()

ggsave(last_plot(), height = 6, width = 10, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_wCVs.png')))

#* design based survey index by area ----

ggplot(iba, aes(x = YEAR, y= BIOM_MT, color = AREA, fill = AREA)) +
  geom_line(lwd = 0.8) +
  geom_ribbon(aes(ymin = BIOM_MT-BIOM_MT*CV,
                  ymax= BIOM_MT+BIOM_MT*CV), alpha = 0.2)+
  scale_color_manual(labels = c('CGOA','EGOA','WGOA'),
                     values = c('grey22','dodgerblue4','goldenrod'))+
  scale_fill_manual(labels = c('CGOA','EGOA','WGOA'),
                    values = c('grey22','dodgerblue4','goldenrod'))+
  labs(x = 'Year', y = 'Survey Biomass (mt)',
       fill = 'Area', col = 'Area')+
  ggsidekick::theme_sleek(base_size = 16)

ggsave(last_plot(), height = 4, width = 6, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_byArea_wCVs.png')))

#* comparison of design-based survey and VAST
## dwnlded table_for_ss3.csv here: https://drive.google.com/drive/folders/1ZnF47okW-WFV7akAyAwkxhU3Sp4PJmG-
## note these are not necessairly production ready, just the latest hindcast.
vast <- read.csv(here('data','table_for_ss3-thru2021.csv')) %>%
  mutate(src = 'VAST',
         value=Estimate_metric_tons/1000,
         lci = (Estimate_metric_tons -1.96*SD_mt)/1000 ,
         uci =  (Estimate_metric_tons +1.96*SD_mt)/1000 ) %>%
  select(Year, value, lci, uci, src) %>%
  rbind(., index %>% mutate(src = 'design-based') %>%   
          select(Year=YEAR, value = BIOM_MT , lci, uci, src)) %>%
  filter(value > 0)

ggplot(vast, aes(x = Year, y = value, color = src, fill = src, group = src)) +
  geom_line(lwd = 1) +
  ggsidekick::theme_sleek()+
  theme(legend.text = element_text(size = 14), 
        legend.position = 'bottom',
        axis.text = element_text(size = 14),
        axis.title  = element_text(size = 14)) + 
  scale_color_manual(values = c('grey22','goldenrod'))+
  scale_fill_manual(values = c('grey22','goldenrod'))+
  # geom_point(data = subset(index, Year == 2019), color = 'blue') +
  scale_x_continuous(labels = seq(1980,2025,5),breaks = seq(1980,2025,5))+
  scale_y_continuous(limits = c(0,350) ) +
  labs(x = 'Year', y = 'Survey Biomass (mt)', color = '',fill = '')+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)

ggsave(last_plot(), height = 4, width = 6, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_VASTvsDesign.png')))

#* Catches, TACS, ABCs plot----

## presentation slide with yearly catches << TACs < ABCs thru time 

cbpal <- c("#999999", "#E69F00", "#56B4E9", "#009E73" ,"#F0E442", "#0072B2", "#D55E00" )
cbpal <- c("#E69F00", "#56B4E9", "#009E73" ,'black','grey66' )
cbpal <- c("#E69F00", "#56B4E9", 'black','grey66' )

catch <- read.csv(here('data','2021-10-28-catch.csv'))
mgmt0 <- read.csv(here('data','2021-09-20-GOA_flathead_harvest_reformat.csv'), header = F)[,-1]
mgmt1 <- rbind(mgmt0[1:2,],as.numeric(gsub(",", "", mgmt0[3,]))) ## make harvest specs numeric (remove comma)
mgmt<- mgmt1%>%
  t() %>%  data.frame(.) %>%
  pivot_wider(.,names_from = 'X2', values_from = 'X3') %>%
  mutate(Yr = X1) %>%
  select(Yr, TAC, ABC, OFL)

## extrapolated catches
xtrayrs <- data.frame(Yr = sppcatch$Yr,
                      Catch = sppcatch$catch)

merge(mgmt, base17mod$catch %>% 
        select(Yr, Catch = Obs),
      by ='Yr', all.x = TRUE)  %>%
  merge(., xtrayrs, by = 'Yr',all = T) %>%
  mutate(c1= Catch.x, c2= Catch.y) %>%
  data.frame(.) %>%
  select(-Catch.x, -Catch.y) %>%
  reshape2::melt(., id = 'Yr') %>%
  mutate(value = as.numeric(value), Yr = as.numeric(Yr)) %>%
  filter(variable != 'OFL') %>%
  ggplot(., aes(x = Yr,y = value, color = variable, group = variable)) +
  geom_line(lwd = 1.1) +
  ggsidekick::theme_sleek() +
  theme(legend.text = element_text(size = 14), legend.position = 'bottom',
        axis.text = element_text(size = 14),axis.title  = element_text(size = 14)) +
  scale_x_continuous(limits = c(1990,2025), 
                     breaks =  seq(1990,2025,5),
                     labels = seq(1990,2025,5)) +
  # scale_color_manual(values = cbpal,
  #                    labels = c('TAC','ABC','OFL',"Catches in Base Model", "Catches used in Projections" ))+
  # annotate('text', x = rep(2020.5,4), y = c(47500,40000,30000, 4500),
  #          label = c('OFL','ABC','TAC','Catches'),
  #          color = c(cbpal[3:1],'grey44'), size = 6)+
  scale_color_manual(values = cbpal,
                     labels = c('TAC','ABC',"Catches in Base Model", "Catches used in Projections" ))+
  annotate('text', x = rep(2020.5,3), y = c(40000,30000, 4500),
           label = c('ABC','TAC','Catches'),
           color = c(cbpal[2:1],'grey44'), size = 6)+
  labs(x = 'Year', y = 'Catch or Harvest Spex (t)', color = '')

ggsave(last_plot(), file = here("figs","harvest_spex_vs_catch.png"),
       height = 7, width  = 12, unit = 'in', dpi = 520)

## age comps for now ----
## checkout get_agecomps_fishery in BSAI-flathead/2020_files/data
acomps0 <- readRDS("~/assessments/2021/GOA-flathead/data/2021-11-12-fishery_agecomps.RDS") %>% filter(!is.na(AGE))
lcomps0 <- readRDS("~/assessments/2021/GOA-flathead/data/2021-11-12_fishery_lencomps.RDS")



anyear <- acomps0 %>%
  select(AGE, SEX, YEAR) %>%
  group_by(YEAR,SEX) %>%
  summarise(n=n())


acomps0 %>%
  select(AGE, SEX, YEAR) %>%
  group_by(YEAR, AGE,SEX) %>% 
  summarise(nage = n()) %>%
  merge(., anyear, by = c('YEAR','SEX'), all.x = T) %>% head()

