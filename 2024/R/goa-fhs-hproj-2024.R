## Standalone script for 2024 GOA_FHS Harvest Projection
## This will download & generate everything used to populate the SAFE
## M Kapur maia.kapur@noaa.gov Jun 2024

# Packages and RODBC setup ---- 
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

year <- 2024
## load previous full assessment
mod <- r4ss::SS_output(here::here('2022','model_runs','m0_8-newMI-biasAdj'))

## only need to do the following once:
# afscdata::setup_folders(year) 
# afscdata::goa_fhs(year,off_yr = TRUE)
## manually download any needed years of weekly catches from 
## https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish
