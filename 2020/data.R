library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

source("../../colesbss/functions.R")
## Setup the network connections
source('../../odbc_connect.R')

species <- 10130
sp_area <- "'GOA'"

## ----- Catch from AKFIN -----
## the FMP area for this stock
## This is from the query I got from Carey. According to Jean Lee
## from NPFMC the eastern gulf is the sum of SE, WY, and EY. I
## checked and this matches the catches from AKFIN
## fsh_sp_area <- "'CG','PWSI','SE','SEI','WG','WY'"
## the GOA FMP sub-areas in the COUNCIL.COMPREHENSIVE_BLEND_CA database table
fsh_sp_area <- "'CG','SE','WG','WY','EY'"
message("Querying AKFIN to get catch..")
catch <- GET_CATCH(fsh_sp_area=fsh_sp_area,
                   fsh_sp_label="'FSOL'",
                   final_year=2020,
                   ADD_OLD_FILE=FALSE)$CATCH
catch <- arrange(catch, YEAR, ZONE, GEAR1)
write.csv(catch, file='data/catch.csv', row.names=FALSE)


### Weekly catch  by year was downloaded from here:
## https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish
## and manually copied into the data folder
warning("Did you download the weekly data?")

### --------------------------------------------------
## Survey biomass
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
index <- sqlQuery(AFSC, test)
if(!is.data.frame(index)) stop("Failed to query GOA survey data")
write.csv(index, 'data/index.csv', row.names=FALSE)


## Survey biomass by area
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
write.csv(index_by_area, 'data/index_by_area.csv', row.names=FALSE)

### Close connections to prevent a warning when sourced
odbcCloseAll()
