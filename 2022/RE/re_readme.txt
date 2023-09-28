To do this the old fashioned (ADMB) way, generate inputs with the code in `04_projections_apportionment` under "RE.Dat inputs".

Requires an empty folder each for Eastern, Central, and Western. There's now a loop which copies in the misc ADMB files, auto-populates the re.dat object based on the downloaded design-based survey indices by arrea, and runs the ADMB-RE code

Each folder contains a csv which was saved using this code; values therein were copied into re.dat .
 
The re.exe was run for each area, wrangled using code in section "wrangle RE Outputs" and saved to the Excel file
"-AreaApportionment.csv."

Note that "EGOA" is comprised of "Southeast" and W Yakutat. 

The biomass fractions come from Oracle under RACE Survey > GOA - FRactional Biomass in Eastern GOA,
which may or may not be updated (as of Sep 2022 I'm using the 2021 fractions).

In 2022 I did a bridge from ADMB to Jane's package. First I ran a for-loop that pulled the information from the rwout.rep files and fit each model individually. Running compare_rema illustrated that whatever ADMB-RE version is within the .exe I used last year returns the same values as the individual, univariate TMB version. 

Then I ran REMA with all three strata at once (reading in the data from rwout.rep). These are not different from the separate runs but execute more quickly.

In future years, we can simply pass REMA a long-format data frame (as `biomass_dat`) with a column for YR, STRATA, BIOMASS, CV.