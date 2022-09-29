To do this the old fashioned (ADMB) way, generate inputs with the code in `04_projections_apportionment` under "RE.Dat inputs".

Requires an empty folder each for Eastern, Central, and Western. There's now a loop which copies in the misc ADMB files, auto-populates the re.dat object based on the downloaded design-based survey indices by arrea, and runs the ADMB-RE code

Each folder contains a csv which was saved using this code; values therein were copied into re.dat .
 
The re.exe was run for each area, wrangled using code in section "wrangle RE Outputs" and saved to the Excel file
"-AreaApportionment.csv."

Note that "EGOA" is comprised of "Southeast" and W Yakutat. 

The biomass fractions come from Oracle under RACE Survey > GOA - FRactional Biomass in Eastern GOA,
which may or may not be updated (as of Sep 2022 I'm using the 2021 fractions).