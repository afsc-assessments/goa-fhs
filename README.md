# goa-fhs

Gulf of Alaska flathead sole stock assessments (full and harvest projections). Maia Sosa Kapur [maia.kapur\@noaa.gov](mailto:maia.kapur@noaa.gov){.email}

2023: Harvest Projection

2022: Full Assessment (M Kapur)

2021: Partial (since renamed "harvest projection")

2020: Partial (since renamed "harvest projection") (C Monnahan)


Code here builds upon work by C. McGilliard and C. Monnohan. Document dependency on [safe](https://github.com/BenWilliams-NOAA/safe) and apportionments via [rema](https://github.com/afsc-assessments/rema) both v0.1.0.

## How to Reproduce this Assessment
The code in `R/` should be self-contained & self-explanatory, with the exception that it depends upon "newsbss" code for SQL queries & data munging developed by C. McGilliard. The code used for these functions should be accessible on the shared drive via `M:\Monnahan\newsbss`; this hasn't been updated since 15 Sep 2021.

### 00_getData.R
This script will allow you to download, or provide instructions for manually downloading, input data for the assessment (or miscellaneous figures). It also has code to reformat inputs and save CSVs ready to copy into SS. These code are pretty self-explanatory; the main hiccups here were 1) needing to ask the RACE group (Megsie Siple, margaret.siple@noaa.gov) for a CSV of the survey observations by depth, and 2) downloading weekly catches from https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-groundfish for extrapolation purposes. For the former, Megsie was able to pull data for me within a few days.
As of 2022 this model does not use the design-based index, but I have requested that the GAP group continue producing that index for sensitivity purposes during "on" years, using their formal request process in ~Jan of each year.

### 01_bridging-and-modelDev.qmd
You do not need to re-run this script; rather it is a notebook-style archive of the steps taken to bridge the 2017 model into the latest version of SS3, add in the data, and check some outstanding sensitivities.

### 02_basemodel.qmd
This script assumes you have the executed model files; these are saved both on MSK's local computer under `m0_8-newMI-biasAdj`, or in this repo under `ModelFiles_PlanTeam`. This can be used to re-create many figures in the document, including the cool maps of survey abundance using `akgfmaps`.

### 03_sensitivities.qmd
Has code for automating the retrospective analysis, likelihood profiles, and a notebook-style walkthrough of the sensitivities ran for this assessment. There were many as this hadn't been assessed since 2017 and had undergone a CIE.

### 04_projections_apportionment.qmd
This shows the steps used to extrapolate the catches for the rest of 2022, automatically write the proj input file using code modified from CCM, and extract and build the SAFE table from the projection outputs. (You can actually run the proj model right from R using the call to `shell` on line 167).

### 05_notes.qmd
Dump of notes and to-dos for next time, not well organized.

# Docs/
This is the deploy hub for the static web pages I provided with the sensitivities/add'l analyses presented in 2022. Because I didn't want to confuse reviewers thinking these were proposed model alternates, I instead wrote up these .qmds (using figures made in 03_sensitivities.qmd) and deployed them with some explanatory text online, providing links in the document. These are for archival and reference purposes only.
