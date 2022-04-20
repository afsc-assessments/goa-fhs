#GOA Flathead Sole Assessment Input sourcing (Created by Carey McGilliard 2/2022):

#Catch biomass is one area and downloaded directly from AKFIN Answers
#
#----------------------------------------------------------------------------------
#throughout make sure the SpeciesCode = 10130 for the survey, 103 for observer data
#----------------------------------------------------------------------------------
#
#Fishery lengths
#update directories and source:
source("C:\\GitProjects\\newsbss\\Get_Fishery_Lengths\\Get_Fishery_Lengths_With_Extrapolated_Number.R")

#Nsamp for Fishery lengths (and tables for the SAFE):
#update directories and source:
source("C:\\GitProjects\\newsbss\\Get_Fishery_Lengths\\Get_Fishery_Length_HaulStats_With_NORPAQ.R")

#Survey lengths
#update directories and source:
source("C:\\GitProjects\\newsbss\\Get_GOA_Survey_Length_and_Age_Comp\\Get_GOA_Length_Comps.R")

#update directories and source (for nsamp for survey lengths and for ages), by area (made with Wayne Palsson's help):
source("C:\\GitProjects\\newsbss\\Get_GOA_Survey_Length_and_Age_Comp\\Get_Survey_Length_Age_Stats.R")

#Fishery ages (if there are any for flathead now (?); this was a criticism of the CIE review for rex- it uses raw lengths rather than extrapolated lengths to include the port data):
source("C:\\GitProjects\\newsbss\\Inputs_PieceByPiece\\Get_Fishery_Ages\\Get_Fishery_Ages_Port_or_Haul_noStratifying_noExtrapLens.R")

#There are other files for doing this with stratifying options, but these methods need to be revisited
#Note that there are lines for reading in the data from .Rdata files, but below this are the queries for getting the data from SQL and then saving those same .Rdata files
#This file also gets the number of hauls.
#Don't forget to add in columns for ages that don't appear in the data - this isn't automated.
#Run with 
#FmpArea = "600 and 699" 
#ByGear = 0 
#ByPortOrHaul = 0 
#LengthBins = seq(6,70,2)
#AgeBins = seq(1,29,1)


#Survey conditional age-at-lengths
#do the SQL query and write the data necessary to read in to the next step here (which will also plot a bunch of age-length ggplots):
source("C:\\GitProjects\\newsbss\\Inputs_PieceByPiece\\Get_GOA_Survey_Length_and_Age_Comp\\Get_Survey_Length_Age_and_Plot.R")

#Run this to get the conditional age-at-length data roughly formatted for assessment input
source("C:\\GitProjects\\newsbss\\Inputs_PieceByPiece\\Get_GOA_Survey_Length_and_Age_Comp\\GET_GOA_CONDITIONAL_AGE_AT_LENGTH.R")
#run it for eastern == 0 (this will write output combined over areas)
#len_bins = seq(6,70,2)
#max_age = 29

#GOA marginal age comps (include so that you can see the ghosted fits in r4ss output)
source("C:\\GitProjects\\newsbss\\Get_GOA_Survey_AgeComposition.R")
#set 

#That is the end of the data input process

#Before running the projection part of the code below do 2 manual steps:
#1. update AK-projection-simulation repo (pull updates)
#2. update the projection input files aside from the main (ForProjections) data file (these are in: 
#C:\Users\carey.mcgilliard\Work\FlatfishAssessments\ProjectionModel\FlatheadTemplate)
#Updating requires updated catches and updating the number of years of projections (14 for a full assessment year with a new data) and new catches and check # years of catches input in proj input files
#Probably worthwhile to check into whether it's better to run Jim's R code in AK-projection-simulation repo?? but the code below is for SS output)
#Carey can never get Jim's newest projection code to run.

#To:
#1. plot SS results
#2. Find Francis or McAllister and Ianelli Data weighting update values (for variance adjustments)
#3. Plot fishery selex and maturity comparison for rex (as this was what made bio ref pts unreliable in the single area model - fishery selex way to right of maturity)
#4. Plot growth data and estimated growth curves for both areas (uses AgeLength.csv, which was made earlier in newsbss/Inputs_PieceByPiece/Get_Survey_Length_Age_and_Plot.R)
#5. Write .csv files for making parameter reporting tables
#6. Write the projection input file to run folder (Careful: be sure to check over the projection file, especially female and male maturity)
#(note writing directly to the run folder is meant to help prevent reporting an exec summary table for the wrong run and also helps when comparing projections for multiple runs)
#7. Run projections:
source("C:\\GitProjects\\newsbss\\Process_SS_Output\\SS3toProjectTables.R")


#To make the executive summary table and harvest spec tables and do the required phase plot:
source(C:\GitProjects\newsbss\Process_SS_Output\ExecSummaryTable_Phase_Plot_HarvestRecsTable.R)

#To run retrospectives:
#Make a new folder (Like Run_8_Retro), run the following (and this will plot the retros too):
#change file paths and run folders
source("C:\\GitProjects\\newsbss\\Process_SS_Output\\RunRetrospectives.R")

#Compare any alternative runs using:
#change file paths and run folders
source("C:\\GitProjects\\newsbss\\Process_SS_Output\\CompareSS3Runs.R")
#this includes comparing likelihoods and parameter values and making tables for the safe for these comparisons

#To make Time Series tables:
#Run SStoProjectTables.R to get MyOutput (summarizes SS run) then:
source("C:\\GitProjects\\newsbss\\Process_SS_Output\\Time_Series_Param_Tables_2_Growth_Morphs.R")
#This should also work for a single growth morph, but 2 GPs needed a small code update this year, so 1 GP might also.

#To make tables describing the fishery:
source("C:\GitProjects\newsbss\AssessmentTables\Catch_by_Grouping.R")

#To get survey cpue maps:
#Jim Ianelli.




