## Preprocess data, write TAF data tables

## Before: her.27.6aS7bc_Survey_Catch_Data.csv, her.27.6aS7bc_LengthFreq_DataPivot.csv (bootstrap/data)
## After:  survey_catch.csv, length_frequency.csv (data)

library(icesTAF)

# setwd(".//TAF//2021_her.27.6aS7bc//2021_her.27.6aS7bc")

mkdir("data")

##############################################################################################
## Read in and Prepare Survey, Catch and Length Frequency Input Files
#############################################################################################


## Import data - Survey biomass index and catch data; length frequency data -----

survey_catch <- read.csv("bootstrap/data/her.27.6aS7bc_Survey_Catch_Data.csv",header = TRUE )
(survey_catch <- subset(survey_catch, Survey_Index != "NA"))


length_frequency <- read.csv("bootstrap/data/her.27.6aS7bc_LengthFreq_DataPivot.csv")
names (length_frequency) <-gsub ("X", "", names(length_frequency))
length_frequency



## Write out the csv files
write.taf(survey_catch, dir="data")
write.taf(length_frequency, dir="data")




