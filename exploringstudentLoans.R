#Download files using my own function
source("./UsefulFunctions.R")
library(data.table)
library(readxl)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(leaflet)
library(maps)
# The data is available at:
# 
#         https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv.
#       Note that the data file contains the data for the most recent cohort.
#       If you are interested in all preceding cohorts you can find the data at
#       https://ed-public-download.app.cloud.gov/downloads/CollegeScorecard_Raw_Data.zip.
# 
# Documentation for the data is available at https://collegescorecard.ed.gov/data/documentation/.
# Here, you can also download the data dictionary (https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx).

        address <- "https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv"
        addressFull <- "https://ed-public-download.app.cloud.gov/downloads/CollegeScorecard_Raw_Data.zip%20"
        directoryAddress <- "https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx"
        URLs <- c(address,addressFull,directoryAddress)
        lapply(URLs, smartDownload)

#the recent Grads
        RecentCohort <- fread("Most-Recent-Cohorts-All-Data-Elements.csv")
        View(RecentCohort)


#Check out earlier grads
#Get the dictionary first can use the UsefulFunctions.R file xlsxtoDT()
        DictionaryExcelfile <-"CollegeScorecardDataDictionary.xlsx"
        Dictionary <- xlsxToDT(DictionaryExcelfile)
        Dictionary$data_dictionary
        rm(DictionaryExcelfile)

#figure out how to look at all of the possible values for a particular category
#replace all the entries in the `VARIABLE NAME` from chosen variable to the end
# with the variable name. Do that from the named row to the end of the table (in a new
# variable so as to not save over variable column)
        AddAllVarNames(Dictionary$data_dictionary)
#now there is a Variable name for every row for easy subsetting
        setkey(Dictionary$data_dictionary,AllVarNames)
        dic <- Dictionary$data_dictionary#for easy reference

#Now looking at the actual data
        unzip("CollegeScorecard_Raw_Data.zip")
        unzip("CollegeScorecard_Raw_Data/Crosswalks_20160908.zip",
                        exdir = "CollegeScorecard_Raw_Data/Crosswalks")
#now that it's all unzipped, can check out what's going on
        fifteen <- fread("CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv",na.strings = c("NA","NULL"))
        summary(fifteen)
        View(fifteen)
        
#Also see what the crosswalks are all about
#Make a function for reading all sheets of an excel file into a list found in:
#UsefulFunctions.R

        crosswalk2014 <- xlsxToDT(crosswalk2014file)
        library(tidyverse)
        
#check to see if 2014-15 year is identical to recent cohort
        identical(fifteen$UGDS_MEN,RecentCohort$UGDS_MEN)
        
#read in another year
        fourteen <- fread("./CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv",na.strings = c("NA","NULL"))
        identical(fourteen$UGDS_MEN,RecentCohort$UGDS_MEN)
        View(fourteen)
        
#Just look at most recent cohort first for a while
        str(fifteen)
        Dictionary$data_dictionary["RELAFFIL"]
        fifteen[1:100,(c("INSTNM",sort(grep("SAT",names(fifteen),value=TRUE)))),with=FALSE]
        fifteen[,.(INSTNM,GT_25K_P)]
        g <- ggplot(data=fifteen,aes(x=SATWRMID)) + geom_histogram()
        ggplot(data=fifteen,aes(x=SAT_AVG,y=MN_EARN_WNE_P6)) + geom_point()

#Looks like there are no data points for this year. Maybe try an earlier year Exploring2010.R
       