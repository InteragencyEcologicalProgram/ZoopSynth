# code to merge EMP, FMWT, and 20mm date - merges EMP and FMWT first, 
# then merges 20mm eniroenmntal data to CPUE data, and then merges all 3

library(dplyr)
library(reshape2)
library(visreg)
library(readxl)

#download the file
download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2017CBMatrix.xlsx", 
              "1972-2017CBMatrix.xlsx", mode="wb")


# first import and modify EMP data
EMP <- read_excel("1972-2017CBMatrix.xlsx", 
                      sheet = "CB CPUE Matrix 1972-2017", 
                  col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                               "text", "text", "text", "numeric", 
                               "text", "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric", "numeric"))


# make taxa long form so can merge with crosswalk 

zooEMP = melt(EMP, id = c("SurveyCode", "Year", "Survey", "SurveyRep", 
                          "Date", "Station", "EZStation", "DWRStation", 
                          "Core", "Region", "Secchi", "Chl-a", "Temperature",
                          "ECSurfacePreTow", "ECBottomPreTow", "CBVolume"), 
                           variable.name = "Taxa", value.name = "CPUE")


# add project column
zooEMP$Project = "EMP"

# columns in common between EMP and FMWT are: project, year, survey, date, 
# station, region, CondBott/ECBottomPreTow, CondSurf/ECSurfacePreTow, secchi, 
# temperature/tempsurf, cbvolume/volume, taxa, cpue (selected for columns below)
#Select for columns in common:

zooEMP2 = zooEMP[,c(2,3,5,6,10,11,13,14,15,16,17,18,19)]

zooEMP2 = rename(zooEMP2, Volume = CBVolume) 
zooEMP2 = rename(zooEMP2, CondSurf = ECSurfacePreTow) 
zooEMP2 = rename(zooEMP2, CondBott = ECBottomPreTow) 

# make column names line up between crosswalk and EMP data
zooEMP3 = rename(zooEMP2, EMPtaxa = Taxa)

# now add crosswalk LCD names
crosswalk <- read_excel("FMWT_crosswalk.xlsx", sheet = "EMP_FMWT")

zooEMP4 = merge(zooEMP3, distinct(crosswalk[,c(2,4)]), by = "EMPtaxa")

# remove unique EMP taxa column
zooEMP5 = zooEMP4[,-c(1)]


# then modify FMWT data set (still includes summer samples - not sure if they 
# should be removed)

#download the file
download.file("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSZooplanktonDataCPUEOct2017.xls", 
              "FMWT_TNSZooplanktonDataCPUEOct2017.xls", mode="wb")

FMWT <- read_excel("FMWT_TNSZooplanktonDataCPUEOct2017.xls", 
                  sheet = "FMWT&TNS ZP CPUE")


zooFMWT = melt(FMWT, id = c("Project", "Year", "Survey", "Month", "Date", 
                            "Station", "Index", "Time", "TowDuration", 
                            "Region", "FLaSHRegionGroup", "TideCode", 
                            "DepthBottom", "CondSurf", "PPTSurf", 
                            "SurfSalinityGroup", "CondBott", "PPTBott", 
                            "TempSurf", "Secchi", "Turbidity", "Microcystis", 
                            "TotalMeter", "Volume"), variable.name = "Taxa", 
                             value.name = "CPUE")

zooFMWT2 = zooFMWT[,c(1,2,3,5,6,10,14,17,19,20,24,25,26)]

zooFMWT2 = rename(zooFMWT2, Temperature = TempSurf)

# make column names line up between crosswalk and FMWT data
zooFMWT3 = rename(zooFMWT2, FMWTtaxa = Taxa)


# now add crosswalk LCD names
zooFMWT4 = merge(zooFMWT3, distinct(crosswalk[,c(3,4)]), by = "FMWTtaxa")


# remove unique FMWT taxa column
zooFMWT5 = zooFMWT4[,-c(1)]

# now combine two data sets - stack vertically
FMWT_EMP = rbind(zooEMP5, zooFMWT5)


# convert back into wide form with taxa as column headers

# check to see if duplicates

zooEMPc = dcast(FMWT_EMP, Year + Survey + Date + Station + CondSurf+ CondBott + 
                  Region + Secchi + Temperature + Project + Volume ~ LCDtax, 
                value.var = "CPUE", fun.aggregate = sum)
# is it correct to sum them? (for instance SYNCH is enetered twice in the LCD 
# column because it corresponds to both SYNCH and SYNCHBIC in the EMPtaxa 
# column - so EMP has two entries for SYNCH)
# Rosie - Yes! Looks great.


## Import 20 mil and calculate CPUE

zoopquery20mm <- read_excel("zoopquery20mm.xlsx", 
                            col_types = c("date", "numeric", "skip", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text"))

#Rosie's code to calculate CPUE for 20mm

zoopquery20mm$ZSampleID = paste(zoopquery20mm$Station20mm, zoopquery20mm$SampleDate)

#combine the slides
totalcells = group_by(zoopquery20mm, ZSampleID) %>% 
  summarise(totcells = max(CellNumber), totCountall = sum(ZooCount))

zoo20 = group_by(zoopquery20mm, Station20mm, SampleDate, ZSampleID, Dilution, ZooCode, CommonName,
                 MeterStart, MeterEnd) %>% 
  summarise(totCount = sum(ZooCount))

zoo20 = merge(zoo20, totalcells)

#adjust for subsampling
zoo20$atotal = (zoo20$totCount/zoo20$totcells)*zoo20$Dilution

#Volume sampled is the difference in flowmeter readings times the flowmeter constand
#times the mouth area
zoo20$volume = NA
zoo20$volume = (zoo20$MeterEnd-zoo20$MeterStart)*0.026873027*0.0167


#volume for samples where the flowmeter turned over to zero.
zoo20$volume[which(zoo20$volume<0)] = ((1000000 - zoo20$MeterStart[which(zoo20$volume<0)])+
                                         zoo20$MeterEnd[which(zoo20$volume<0)])*0.026873027*0.0167 #I've used the factory calibration here

#some of the samples iddn't have flowmeters, so we'll use the average volume for those
zoo20$volume[which(is.na(zoo20$volume))] = mean(zoo20$volume, na.rm = T)

#Calculate CPUE (total critters divided by volume sampled)
zoo20$CPUE = zoo20$atotal/zoo20$volume

##NEW
# merge 20mm CPUE with environmental data
# CPUE for 20mm are in zoo20.R file that was created in zoo20mil_w20mil.R

#merge environmental data for 20mm w/ CPUE data
# import survey and station informationf from excel files (these excel files are from 20mm mdb - requires a lot of different
# packages to import directly from mdb so just converted them to excel files for now)


#Rosie - we can probably just put this in to the origional query from teh database...

survey <- read_excel("20mm_Survey.xlsx")

stations <- read_excel("20mm_Station.xlsx")

# merge based on survey ID - unique for every date (date not in station data)

enviro20mm = merge(survey, stations, by = "SurveyID")
View(enviro20mm) # now date is attached

# merge station and date so we have a common term with zoo20 to merge

enviro20mm$ZSampleID = paste(enviro20mm$Station, enviro20mm$SampleDate)


# then merge with CPUE data based on ZSampleID

complete20mm = merge(enviro20mm, zoo20, by = "ZSampleID")


#import crosswalk table for taxa
taxa20 <- read_excel("FMWT_crosswalk.xlsx", 
                     sheet = "for20mm")

#attatch taxa to the main data set
zoop20 = merge(complete20mm, taxa20, by = "CommonName")

zoop20.1 = zoop20[,-c(1,2,3,5,6,7,9,10,11,12,13,14,19,20,21,22,23,24,25,26,27,
                      28,29,30,31)]


# add project column
zoop20.1$Project = "20mm"


# rename columns to match FMWT_EMP
zoop20.1 = rename(zoop20.1, CondBott = BottomEC)
zoop20.1 = rename(zoop20.1, CondSurf = TopEC)
zoop20.1 = rename(zoop20.1, Date = SampleDate.x)
zoop20.1 = rename(zoop20.1, Temperature = Temp)

FMWT_EMP_2 = FMWT_EMP[,c(3,4,6,7,8,9,11,12,13)] 
View(FMWT_EMP_2)

View(zoop20.1)
#column names match

# bind all three
FMWT_EMP_20mm = rbind(FMWT_EMP_2, zoop20.1)
View(FMWT_EMP_20mm)

# create unique sample ID

FMWT_EMP_20mm$sampleID = paste(FMWT_EMP_20mm$Date,FMWT_EMP_20mm$Station,FMWT_EMP_20mm$Project)

#reorder columns
merged_zoop <- FMWT_EMP_20mm[c(10,8,1,2,3,4,5,6,9,7)]

# below is just a function to round
round_df <- function(df, digits = 3) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

mergeddata<-round_df(merged_zoop, digits = 3)
View(mergeddata)
str(mergeddata)

write.csv(mergeddata, file = "merged.csv")




