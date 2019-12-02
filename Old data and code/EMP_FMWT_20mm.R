# code to merge EMP, FMWT, and 20mm date - merges EMP and FMWT first, then 20mm

library(readxl)
library(tidyverse)
library(reshape2)

# first import and modify EMP data
EMP <- read_excel("1972-2017CBMatrix.xlsx", 
                      sheet = "CB CPUE Matrix 1972-2017")
View(EMP)

# make taxa long form so can merge with crosswalk 
summary(EMP)
zooEMP = melt(EMP, id = c("SurveyCode", "Year", "Survey", "SurveyRep", "Date", "Station", "EZStation", "DWRStation", "Core", "Region", "Secchi", "Chl-a", "Temperature",
                          "ECSurfacePreTow", "ECBottomPreTow", "CBVolume"), variable.name = "Taxa", value.name = "CPUE")
View(zooEMP)

# add project column
zooEMP$Project = "EMP"

# columns in common between EMP and FMWT are: project, year, survey, date, station, region, 
# secchi, temperature/tempsurf, cbvolume/volume, taxa, cpue (selected for columns below)

zooEMP2 = zooEMP[,c(2,3,5,6,10,11,13,16,17,18,19)]
View(zooEMP2)
zooEMP2 = rename(zooEMP2, Volume = CBVolume)

# make column names line up between crosswalk and EMP data
zooEMP3 = rename(zooEMP2, EMPtaxa = Taxa)
View(zooEMP3)

# now add crosswalk LCD names
crosswalk <- read_excel("FMWT_crosswalk.xlsx", sheet = "EMP_FMWT")
View(crosswalk)
zooEMP4 = merge(zooEMP3, distinct(crosswalk[,c(1,2, 4)]), by = "EMPtaxa")
View(zooEMP4)

# remove unique EMP taxa column
zooEMP5 = zooEMP4[,-c(1)]
View(zooEMP5)


# then modify FMWT data set (still includes summer samples - not sure if they should be removed)
FMWT <- read_excel("FMWT_TNSZooplanktonDataCPUEOct2017.xls", 
                  sheet = "FMWT&TNS ZP CPUE")

zooFMWT = melt(FMWT, id = c("Project", "Year", "Survey", "Month", "Date", "Station", "Index", "Time", 
                            "TowDuration", "Region", "FLaSHRegionGroup", "TideCode", "DepthBottom",
                                    "CondSurf", "PPTSurf", "SurfSalinityGroup", "CondBott", "PPTBott", 
                            "TempSurf", "Secchi", "Turbidity", "Microcystis", "TotalMeter", "Volume"), 
               variable.name = "Taxa", value.name = "CPUE")

zooFMWT2 = zooFMWT[,c(1,2,3,5,6,10,19,20,24,25,26)]

zooFMWT2 = rename(zooFMWT2, Temperature = TempSurf)

# make column names line up between crosswalk and FMWT data
zooFMWT3 = rename(zooFMWT2, FMWTtaxa = Taxa)
View(zooFMWT3)

# now add crosswalk LCD names
zooFMWT4 = merge(zooFMWT3, distinct(crosswalk[,c(1,3, 4)]), by = "FMWTtaxa")
View(zooFMWT4)

# remove unique FMWT taxa column
zooFMWT5 = zooFMWT4[,-c(1)]
View(zooFMWT5)

# now combine two data sets - stack vertically
FMWT_EMP = rbind(zooEMP5, zooFMWT5)
View(FMWT_EMP)

# convert back into wide form with taxa as column headers

# check to see if duplicates

zooEMPc = dcast(FMWT_EMP, Year + Survey + Date+ Station + Region + Secchi + Temperature + 
                  Project + Volume ~ LCDtax, value.var = "CPUE", fun.aggregate = sum)
# is it correct to sum them? (for instance SYNCH is enetered twice in the LCD column because
# it corresponds to both SYNCH and SYNCHBIC in the EMPtaxa column - so EMP has two entries
# for SYNCH)
View(zooEMPc)

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

#import crosswalk table for taxa
taxa20 <- read_excel("FMWT_crosswalk.xlsx", 
                     sheet = "for20mm")

#attatch taxa to the main data set
zoop20 = merge(zoo20, taxa20, by = "CommonName")

zoop20.1 = zoop20[,-c(1,2,5,6,7,8,9,10,11,12)]

# add project column
zoop20.1$Project = "20mm"

# rename columns to match FMWT_EMP_2
zoop20.1 = rename(zoop20.1, Station = Station20mm)
zoop20.1 = rename(zoop20.1, Date = SampleDate)
zoop20.1 = rename(zoop20.1, Volume = volume)

FMWT_EMP_2 = FMWT_EMP[,c(3,4,8,9,10,12)] 
View(FMWT_EMP_2)

# bind all three
FMWT_EMP_20mm = rbind(FMWT_EMP_2, zoop20.1)
View(FMWT_EMP_20mm)

write.csv(FMWT_EMP_20mm, "FMWT_EMP_20mm.csv", row.names = F)

merged <- round(FMWT_EMP_20mm$CPUE, digits = 3)
View(merged)

#convert to wide data frame
FMWT_EMP_20mm_wide = dcast(FMWT_EMP_20mm, Date + Station + Project + Volume ~ LCDtax, 
                           value.var = "CPUE", fun.aggregate = sum)
View(FMWT_EMP_20mm_wide)

# create unique sample ID

FMWT_EMP_20mm_wide$sampleID = paste(FMWT_EMP_20mm_wide$Date, FMWT_EMP_20mm_wide$Station, 
                                    FMWT_EMP_20mm_wide$Project)
FMWT_EMP_20mm_wide <- FMWT_EMP_20mm_wide[c(56,1:55)]

# below is just a function to round
round_df <- function(df, digits = 3) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

mergeddata<-round_df(FMWT_EMP_20mm_wide, digits = 3)


