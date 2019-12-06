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
zooEMP3 = rename(zooEMP2, EMP = Taxa)
View(zooEMP4)

# now add crosswalk LCD names
crosswalk <- read_excel("new_crosswalk.xlsx", sheet = "LCD")

zooEMP4 = merge(zooEMP3, distinct(crosswalk[,c(1,2)]), by = "EMP")

# remove unique EMP taxa column
zooEMP5 = zooEMP4[,-c(1)]
as.factor(zooEMP5$Taxname)
Taxname<-unique(zooEMP5$Taxname)
EMPlist <- data.frame(Taxname)
View(EMPlist)
hier <- read_excel("new_crosswalk.xlsx", sheet = "Hierarchy")
View(hier)
EMPhier = merge(EMPlist, distinct(hier[,c(1,2,3,4,5,6,7,8)]), by = "Taxname")
View(EMPhier)
## now we have hierarchy table for taxa present in EMP, so lets do the same for FMWT and then do if else statements to get LCD

#############import FMWT

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
str(zooFMWT2)

# make column names line up between crosswalk and FMWT data
zooFMWT3 = rename(zooFMWT2, FMWT = Taxa)


# now add crosswalk LCD names
zooFMWT4 = merge(zooFMWT3, distinct(crosswalk[,c(1,3)]), by = "FMWT")
View(zooFMWT4)

# remove unique FMWT taxa column
zooFMWT5 = zooFMWT4[,-c(1)]

as.factor(zooFMWT5$Taxname)
Taxname<-unique(zooFMWT5$Taxname)
FMWTlist <- data.frame(Taxname)
View(FMWTlist)

FMWThier = merge(FMWTlist, distinct(hier[,c(1,2,3,4,5,6,7,8)]), by = "Taxname")
View(FMWThier)
View(EMPhier)


####loops to get LCD for two data sets (EMP and FMWT)

# getting LCD vector for FMWT
LCDFMWT <- c() # create an empty vector for LCD tax
for (i in 1:nrow(FMWThier)) { # outside loop tell it to check each line going down for each tax name
  if ((FMWThier[i,8] %in% EMPhier[,8]) & (!is.na(FMWThier[i,8]))) {   #tells it "if species is equal to any of the species in the other
    LCDFMWT[i] = FMWThier[i,8]        #dataset then add species to new column, if not .. if genus is equal to other genus then
  }  else if ((FMWThier[i,7] %in% EMPhier[,7]) & (!is.na(FMWThier[i,7]))) { #add genus to new column, etc...
    LCDFMWT[i] = FMWThier[i,7]   
  } else if ((FMWThier[i,6] %in% EMPhier[,6]) & (!is.na(FMWThier[i,6]))) {
    LCDFMWT[i] = FMWThier[i,6]   
  } else if ((FMWThier[i,5] %in% EMPhier[,5]) & (!is.na(FMWThier[i,5]))) {
    LCDFMWT[i] = FMWThier[i,5]   
  } else if ((FMWThier[i,4] %in% EMPhier[,4]) & (!is.na(FMWThier[i,4]))) {
    LCDFMWT[i] = FMWThier[i,4]   
  } else if ((FMWThier[i,3] %in% EMPhier[,3]) & (!is.na(FMWThier[i,3]))) {
    LCDFMWT[i] = FMWThier[i,3] 
  }
}
LCDFMWT


## Now lets get LCD taxa for EMP

LCDEMP <- c() # create an empty vector for LCD tax
for (i in 1:nrow(EMPhier)) { # outside loop tell it to check each line - change to nrow
  if ((EMPhier[i,8] %in% FMWThier[,8]) & (!is.na(EMPhier[i,8]))) {   # inside loop tells it "if species is equal to any of the species in the other
    LCDEMP[i] = EMPhier[i,8]        #dataset then add species to new column, if not .. if genus is equal to other genus then
  }  else if ((EMPhier[i,7] %in% FMWThier[,7]) & (!is.na(EMPhier[i,7]))) { #add genus to new column
    LCDEMP[i] = EMPhier[i,7]   
  } else if ((EMPhier[i,6] %in% FMWThier[,6]) & (!is.na(EMPhier[i,6]))) {
    LCDEMP[i] = EMPhier[i,6]   
  } else if ((EMPhier[i,5] %in% FMWThier[,5]) & (!is.na(EMPhier[i,5]))) {
    LCDEMP[i] = EMPhier[i,5]   
  } else if ((EMPhier[i,4] %in% FMWThier[,4]) & (!is.na(EMPhier[i,4]))) {
    LCDEMP[i] = EMPhier[i,4]   
  } else if ((EMPhier[i,3] %in% FMWThier[,3]) & (!is.na(EMPhier[i,3]))) {
    LCDEMP[i] = EMPhier[i,3] 
  }
}
LCDEMP

## use rbind at end to turn LCD vector into column

# now add these LCD columns to the hierarchy table for each data set, then add this back to data set
# and delete unwanted columns from hierarchy table
# in the future can just create a column on the taxa list for each data set (but would
# need to be in the same order)
EMPlist.2<-cbind(EMPhier, LCDEMP) # because we need Taxname with LCD name to attach back to main dataset
View(EMPlist.2)
zooEMP6 = merge(zooEMP5, EMPlist.2[,c(1,9)], by = "Taxname")
View(zooEMP7)
zooEMP7 = zooEMP6[,-c(1)] # remove old Taxname so we just have LCD

# for FMWT:
FMWTlist.2<-cbind(FMWThier, LCDFMWT) # because we need Taxname with LCD name to attach back to main dataset
View(FMWTlist.2)
zooFMWT6 = merge(zooFMWT5, FMWTlist.2[,c(1,9)], by = "Taxname")
View(zooFMWT6)
zooFMWT7 = zooFMWT6[,-c(1)] # remove old Taxname so we just have LCD
View(zooFMWT7)

# now combine datasets - first make taxa column have the same name
zooEMP7 = rename(zooEMP7, LCDTax = LCDEMP)
zooFMWT7 = rename(zooFMWT7, LCDTax = LCDFMWT)
FMWT_EMP = rbind(zooEMP7, zooFMWT7)
View(FMWT_EMP)

#add CPUEs for common taxa within samples - this sums up cpue's for common taxa
zoocomb = dcast(FMWT_EMP, Year + Survey + Date + Station + CondSurf + CondBott + 
                  Region + Secchi + Temperature + Project + Volume ~ LCDTax, 
                value.var = "CPUE", fun.aggregate = sum)
View(zoocomb)


# create unique sample ID

zoocomb$sampleID = paste(zoocomb$Date,zoocomb$Station,zoocomb$Project)
View(zoocomb)

#reorder columns
merged_zoop <- zoocomb[c(57,1:56)]
View(merged_zoop)
# below is just a function to round
round_df <- function(df, digits = 3) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

mergeddata<-round_df(merged_zoop, digits = 3)
View(mergeddata)
# data in long form



### now going to start from before LCD tax was determined to do LCD for three datasets 
# EMP, FMWT, and 20mm


## Import 20 mil and calculate CPUE - hopefully there will be flat files online in the future

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

#Volume sampled is the difference in flowmeter readings times the flowmeter constant
#times the mouth area
zoo20$volume = NA
zoo20$volume = (zoo20$MeterEnd-zoo20$MeterStart)*0.026873027*0.0167


#volume for samples where the flowmeter turned over to zero.
zoo20$volume[which(zoo20$volume<0)] = ((1000000 - zoo20$MeterStart[which(zoo20$volume<0)])+
                                         zoo20$MeterEnd[which(zoo20$volume<0)])*0.026873027*0.0167 #I've used the factory calibration here

#some of the samples didn't have flowmeters, so we'll use the average volume for those
zoo20$volume[which(is.na(zoo20$volume))] = mean(zoo20$volume, na.rm = T)

#Calculate CPUE (total critters divided by volume sampled)
zoo20$CPUE = zoo20$atotal/zoo20$volume


# merge 20mm CPUE with environmental data
# CPUE for 20mm are in zoo20.R file that was created in zoo20mil_w20mil.R

#merge environmental data for 20mm w/ CPUE data
# import survey and station informationf from excel files (these excel files are from 20mm mdb - requires a lot of different
# packages to import directly from mdb so just converted them to excel files for now)

survey <- read_excel("20mm_Survey.xlsx")

stations <- read_excel("20mm_Station.xlsx")

# merge based on survey ID - unique for every date (date not in station data)

enviro20mm = merge(survey, stations, by = "SurveyID")
View(enviro20mm) # now date is attached

# merge station and date so we have a common term with zoo20 to merge

enviro20mm$ZSampleID = paste(enviro20mm$Station, enviro20mm$SampleDate)


# then merge with CPUE data based on ZSampleID

complete20mm = merge(enviro20mm, zoo20, by = "ZSampleID")

View(complete20mm)


#import new crosswalk table for taxa
taxa20 <- read_excel("new_crosswalk.xlsx", 
                     sheet = "LCD")
taxa20 = rename(taxa20, CommonName = twentymm) # so common name between taxa20 and crossover table

View(zoop20)

#attach taxa to the main data set
zoop20 = merge(complete20mm, taxa20[,c(1,4)], by = "CommonName")



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


# now create hierarchy table for 20mm
View(zoop20.1)
# remove annelid, insects (chironomid and other larvae) bc not in other data sets
zoop20.2 <- droplevels(zoop20.1[!zoop20.1$Taxname == 'Annelid worms',])
zoop20.3 <- droplevels(zoop20.2[!zoop20.2$Taxname == 'Chironomid larvae',])
zoop20.4 <- droplevels(zoop20.3[!zoop20.3$Taxname == 'Other Insect larvae',])
as.factor(zoop20.4$Taxname)
Taxname<-unique(zoop20.4$Taxname)
mmlist <- data.frame(Taxname)
View(mmlist)

hier <- read_excel("new_crosswalk.xlsx", sheet = "Hierarchy")
View(hier)
mmhier = merge(mmlist, distinct(hier[,c(1,2,3,4,5,6,7,8)]), by = "Taxname")
View(mmhier)
#now have a hierarchy table for 20mm so can create loops (three-way check)
LCDmm <- c() # create an empty vector for LCD tax
for (i in 1:nrow(mmhier)) { # outside loop tell it to check each line - change to nrow
  if ((mmhier[i,8] %in% EMPhier[,8]) & (mmhier[i,8] %in% FMWThier[,8]) & (!is.na(mmhier[i,8]))) {   # inside loop tells it "if species is equal to any of the species in the other
    LCDmm[i] = mmhier[i,8]        #dataset then add species to new column, if not .. if genus is equal to other genus then
  }  else if ((mmhier[i,7] %in% EMPhier[,7]) & (mmhier[i,7] %in% FMWThier[,7]) & (!is.na(mmhier[i,7]))) { #add genus to new column
    LCDmm[i] = mmhier[i,7]   
  } else if ((mmhier[i,6] %in% EMPhier[,6]) & (mmhier[i,6] %in% FMWThier[,6]) & (!is.na(mmhier[i,6]))) {
    LCDmm[i] = mmhier[i,6]   
  } else if ((mmhier[i,5] %in% EMPhier[,5]) & (mmhier[i,5] %in% FMWThier[,5]) & (!is.na(mmhier[i,5]))) {
    LCDmm[i] = mmhier[i,5]   
  } else if ((mmhier[i,4] %in% EMPhier[,4]) & (mmhier[i,4] %in% FMWThier[,4]) & (!is.na(mmhier[i,4]))) {
    LCDmm[i] = mmhier[i,4]   
  } else if ((mmhier[i,3] %in% EMPhier[,3]) & (mmhier[i,3] %in% FMWThier[,3]) & (!is.na(mmhier[i,3]))) {
    LCDmm[i] = mmhier[i,3] 
  }
}
LCDmm

# getting LCD vector for FMWT
LCDFMWT <- c() # create an empty vector for LCD tax
for (i in 1:nrow(FMWThier)) { # outside loop tell it to check each line - change to nrow
  if ((FMWThier[i,8] %in% EMPhier[,8]) & (FMWThier[i,8] %in% mmhier[,8]) & (!is.na(FMWThier[i,8]))) {   # inside loop tells it "if species is equal to any of the species in the other
    LCDFMWT[i] = FMWThier[i,8]        #dataset then add species to new column, if not .. if genus is equal to other genus then
  }  else if ((FMWThier[i,7] %in% EMPhier[,7]) & (FMWThier[i,7] %in% mmhier[,7]) & (!is.na(FMWThier[i,7]))) { #add genus to new column
    LCDFMWT[i] = FMWThier[i,7]   
  } else if ((FMWThier[i,6] %in% EMPhier[,6]) & (FMWThier[i,6] %in% mmhier[,6]) & (!is.na(FMWThier[i,6]))) {
    LCDFMWT[i] = FMWThier[i,6]   
  } else if ((FMWThier[i,5] %in% EMPhier[,5]) & (FMWThier[i,5] %in% mmhier[,5]) & (!is.na(FMWThier[i,5]))) {
    LCDFMWT[i] = FMWThier[i,5]   
  } else if ((FMWThier[i,4] %in% EMPhier[,4]) & (FMWThier[i,4] %in% mmhier[,4]) & (!is.na(FMWThier[i,4]))) {
    LCDFMWT[i] = FMWThier[i,4]   
  } else if ((FMWThier[i,3] %in% EMPhier[,3]) & (FMWThier[i,3] %in% mmhier[,3]) & (!is.na(FMWThier[i,3]))) {
    LCDFMWT[i] = FMWThier[i,3] 
  }
}
LCDFMWT


## Now lets get LCD taxa for EMP

LCDEMP <- c() # create an empty vector for LCD tax
for (i in 1:nrow(EMPhier)) { # outside loop tell it to check each line - change to nrow
  if ((EMPhier[i,8] %in% FMWThier[,8]) & (EMPhier[i,8] %in% mmhier[,8]) & (!is.na(EMPhier[i,8]))) {   # inside loop tells it "if species is equal to any of the species in the other
    LCDEMP[i] = EMPhier[i,8]        #dataset then add species to new column, if not .. if genus is equal to other genus then
  }  else if ((EMPhier[i,7] %in% FMWThier[,7]) & (EMPhier[i,7] %in% mmhier[,7]) & (!is.na(EMPhier[i,7]))) { #add genus to new column
    LCDEMP[i] = EMPhier[i,7]   
  } else if ((EMPhier[i,6] %in% FMWThier[,6]) & (EMPhier[i,6] %in% mmhier[,6]) & (!is.na(EMPhier[i,6]))) {
    LCDEMP[i] = EMPhier[i,6]   
  } else if ((EMPhier[i,5] %in% FMWThier[,5]) & (EMPhier[i,5] %in% mmhier[,5]) & (!is.na(EMPhier[i,5]))) {
    LCDEMP[i] = EMPhier[i,5]   
  } else if ((EMPhier[i,4] %in% FMWThier[,4]) & (EMPhier[i,4] %in% mmhier[,4]) & (!is.na(EMPhier[i,4]))) {
    LCDEMP[i] = EMPhier[i,4]   
  } else if ((EMPhier[i,3] %in% FMWThier[,3]) & (EMPhier[i,3] %in% mmhier[,3]) & (!is.na(EMPhier[i,3]))) {
    LCDEMP[i] = EMPhier[i,3] 
  }
}
LCDEMP



## use rbind at end to turn LCD vector into column

# now add these LCD columns to the hierarchy list for each data set, then add this back to data set

EMPlist.2<-cbind(EMPhier, LCDEMP) # because we need Taxname with LCD name to attach back to main dataset
View(EMPlist.2)
zooEMP6 = merge(zooEMP5, EMPlist.2[,c(1,9)], by = "Taxname")
View(zooEMP7)
zooEMP7 = zooEMP6[,-c(1)] # remove old Taxname so we just have LCD

# for FMWT:
FMWTlist.2<-cbind(FMWThier, LCDFMWT) # because we need Taxname with LCD name to attach back to main dataset
View(FMWTlist.2)
zooFMWT6 = merge(zooFMWT5, FMWTlist.2[,c(1,9)], by = "Taxname")
View(zooFMWT6)
zooFMWT7 = zooFMWT6[,-c(1)] # remove old Taxname so we just have LCD
View(zooFMWT7)

#for 20mm
mmlist.2<-cbind(mmhier, LCDmm) # because we need Taxname with LCD name to attach back to main dataset
View(mmlist.2)
zoomm = merge(zoop20.4, mmlist.2[,c(1,9)], by = "Taxname")
View(zoomm2)
zoomm2 = zoomm[,-c(1)] # remove old Taxname so we just have LCD


# now combine datasets - first make taxa column have the same name
# combining zoomm, zooFMWT7, and zooEMP7
zooEMP7 = rename(zooEMP7, LCDTax = LCDEMP)
zooFMWT7 = rename(zooFMWT7, LCDTax = LCDFMWT)
zoomm2 = rename(zoomm2, LCDTax = LCDmm)
zooEMP8 = zooEMP7[,-c(1,2,5,10)] #remove factors not in common with 20mm
zooFMWT8 = zooFMWT7[,-c(2,3,6,11)] #remove factors not in common with 20mm
FMWT_EMP_mm = rbind(zooEMP8, zooFMWT8, zoomm2)
View(FMWT_EMP_mm)

#add CPUEs for common taxa within samples
zoocomb = dcast(FMWT_EMP_mm, Date + Station + CondSurf + CondBott + 
                  Secchi + Temperature + Project ~ LCDTax, 
                value.var = "CPUE", fun.aggregate = sum)
View(zoocomb)

