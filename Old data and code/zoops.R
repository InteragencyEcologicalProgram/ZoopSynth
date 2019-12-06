#zooplankton
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(RColorBrewer)

zoopsfrp <- read_excel("zoopsfrp.xlsx", col_types = c("numeric", 
                                                      "text", "text", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "text", "text", "text", "text", "numeric", 
                                                      "numeric", "numeric", "date", "text", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "numeric"))
View(zoopsfrp)

################################################################################################################################
#Calculate CPUE, and look at them by station so we can compare them to EMP

# Effort for benthic and oblique trawls is based on flowmeter readings and net mouth area

#combine the slides
totalcells = group_by(zoopsfrp, ZSampleID) %>% 
  summarise(totcells = max(CellNumber), totCountall = sum(Count))

zoo = group_by(zoopsfrp, Station, Location, Date, ZSampleID, Dilution, ZooCode, CommonName,
                Phylum, Class, Order, Family,Genus, Species, LifeStage, MeterStart2, MeterEnd2) %>% 
  summarise(totCount = sum(Count))

zoo = merge(zoo, totalcells)

#adjust for subsampling
zoo$atotal = (zoo$totCount/zoo$totcells)*zoo$Dilution

#Volume sampled
zoo$volume = (zoo$MeterEnd2-zoo$MeterStart2)*0.026873027*0.0167


#volume for samples where the flowmeter turned over to zero.
zoo$volume[which(zoo$volume<0)] = ((1000000 - zoo$MeterStart2[which(zoo$volume<0)])+
                                     zoo$MeterEnd2[which(zoo$volume<0)])*0.026873027*0.0167 #I've used the factory calibration here

#some of the samples iddn't have flowmeters, so we'll use the average volume for those
zoo$volume[which(is.na(zoo$volume))] = mean(zoo$volume, na.rm = T)

#Calculate CPUE
zoo$CPUE = zoo$atotal/zoo$volume


#figuer out how many critters total
tot = sum(zoo$CPUE)

#Now how many critters per taxon
zooptax = group_by(zoo, CommonName) %>% summarise(totaltaxa = sum(CPUE, na.rm = T), 
                                                  prop = totaltaxa/sum(zoo$CPUE, na.rm = T), 
                                                  tax = CommonName[1])

#lump the very rare taxa into "other
#zooptax$tax[which(zooptax$prop < 0.005)] = "other"

#put the new taxonomic designations onto the dataset
#zoo = merge(zoo, zooptax)

#import some other categories
frptaxa <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "allfrptaxa")

zoop = merge(zoo, frptaxa)

#Now let's look at what we got!
#############################################################################################################

#filter for just 2017 samples, spring blitz.
zoo2017 = filter(zoop, Date > "2017-3-1" & Date < "2017-5-1")



#set up a color pallete
mypal = c(brewer.pal(9, "Set1"), brewer.pal(12, "Set3"), "black", "white", "grey", "pink")



#quick plot of CPUE by location
zoo2017ave = group_by(zoo2017, Analy, CommonName, Location) %>% summarize(CPUE = mean(CPUE, na.rm = T))
zoo2017ave =droplevels(zoo2017ave)
z1 = ggplot(zoo2017ave, aes(x=Location, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = Analy)) + scale_fill_manual(values = mypal)

############################################################################################
#print all the station and location info so we can line it up with EMP and 20mil
stations = group_by(zoo2017, Location, Station) %>% summarise(n = length(Location))

#read in our stations crosswalk
sta = read.csv("stations.csv")

#read in the EMP data from this past spring
EMPzoo <- read.csv("EMPzoops.csv")
EMPzoo$Date = as.Date(EMPzoo$Date, format = "%m/%d/%Y")
EMPzoo$EMPz = EMPzoo$EMP

#import the analysis groups
emptaxa <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "attachtoEMP")
EMPzoo1 = merge(EMPzoo, emptaxa, all.x = F)

#filter out the spring 2017 samples
EMP2017 = filter(EMPzoo1, Date > "2017-3-1" & Date < "2017-5-1")

#quick plot of CPUE by station
ggplot(EMP2017, aes(x=SurveyCode, y= CPUE)) + geom_bar(stat = "identity") + facet_wrap(~EMPzoop, scales = "free")

#Add their station codes to our data
zoo2017$FRPStation = zoo2017$Station
zoo2017 = merge(zoo2017, sta, by = "FRPStation")


#Filter out the stations that have no analog in the other survey
zoo2017 = filter(zoo2017, !is.na(EMPzoop))

#########################################################################
#first do the lowest level of taxonomic resolution

#average CPUE by EMP station so we can merge the two data sets
#first add the zeros back in
zooEMPc = dcast(zoo2017, ZSampleID + Site + Date+ EMPreg  ~ EMPz, fun.aggregate = sum, value.var = "CPUE")
zooEMPm = melt(zooEMPc, id = c("ZSampleID", "Site", "Date", "EMPreg"), variable.name = "EMPz", value.name = "CPUE")
zooEMPm$survey = rep("FRP", nrow(zooEMPm))

#now do the averages
zooEMP = group_by(zooEMPm, EMPz, Site, EMPreg, survey) %>% summarize(CPUE = mean(CPUE, na.rm = T))
zooEMP =droplevels(zooEMP)

#add our regions
sta2 = read.csv("stations2.csv")

#do the same thing for EMP
EMP2017c = dcast(merge(EMP2017, sta2), SurveyCode + EMPzoop + Date +EMPreg ~ EMPz, fun.aggregate = sum, value.var = "CPUE")
EMPm = melt(EMP2017c, id = c("SurveyCode", "EMPzoop", "Date", "EMPreg"), variable.name = "EMPz", value.name = "CPUE")
EMPm$survey = rep("EMP", nrow(EMPm))
EMPm$CPUE[which(is.nan(EMPm$CPUE))] = 0

EMPmsum = group_by(EMPm, EMPz, EMPzoop, EMPreg, survey) %>% summarize(CPUE = mean(CPUE, na.rm = T))

#Filter out the stations that have no analog in the other survey
EMPmsum = filter(EMPmsum, EMPzoop == "NZ028" | EMPzoop == "NZ054"| EMPzoop == "NZ060"| EMPzoop == "NZ064"|
                  EMPzoop == "NZ074"| EMPzoop == "NZ086"| EMPzoop == "NZ032")
EMPmsum = rename(EMPmsum, Site = EMPzoop)



#merge the two data sets
zooAll = rbind(zooEMP, EMPmsum)

zooptaxa = read.csv("zooptaxa.csv")
#filter all the organisms that have no analog
zooAll = filter(zooAll, !is.na(EMPz))
zooAll = filter(zooAll, EMPz != "NA")
zooAll = merge(zooAll, zooptaxa)


#Plot them all together
#quick plot of CPUE by location
z2 = ggplot(zooAll, aes(x=Site, y= CPUE))
z2 + geom_bar(stat = "identity", aes(fill = survey), position = "dodge") + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPz, scales = "free_y")

z2.1 = ggplot(zooAll, aes(x=Site, y= CPUE))
z2.1 + geom_bar(stat = "identity", aes(fill = EMPz)) + 
  #scale_fill_manual(values = mypal) + 
  facet_wrap(~EMPreg,  scales = "free")

#########################################################################
#now higher level of taxonomic resolution

z2 = ggplot(zooAll, aes(x=Site, y= CPUE))
z2 + geom_bar(stat = "identity", aes(fill = Analy)) + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPreg, scales = "free")

################################################################################
#now higher level of taxonomic resolution + life stage

zooAll$AnalyLS = paste(zooAll$Analy, zooAll$lifestage)

#Plot them all together
#quick plot of CPUE by location
z3 = ggplot(zooAll, aes(x=Site, y= CPUE))
z3 + geom_bar(stat = "identity", aes(fill = AnalyLS)) + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPreg, scales = "free")

#################################################################################
#now let's just look at copepods

zooAllz = merge(zooAll, emptaxa)

cops = filter(zooAllz, Analy == "calanoid"| Analy == "cyclopoid" | Analy == "copepod")
foo = group_by(cops, EMPz) %>% summarize(tot = sum(CPUE))
cops2 = filter(merge(cops, foo), tot >0)

cops2 = droplevels(cops2)
#seperate adults and others
copsA = filter(cops2, lifestage == "adult")
copsj = filter(cops2, lifestage == "juv" | lifestage == "nauplii")



#quick plot of CPUE by location
c1 = ggplot(copsA, aes(x=Site, y= CPUE))
c1 + geom_bar(stat = "identity", aes(fill = EMPz), position = "fill") + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPreg, scales = "free_x")
c1 + geom_bar(stat = "identity", aes(fill = EMPz)) + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPreg, scales = "free")


#quick plot of CPUE by location
c2 = ggplot(copsj, aes(x=Site, y= CPUE))
c2 + geom_bar(stat = "identity", aes(fill = EMPz), position = "fill") + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPreg, scales = "free_x")

########################################################################
#CPUE GLM
zooAlls = group_by(zooAll, Site, survey, EMPreg) %>% summarize(tcatch = sum(CPUE))

g1 = glm(tcatch~survey+EMPreg, data = zooAlls)
summary(g1)
