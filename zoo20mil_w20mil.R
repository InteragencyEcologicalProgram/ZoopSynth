#Clean version of our zooplankton data that was taken concurrently with 20 mm in 2017

#Look at the all the 20mm comparison samples
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)
library(RColorBrewer)
library(visreg)

#import the data
library(readxl)
zoopsfrp <- read_excel("zoopsfrp.xlsx", col_types = c("numeric", 
                                                      "text", "text", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "text", "text", "text", "text", "numeric", 
                                                      "numeric", "numeric", "date", "text", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "numeric"))
View(zoopsfrp)

################################################################################################################################
#First, lots of data manipulation

#Calculate CPUE

#combine the slides
totalcells = group_by(zoopsfrp, ZSampleID) %>% 
  summarise(totcells = max(CellNumber), totCountall = sum(Count))

zoo = group_by(zoopsfrp, Station, Location, Date, ZSampleID, Dilution, ZooCode, CommonName,
               Phylum, Class, Order, Family,Genus, Species, LifeStage, MeterStart2, MeterEnd2) %>% 
  summarise(totCount = sum(Count))

zoo = merge(zoo, totalcells)

#adjust for subsampling
zoo$atotal = (zoo$totCount/zoo$totcells)*zoo$Dilution

#Volume sampled is the difference in flowmeter readings times the flowmeter constant
#times the mouth area
zoo$volume = (zoo$MeterEnd2-zoo$MeterStart2)*0.026873027*0.0167


#volume for samples where the flowmeter turned over to zero.
zoo$volume[which(zoo$volume<0)] = ((1000000 - zoo$MeterStart2[which(zoo$volume<0)])+
                                     zoo$MeterEnd2[which(zoo$volume<0)])*0.026873027*0.0167 #I've used the factory calibration here

#some of the samples iddn't have flowmeters, so we'll use the average volume for those
zoo$volume[which(is.na(zoo$volume))] = mean(zoo$volume, na.rm = T)

#Calculate CPUE (total critters divided by volume sampled)
zoo$CPUE = zoo$atotal/zoo$volume


#import analysis categories to make it easier to deal with
frptaxa <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "allfrptaxa")

#attach the other categories to the main data set
zoop = merge(zoo, frptaxa)

#There were hardly any cumaceans, so I'm going to lump them in with "other"
#zoop$Analy[which(zoop$CommonName=="Cumaceans")] = "other"

#Fix a few of the common names
zoop$CommonName[which(zoop$CommonName== "Cladocera")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Simocephalus")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Sida")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Ilyocryptus")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Chydorus")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Moina")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Branchionus")] = "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Kellicottia")] =  "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Keratella")] =  "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Platyias")] =  "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Diacyclops")] = "Cyclopoid Other"
zoop$CommonName[which(zoop$CommonName== "Eucyclops")] = "Cyclopoid Other"
zoop$CommonName[which(zoop$CommonName== "Halicyclops")] = "Cyclopoid Other"

#remove categories not counted by 20mm
zoop = filter(zoop, Analy != "mollusca" &Analy != "mysid" &Analy != "annelid" &
                Analy != "terrestrial", Analy != "collembola" &Analy != "insect" &
                Analy != "Amphipod" &Analy != "gammarid" &
              Analy != "corophium"&Analy != "isopod" & CommonName != "Corixidae"& 
                CommonName != "Ceratopogonidae larvae" & CommonName != "Nematode" &
                 CommonName != "Mite"& CommonName != "Hydra")


#filter out just the samples from last spring
zooLit = filter(zoop, Date > "2017-1-1" & Date < "2017-6-20")

#create a new "month" variable
zooLit$month = month(zooLit$Date)
zooLit = zooLit[,-c(3,4,5)]


#Import information on the samples that were taken in conjunction with the 20mm survey
X20mil <- read_excel("20mil.xlsx", sheet = "zoos", 
                         col_types = c("text", "text", "date", 
                                              "text", "text", "numeric", "numeric"))

#subset just the samples taken in conjunction with 20mm
zooLit2 = merge(zooLit, X20mil, by = "ZSampleID")

#create a new variable that is a combination of analysis group and life stage
zooLit2$anlyLS = paste(zooLit2$Analy, zooLit2$lifestage)


# THere was one sample with a rediculously huge number of rotifers
#I'm going to remove the wierd rotifer sample from Lindsey, 'cause it's wierd.
#zooLit2 = filter(zooLit2, ZSampleID != "ZOOP1-7JUN2017")



############################################################################################################
#now some exploritory plots


#set up a color pallete
mypal = c(brewer.pal(9, "Set1"), brewer.pal(12, "Set3"), "black", "white", "grey", "pink")

#quick plot of CPUE by location

#First calculate the average CPUE of each critter (analysis group/life stage) by location and month
zooLitx = group_by(zooLit2, ZSampleID, anlyLS, Analy, lifestage, Station, month) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))
zooLitave = group_by(zooLitx, anlyLS, Analy, lifestage, Station, month) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T))
zooLitave =droplevels(zooLitave)

#set up labels
zoolabs = c("Amphipoda", "Annelida", "Calanoida","Cal juv", "Cal nauplii", 
            "Cladocera", "Collembola", "Copepoda nauplii", "Corophiidae",
            "Cyclopoda", "Cyclopoid juv", "fish", "Gammaridea", "Harpacticoida",
            "Insecta", "Insect larvae", "Isopoda", "Mollusca", "Mysidea", "Ostracoda", "other",
            "Rotifera", "terrestrial")

#Now a bar plot
z1 = ggplot(zooLitave, aes(x=month, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = mypal, labels = zoolabs, name = NULL) + 
  facet_wrap(~Station, scales = "free_y") +
#  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "bottom")

##############################################################################
#Make some summeries of our data, but don't compare to any of IEP's data because
#we don't have the 20mm data yet.

#Overall CPUE GLM

#First calculate total zoop CPUE per sample
zooLitsum = group_by(zooLit2, ZSampleID, Station, Location, month, Ggdist) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))

#Model log-transformed catch against time (month) and distance from the Golden Gate
g1 = lm(log(CPUE)~month+Ggdist, data = zooLitsum)
summary(g1)
#both terms are highly significant. Catch increases over time and increases in fresher water

#check out the diagnostic plots
plot(g1)
#looks good!

#You can look at the partial residuals plots
visreg(g1)

##############################################################################
#multivariate stats

#Create a community matrix
Commat = dcast(zooLit2, ZSampleID~anlyLS, value.var = "CPUE", fun.aggregate = sum)
Commat = Commat[,2:24]

#relative abundance amtrix
Commatp = Commat/rowSums(Commat)

#make better names for adding to the NMDS plots
names(Commat) =  c("Amphipoda", "Annelida", "Calanoida","Cal juv", "Cal naup", 
                   "Cladocera", "Collembola", "cop naup", "Corophiidae",
                   "Cyclopoda", "Cyclo juv", "fish", "Gammaridea", "Harpacticoida",
                   "Insecta", "Insect larvae", "Isopoda", "Mollusca", "Mysidea", "Ostracoda", "other",
                   "Rotifera", "terrestrial")
names(Commatp) = names(Commat)


#PerMANOVA of abundance matrix versus distance from golden gate and month
adonis(Commat~month + Ggdist, data = zooLitsum)
#both terms are significant

#Do it again with relative abundance
a1 = adonis(Commatp~month + Ggdist, data = zooLitsum)
a1
#looks pretty similar

#now some non=metric multidimentional scaling
n1 = metaMDS(Commat)
n2 = metaMDS(Commatp, trymax = 100)
#Both NMDS work pretty well, but the relelative abundance one is a better fit
#I'm using the GLM of total catch anyway, so we will go with relative abundance from now on.

#Show the NMDS plots
source("plotNMDS.R")
#do a quick plot with hulls by location
PlotNMDS(n2, data = zooLitsum, group = "Location")
#there are definitely differences, but they are a bit of a mess

#use surfaces instead of hulls to show distance from the golden gate and month of collection
PlotNMDS(n2, data = zooLitsum, group = "")
#just ignore the error messages

f = ordisurf(n2~Ggdist, data = zooLitsum, add = T, labcex = 1)
f2 = ordisurf(n2~as.numeric(month), col = "blue", data = zooLitsum, add = T, 
             levels = c(1,2,3,4,5, 6), labcex = 1)
legend(par("usr")[1],par("usr")[3], 
       c("Distance from GG", "Month"),
       col = c("red", "blue"), lty = c(1,1))

#Check out the GAMs that gave us those isoclines
summary(f) #distace from GG
summary(f2) #month of year
#Results are similar to the PerMANOVA, but with a better fit (since they are smooth terms, not linear)

#################################################################################################
#Now let's import the 20mm data and do fun stuff with it
zoopquery20mm <- read_excel("zoopquery20mm.xlsx", 
                            col_types = c("date", "numeric", "skip", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text"))

#create a single sample ID
zoopquery20mm$ZSampleID = paste(zoopquery20mm$Station20mm, zoopquery20mm$SampleDate)
#Calculate CPUE

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

#import analysis categories to make it easier to deal with
taxa20 <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "attatch20mm")
taxa20$CommonName = taxa20$`20mm`

#attach the other categories to the main data set
zoop20 = merge(zoo20, taxa20)

#Subset just the dates and samples we are interested in
zoop20 = filter(zoop20, SampleDate > "2017-1-1" & SampleDate < "2017-6-30")
zoop20 = filter(zoop20, Station20mm == 703 |
                  Station20mm ==602|
                  Station20mm ==609|
                  Station20mm ==705|
                  Station20mm ==706|
                  Station20mm ==720|
                  Station20mm ==724|
                  Station20mm ==726|
                  Station20mm ==801)

#Make the column names match so we can merge the data sets
names(zooLit2)
zooLit3 = zooLit2[,c(1,2, 19, 21, 23, 28, 31, 32)]
zooLit3$survey = "FRP"


names(zoop20)
zoop20$CommonName = zoop20$FRP
zoop20$Date = zoop20$SampleDate
zoop20$SampleDate = NULL
zoop20$anlyLS = paste(zoop20$Analy, zoop20$lifestage)
zoop20.1 = zoop20[,c(1,2,3,13,16, 17, 18,19)]
zoop20.1$survey = "20mm"

allzoops = rbind(zoop20.1, zooLit3)
allzoops$month = month(allzoops$Date)

#re-organize the "other" category

allzoops$anlyLS = paste(allzoops$Analy, allzoops$lifestage)
allzoops$anlyLS[which(allzoops$anlyLS == "other adult")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "other juv")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "other nauplii")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "insect adult")] = "other"

#import distances from the Golden Gate (approximate)
stations20mm <- read_excel("stations20mm.xlsx")
allzoops = merge(allzoops, stations20mm)
 
############################################################################################################
#now some exploritory plots

#quick plot of CPUE by location

#First calculate the average CPUE of each critter (analysis group/life stage) by location and month
zoop20x= group_by(allzoops, ZSampleID, anlyLS, Analy, lifestage, Station20mm, month, survey) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))
zooLitave = group_by(zoop20x, anlyLS, Analy, lifestage, Station20mm, month, survey) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T))
zooLitave =droplevels(zooLitave)

#set up labels
zoolabs = c("Calanoida","Cal juv", "Cal nauplii", 
            "Cladocera", "Copepoda nauplii", "cumaceans",
            "Cyclopoda", "Cyclopoid juv", "decapod juv", "fish",  "Harpacticoida",
           "Ostracoda", "other",
            "Rotifera")

#Now a bar plot
z1 = ggplot(zooLitave, aes(x=survey, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = anlyLS), position = "fill") + 
 scale_fill_manual(values = c(mypal, "white", "green"), name = NULL, labels = zoolabs) + 
  facet_grid(Station20mm~month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")


#look at it one month at a time
#march
zmarch = ggplot(filter(zooLitave, month == 3), aes(x=survey, y= CPUE))
zmarch + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL, labels = zoolabs) + 
  facet_grid(Station20mm~., scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")


#april
zapril = ggplot(filter(zooLitave, month == 4), aes(x=survey, y= CPUE))
zapril + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL, labels = zoolabs) + 
  facet_grid(Station20mm~., scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

#may
zmay = ggplot(filter(zooLitave, month == 5), aes(x=survey, y= CPUE))
zmay + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL, labels = zoolabs) + 
  facet_grid(Station20mm~., scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

#june
zjune = ggplot(filter(zooLitave, month == 6), aes(x=survey, y= CPUE))
zjune + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Station20mm~., scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

###########################################################################################
#stats
#Overall CPUE GLM

#try taking out rotifers
allzoops = filter(allzoops, Analy != "rotifera")

#First calculate total zoop CPUE per sample
zoosum = group_by(allzoops, ZSampleID, Station20mm, month, survey, Ggdist) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))

#Model log-transformed catch against time (month) and station
g1s = lm(log(CPUE)~month+survey + Ggdist, data = zoosum)
summary(g1s)
#Catch increases over time but no difference in catch between surveys

#check out the diagnostic plots
plot(g1s)
#looks good!

#You can look at the partial residuals plots
visreg(g1s)

##############################################################################
#multivariate stats

#Create a community matrix
Commat = dcast(allzoops, ZSampleID~anlyLS, value.var = "CPUE", fun.aggregate = sum)
Commat = Commat[,2:14]

#relative abundance amtrix
Commatp = Commat/rowSums(Commat)

#make better names for adding to the NMDS plots
names(Commat) =  c("Calanoida","Cal juv", "Cal naup", 
                   "Cladocera", "cop naup",  "cumaceans",
                   "Cyclopoda", "Cyclo juv", "Decapoda", "fish", "Harpacticoida",
                    "Ostracoda", "other")
names(Commatp) = names(Commat)


#PerMANOVA of abundance matrix survey and month
adonis(Commat~month + survey + Station20mm, data = zoosum)

adonis(Commat~month + survey + Ggdist, data = zoosum)

#Do it again with relative abundance
a1 = adonis(Commatp~month + survey+ Station20mm, data = zoosum)
a1
#looks pretty similar

adonis(Commatp~month + survey+ Ggdist, data = zoosum)

#now some non=metric multidimentional scaling
n1 = metaMDS(Commat, trymax = 300)
n2 = metaMDS(Commatp, trymax = 100)
#Both NMDS work pretty well, but the relelative abundance one is a better fit
#I'm using the GLM of total catch anyway, so we will go with relative abundance from now on.

#Show the NMDS plots
source("plotNMDS.R")
zoosum$survey = as.factor(zoosum$survey)
zoosum$Station20mm = as.factor(zoosum$Station20mm)
zoosum$month = as.factor(zoosum$month)
#do a quick plot with hulls by location
PlotNMDS(n2, data = zoosum, group = "survey")
#there are definitely differences, but they are a bit of a mess
PlotNMDS(n2, data = zoosum, group = "Station20mm")
PlotNMDS(n2, data = zoosum, group = "month")


#use surfaces instead of hulls to show distance from the golden gate and month of collection
PlotNMDS(n2, data = zoosum, group = "")
#just ignore the error messages

f = ordisurf(n2~Ggdist, data = zoosum, add = T, labcex = 1)
#zoosum$month = as.numeric(zoosum$month)
f2 = ordisurf(n2~month, col = "blue", data = zoosum, add = T, 
              nlevels = 2, labcex = 1)
legend(par("usr")[1],par("usr")[3], 
       c("Distance from GG", "Month"),
       col = c("red", "blue"), lty = c(1,1))
summary(f)
summary(f2)


#######################################################################################
#what if we tried this with common names instead of anlaysis groups?

#Create a community matrix
Commat2 = dcast(allzoops, ZSampleID~CommonName, value.var = "CPUE", fun.aggregate = sum)
Commat2 = Commat2[,2:38]

#relative abundance amtrix
Commatp2 = Commat2/rowSums(Commat2)

#PerMANOVA of abundance matrix survey and month
adonis(Commat2~month + survey + Station20mm, data = zoosum)

#Do it again with relative abundance
a1 = adonis(Commatp2~month + survey+ Station20mm, data = zoosum)
a1
#looks pretty similar

#now some non=metric multidimentional scaling
n12 = metaMDS(Commat2, trymax = 200)
n22 = metaMDS(Commatp2, trymax = 500)


#do a quick plot with hulls by location
PlotNMDS(n12, data = zoosum, group = "survey", textp = F)
#there are definitely differences, but they are a bit of a mess
PlotNMDS(n12, data = zoosum, group = "Station20mm", textp = F)
PlotNMDS(n12, data = zoosum, group = "month", textp = F)

#do a quick plot with hulls by location
PlotNMDS(n22, data = zoosum, group = "survey", textp = F)
#there are definitely differences, but they are a bit of a mess
PlotNMDS(n22, data = zoosum, group = "Station20mm", textp = F)
PlotNMDS(n22, data = zoosum, group = "month", textp = F)

#quick plot of catches by common name
ggplot(allzoops, aes(x=survey, y=CPUE)) +geom_bar(stat = "identity") +
  facet_wrap(~CommonName, scales = "free")

#use surfaces instead of hulls to show distance from the golden gate and month of collection
PlotNMDS(n12, data = zoosum, group = "")
#just ignore the error messages

f = ordisurf(n12~Ggdist, data = zoosum, add = T, labcex = 1)
f2 = ordisurf(n12~as.numeric(month), col = "blue", data = zoosum, add = T, 
              levels = c(1,2,3,4,5, 6), labcex = 1)
legend(par("usr")[1],par("usr")[3], 
       c("Distance from GG", "Month"),
       col = c("red", "blue"), lty = c(1,1))
summary(f)
summary(f2)
