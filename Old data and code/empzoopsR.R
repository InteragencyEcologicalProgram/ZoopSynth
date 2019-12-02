#Compare our 2017 20mm shadow data to EMP's data
library(lubridate)
library(dplyr)
library(ggplot2)
source("zoops.R")

#filter 
zooLit = filter(zoop, Date > "2017-1-1" & Date < "2017-6-20")
zooLit$month = month(zooLit$Date)
zooLit = zooLit[,-c(3,4,5)]

X20mil <- read_excel("20mil.xlsx", sheet = "zoos", 
                     col_types = c("text", "text", "date", 
                                   "text", "text"))
zooLit2 = merge(zooLit, X20mil, by = "ZSampleID")

#quick plot of CPUE by location
zooLitave = group_by(zooLit2, CommonName, Analy, lifestage, Location, month) %>% summarize(CPUE = mean(CPUE, na.rm = T))
zooLitave$anlyLS = paste(zooLitave$Analy, zooLitave$lifestage)

zooLitave =droplevels(zooLitave)
z1 = ggplot(zooLitave, aes(x=month, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = anlyLS)) + scale_fill_manual(values = mypal) + facet_wrap(~Location, scales = "free")





##############################################################################
#get it ready to merge with EMP's data
#Filter out the stations that have no analog in the other survey
zooLit2 = filter(zooLit2, EMPreg!="NA")

#average CPUE by EMP station so we can merge the two data sets
#first add the zeros back in
zooLitc = dcast(zooLit2, ZSampleID + Station + Date+ month+ EMPreg  ~ EMPz, fun.aggregate = sum, value.var = "CPUE")
zooLitm = melt(zooLitc, id = c("ZSampleID", "Station", "month", "Date", "EMPreg"), variable.name = "EMPz", value.name = "CPUE")
zooLitm$survey = rep("FRP", nrow(zooLitm))

#now do the averages
zooLitave2 = group_by(zooLitm, EMPz, Station, EMPreg, survey, month) %>% summarize(CPUE = mean(CPUE, na.rm = T))
zooLitave2 = group_by(zooLitave2, EMPz, EMPreg, survey, month) %>% summarize(CPUE = mean(CPUE, na.rm = T))

zooLitave2 =droplevels(zooLitave2)


ggplot(zooLitave2, aes(x=EMPreg, y= CPUE)) + geom_bar(stat = "identity", aes(fill = EMPz)) +
  facet_wrap(~month)



#filter out the spring 2017 samples
EMP2017x = filter(EMPzoo1, Date > "2017-3-1" & Date < "2017-7-1")
#do the same thing for EMP
EMP2017cx = dcast(merge(EMP2017x, sta2), SurveyCode + EMPzoop + Date +EMPreg ~ EMPz, fun.aggregate = sum, value.var = "CPUE")
EMPm = melt(EMP2017cx, id = c("SurveyCode", "EMPzoop", "Date", "EMPreg"), variable.name = "EMPz", value.name = "CPUE")
EMPm$survey = rep("EMP", nrow(EMPm))
EMPm$CPUE[which(is.nan(EMPm$CPUE))] = 0


EMPm$month = month(EMPm$Date)
EMPmsumx = group_by(EMPm, EMPz, EMPzoop, EMPreg, survey, month) %>% summarize(CPUE = mean(CPUE, na.rm = T))

#Filter out the stations that have no analog in the other survey
EMPmsumx = filter(EMPmsumx, EMPzoop == "NZ028" | EMPzoop == "NZ054"| EMPzoop == "NZ032"| EMPzoop == "NZ060"| EMPzoop == "NZ064"
)
EMPmsumx = rename(EMPmsumx, Station = EMPzoop)

ggplot(EMPmsumx, aes(x=EMPreg, y= CPUE)) + geom_bar(stat = "identity", aes(fill = EMPz)) +
  facet_wrap(~month)


#merge the two data sets
zooAlllit = rbind(zooLitave2, EMPmsumx)
#filter all the organisms that have no analog
zooAlllit = filter(zooAlllit, !is.na(EMPz))
zooAlllit = filter(zooAlllit, EMPz !="NA")
#add teh "analy" column back
zooptaxa = read.csv("zooptaxa.csv")
zooAlllit1 = merge(zooAlllit, zooptaxa)
zooAlllit1$AnalyLS = paste(zooAlllit1$Analy, zooAlllit1$lifestage)

#try summing by anaaly
zootest = group_by(zooAlllit1, Analy, month, Station, EMPreg, survey) %>% summarize(CPUE = sum(CPUE))


months <- c("3" ="March", "4"="April", "5"="May", "6"="June")
zooAlllit1$EMPreg = factor(zooAlllit1$EMPreg, levels = c("Horseshoe","broadslough", "browns","Grizzly", "Suisun" ),
                           labels = c("Horseshoe","Broad", "Confluence","Grizzly", "Suisun" ))

#Plot them all together
#quick plot of CPUE by location
z2l = ggplot(zooAlllit1, aes(x=EMPreg, y= CPUE))
z2l + geom_bar(stat = "identity", aes(fill = survey), position = "dodge") + 
  scale_fill_manual(values = mypal) + 
  facet_wrap(~EMPz, scales = "free_y")

z2.1 = ggplot(zooAlllit1, aes(x=survey, y= CPUE))
z2.1 + geom_bar(stat = "identity", aes(fill = EMPz)) + 
  #scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month,  scales = "free", labeller = labeller(month = months))

z2.1 + geom_bar(stat = "identity", aes(fill = Analy)) + 
  scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month, scales = "free", labeller = labeller(month = months))


z2.1 + geom_bar(stat = "identity", aes(fill = AnalyLS)) + 
  scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month, scales = "free", labeller = labeller(month = months))

ztest = ggplot(zootest, aes(x=survey, y = CPUE))
ztest + geom_bar(stat = "identity", aes(fill = Analy))+
  scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month, scales = "free", labeller = labeller(month = months))


z2.1 + geom_bar(stat = "identity", aes(fill = EMPz), position = "fill") + 
  #scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month,  scales = "free", labeller = labeller(month = months))

z2.1 + geom_bar(stat = "identity", aes(fill = AnalyLS), position = "fill") + 
  scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month, scales = "free", labeller = labeller(month = months)) +
  theme(axis.text.x = element_text(size = 20), 
        strip.text = element_text(size = 20), axis.title = element_blank())

z2.1 + geom_bar(stat = "identity", aes(fill = AnalyLS)) + 
  scale_fill_manual(values = mypal) + 
  facet_grid(EMPreg~month, scales = "free", labeller = labeller(month = months)) +
  theme(axis.text.x = element_text(size = 20), 
        strip.text = element_text(size = 20), axis.title = element_blank())

z3 = ggplot(zooAlllit1, aes(x=month, y = CPUE))
z3 + geom_area(aes(fill = Analy), position = "fill")+ facet_wrap(survey~EMPreg)


###################################################################################

#CPUE GLM
zooAlls = group_by(zooAlllit1, Station, survey, EMPreg, month) %>% summarize(tcatch = sum(CPUE))
zooAlls = group_by(zooAlls, survey, EMPreg, month) %>% summarize(mcatch = mean(tcatch, na.rm = T))


ggplot(zooAlls, aes(x = survey, y = mcatch)) + geom_bar(stat = "identity") + 
  facet_grid(EMPreg~month,  scales = "free", 
             labeller=labeller(month = months)) 


g1 = glm(log(mcatch + 1)~survey+EMPreg + month, data = zooAlls)
summary(g1)

#overall, FRP catches less, Suisun has higher catch, and catch gets higher later in the season.
library(visreg)
visreg(g1)

#################################################################################
#permanova
zooAllMat = dcast(zooAlllit, Station + survey + EMPreg + month ~ EMPz, value.var = "CPUE", fun.aggregate = sum)
#Source NMDS plotting function
source("plotNMDS.R")

library(vegan)
a1 = adonis(zooAllMat[,5:42]~ survey + EMPreg + month, data = zooAllMat)
a1
a1$coefficients

library(indicspecies)
# This tests to see whether some groups are indicators of particular sites
indi1 = multipatt(zooAllMat[,5:42], zooAllMat$EMPreg, func = "r.g", control = how(nperm=999))
summary(indi1)

#This looks at all the species to see if they are associated with one or another group
prefsign = signassoc(zooAllMat[,5:42], cluster = zooAllMat$EMPreg, alternative = "two.sided",
                     control = how(nperm=999))
prefsign


indi2 = multipatt(zooAllMat[,5:42], zooAllMat$survey, func = "r.g", control = how(nperm=999))
summary(indi2)

#This looks at all the species to see if they are associated with one or another group
prefsign2 = signassoc(zooAllMat[,5:42], cluster = zooAllMat$survey, alternative = "two.sided",
                      control = how(nperm=999))
prefsign2


#plot an NMDS
zooAllMat$EMPreg = as.factor(zooAllMat$EMPreg)
zooAllMat$survey = as.factor(zooAllMat$survey)
zooAllMat$month = as.factor(zooAllMat$month)
NMDS2 = metaMDS(zooAllMat[,5:42], trymax=500, k=2)
PlotNMDS(NMDS2, data = droplevels(zooAllMat), textp = F, group = "EMPreg")
PlotNMDS(NMDS2, data = droplevels(zooAllMat), textp = T, group = "survey")
PlotNMDS(NMDS2, data = droplevels(zooAllMat), textp = T, group = "month")

#####################################################################################################
#try it with relative abundance
zooAllMatp = zooAllMat[,5:42]/ rowSums(zooAllMat[,5:42]) 

a1x = adonis(zooAllMatp~ survey + EMPreg + month, data = zooAllMat)
a1x
a1$coefficients
#Dang. The surveys really arn't different. Interactions don't do much either



library(indicspecies)
# This tests to see whether some groups are indicators of particular sites
indi1 = multipatt(zooAllMatp, zooAllMat$EMPreg, func = "r.g", control = how(nperm=999))
summary(indi1)

#This looks at all the species to see if they are associated with one or another group
prefsign = signassoc(zooAllMatp, cluster = zooAllMat$EMPreg, alternative = "two.sided",
                     control = how(nperm=999))
prefsign


indi2 = multipatt(zooAllMatp, zooAllMat$survey, func = "r.g", control = how(nperm=999))
summary(indi2)

#This looks at all the species to see if they are associated with one or another group
prefsign2 = signassoc(zooAllMatp, cluster = zooAllMat$survey, alternative = "two.sided",
                      control = how(nperm=999))
prefsign2


#plot an NMDS

NMDS2 = metaMDS(zooAllMatp, trymax=500, k=2)
PlotNMDS(NMDS2, data = droplevels(zooAllMat), textp = T, group = "EMPreg")
PlotNMDS(NMDS2, data = droplevels(zooAllMat), textp = F, group = "survey")
PlotNMDS(NMDS2, data = droplevels(zooAllMat), textp = T, group = "month")

####################################################################################
#let's just look at adults
adults = filter(zooAlllit1, lifestage == "adult")

ads = group_by(adults, Station, survey, EMPreg, month) %>% summarize(tcatch = sum(CPUE))
ads = group_by(ads, survey, EMPreg, month) %>% summarize(mcatch = mean(tcatch, na.rm = T))


ggplot(ads, aes(x = survey, y = mcatch)) + geom_bar(stat = "identity") + 
  facet_grid(EMPreg~month,  scales = "free", 
             labeller=labeller(month = months)) 


ag1 = glm(log(mcatch + 1)~survey+EMPreg + month, data = ads)
summary(ag1)

#overall, FRP catches less, Suisun has higher catch, and catch gets higher later in the season.
library(visreg)
visreg(g1)

#################################################################################
#permanova
adMat = dcast(adults, Station + survey + EMPreg + month ~ EMPz, value.var = "CPUE", fun.aggregate = sum)

a1 = adonis(adMat[,5:30]~ survey + EMPreg + month, data = adMat)
a1
a1$coefficients

# This tests to see whether some groups are indicators of particular sites
indi1 = multipatt(adMat[,5:30], adMat$EMPreg, func = "r.g", control = how(nperm=999))
summary(indi1)

#plot an NMDS
adMat$EMPreg = as.factor(adMat$EMPreg)
adMat$survey = as.factor(adMat$survey)
adMat$month = as.factor(adMat$month)
NMDS2 = metaMDS(zooAllMat[,5:42], trymax=500, k=2)
PlotNMDS(NMDS2, data = droplevels(adMat), textp = F, group = "EMPreg")
PlotNMDS(NMDS2, data = droplevels(adMat), textp = T, group = "survey")

#####################################################################################################
#try it with relative abundance
adMatp = adMat[,5:30]/ rowSums(adMat[,5:30]) 

a1 = adonis(adMatp~ survey + EMPreg + month, data = adMat)
a1
a1$coefficients
#Surveys still aren't different with just the adults.

#plot an NMDS

NMDS2 = metaMDS(adMatp, trymax=500, k=2)
PlotNMDS(NMDS2, data = droplevels(adMat), textp = F, group = "EMPreg")
PlotNMDS(NMDS2, data = droplevels(adMat), textp = T, group = "survey")
PlotNMDS(NMDS2, data = droplevels(adMat), textp = T, group = "month")


###############################################################################
#Just adult copepods


copA = filter(adults, Analy == "calanoid"| Analy == "cyclopoid" | Analy == "copepod")


copA = droplevels(copA)



#quick plot of CPUE by location
c1 = ggplot(copA, aes(x=survey, y= CPUE))
c1 + geom_bar(stat = "identity", aes(fill = EMPz), position = "fill") + 
  scale_fill_manual(values = mypal) + facet_wrap(~EMPreg, scales = "free_x")
c1 + geom_bar(stat = "identity", aes(fill = EMPz)) + 
  scale_fill_manual(values = mypal) + facet_grid(EMPreg~month, scales = "free")


#multivariates
cMat = dcast(copA, Station + survey + EMPreg + month ~ EMPz, value.var = "CPUE", fun.aggregate = sum)
cMatp = cMat[,5:18]/ rowSums(cMat[,5:18]) 

a2 = adonis(cMat[,5:18]~ survey + EMPreg + month, data = cMat)
a2
a2 = adonis(cMatp~ survey + EMPreg + month, data = cMat)
a2

#plot an NMDS

NMDS2 = metaMDS(cMatp, trymax=500, k=2)
cMat$EMPreg = as.factor(cMat$EMPreg)
cMat$survey = as.factor(cMat$survey)
cMat$month = as.factor(cMat$month)
PlotNMDS(NMDS2, data = droplevels(cMat), textp = F, group = "EMPreg")
PlotNMDS(NMDS2, data = droplevels(cMat), textp = T, group = "survey")
PlotNMDS(NMDS2, data = droplevels(cMat), textp = T, group = "month")

#CPUE GLM
copss = group_by(copA, Station, survey, EMPreg, month) %>% summarize(tcatch = sum(CPUE))
copss = group_by(copss, survey, EMPreg, month) %>% summarize(mcatch = mean(tcatch, na.rm = T))

cg1 = glm(log(mcatch + 1)~survey+EMPreg + month, data = copss)
summary(cg1)

