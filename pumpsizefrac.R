#see if we can look at the EMP pump samples to determine what critters from the 43 micron mesh
#get left out of the 150 micron mesh.
library(lubridate)
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyverse)
library(reshape2)

EMPpump <- read.csv("~/ZoopSynth/Old data/EMPpump.csv")
EMPCB = read.csv("~/ZoopSynth/Old data/EMPzoops.csv")
EMPpump$Date = mdy_hm(EMPpump$Date)
EMPpump$SizeFrac = as.factor(EMPpump$SizeFrac)
EMPCB$Date = mdy_hm(EMPCB$Date)
EMPCB = rename(EMPCB, taxon = EMP, Station = EMPzoop)


#make a unique code for each sample
EMPpump$sampleID = paste(EMPpump$SurveyCode, EMPpump$Station)
EMPCB$sampleID = paste(EMPCB$SurveyCode, EMPCB$Station)

#calculate relative abundance of each taxon in the sample
EMPpump2 = group_by(EMPpump, sampleID, SizeFrac) %>% summarise(totCPUE = sum(CPUE))
EMPpump = merge(EMPpump, EMPpump2)
EMPpump$rel = EMPpump$CPUE/EMPpump$totCPUE

#need to drop any samples that only have one size fraction
foo = group_by(EMPpump2, sampleID) %>% summarise(frac = sum(as.numeric(SizeFrac)))
EMPpump = merge(EMPpump, foo)
EMPpump = filter(EMPpump, frac == 3)

#filter the data so I just have the Sacramento River stations, since Louise and I want to compare Decker Island
#Note: I could filter this to just do station 64, which is the one next to Decker, but I want to increase the replication

#EMPpump = filter(EMPpump, Station =="NZ060" |Station =="NZ064"| Station =="NZ086"| 
#                   Station =="NZD16"|  Station =="NZ074" )


###########################################################################################################
#reshape the data so we can get a community matrix to put into the MANOVA

#CPUE matrix
EMPpumpW = dcast(EMPpump, sampleID + SizeFrac ~ taxon, value.var = "CPUE")
EMPpumpW[is.na(EMPpumpW)] =0

#Relative abudance matrix
EMPpumpRel = dcast(EMPpump, sampleID + SizeFrac ~ taxon, value.var = "rel")
EMPpumpRel[is.na(EMPpumpRel)] =0

#see the difference between the size fractions
a1 = adonis(EMPpumpRel[,3:ncol(EMPpumpRel)]~SizeFrac, data = EMPpumpRel)

#the coefficients should tell us the difference in abundance of each taxa 
a1$coefficients

#Try telling the function to do it with the samples paired
#set up the permutational thingy
h1 = how(within = Within(type = "none"), plots = Plots(strata = EMPpumpRel$sampleID, type = "free"),  nperm = 999)

a2 = adonis(EMPpumpRel[,3:ncol(EMPpumpRel)]~SizeFrac, data = EMPpumpRel, permutations = h1)
a2$coefficients

#I'm not sure I did that right, so let's try another way
a3 = adonis(EMPpumpRel[,3:53]~sampleID + SizeFrac, data = EMPpumpRel, permutations = 199)
a3$coefficients
#yeah, thats def wrong

######################################################################################################
#look at CPUE rather than relative abundance

h1 = how(within = Within(type = "none"), plots = Plots(strata = EMPpumpW$sampleID, type = "free"),  nperm = 199)

C2 = adonis(EMPpumpW[,3:53]~SizeFrac, data = EMPpumpW, permutations = h1)
C2$coefficients




#########################################################################################################
#first let's just do a quick plot of relative abundance, by size fraction
#need to put the zeros in there to get the means right!
EMPpump3 = melt(EMPpumpW, id.vars = c("sampleID", "SizeFrac"), variable.name = "taxon", value.name = "CPUE")
sizesum = group_by(EMPpump3, SizeFrac, taxon) %>% summarize(meanCPUE = mean(CPUE))

#relative abundance version
EMPpump3r = melt(EMPpumpRel, id.vars = c("sampleID", "SizeFrac"), variable.name = "taxon", value.name = "rel")
sizesumR = group_by(EMPpump3r, SizeFrac, taxon) %>% summarize(meanrel = mean(rel))


p1 = ggplot(sizesum, aes(x=SizeFrac, y = meanCPUE))
p1+ geom_bar(stat = "identity", aes(fill = taxon), position = "fill")
#definitely some differences in there.

p2 = ggplot(sizesum, aes(x=SizeFrac, y = meanCPUE))
p2+ geom_bar(stat = "identity", aes(fill = SizeFrac)) + facet_wrap(~taxon, scales = "free_y")

p3 = ggplot(sizesumR, aes(x=SizeFrac, y = meanrel))
p3+ geom_bar(stat = "identity", aes(fill = SizeFrac)) + facet_wrap(~taxon, scales = "free_y")
#that's not very useful


###########################################################################################
#I'm overthinking this. I just need the average % catch of each critter than occurs in the large
#size fraction. MANOVA is just to see if the difference is significant.
EMPpump4 = group_by(EMPpump3, sampleID, taxon) %>% summarize(tot = sum(CPUE), 
                                                             fract1 = CPUE[which(SizeFrac=="1")]/tot,
                                                             fract2 = CPUE[which(SizeFrac=="2")]/tot)
EMPpump4$fract1[which(is.nan(EMPpump4$fract1))] = 0
EMPpump4$fract2[which(is.nan(EMPpump4$fract2))] = 0

#I think I want to remove the zeros. It doesn't make sense to talk about the proportion in one size
#fraction or the other if none were caught at all.

EMPpump4 = filter(EMPpump4, tot != 0)

# Now take the average proportion in each size fraction

taxprops = group_by(EMPpump4, taxon) %>% summarize(frac1 = mean(fract1), frac2 = mean(fract2))

###########################################################################################
#Louise's data contains both size fractions. If I multiply by her data by "frac2", that
#should give me the number of critters one would expect in the large size fraction.
#but first I need to make the critters talk to each other.

#see what critters Louise got:
zoop_veg <- read.csv("~/Documents/EMPzoops/zoop_veg.csv")
vegtax = group_by(zoop_veg, category, taxon, genus, stage) %>% summarize(tottax = sum(count))

#Huh. It looks like they don't ID rotifers, they don't ID copepodiids and nauplii that's a 
#comparibility problem, but we can work with it.

#Let's make a new column with taxa + life stage
vegtax$taxon2 = paste(vegtax$taxon, vegtax$stage)
write.csv(vegtax, "vegtax.csv")

#I developed a crosswalk betwee the EMP species codes, FRP species codes, and Veg study codes:
library(readxl)
crosswalk <- read_excel("FRP_EMP_Veg crosswalk.xlsx")

# Unfortunately, there are a lot of taxa that are only on one of the three lists, and I don't know whether
# they don't occur in the other studies, or are not identified to the same level of resolution, so the
# column "LCDtax" is the lowest common denominator between the three lists. We'll have to use those
# ID's to make the comparison.

#############################################################################
#look at CB versus pump
EMPpump3$net = "pump"
EMPCB$net = "CB"
EMPCB = filter(EMPCB, Date > "2008-02-01")
EMPpumpall = group_by(EMPpump3, sampleID, taxon, net) %>% 
  summarize(CPUE = sum(CPUE))

EMPall = rbind(ungroup(EMPpumpall), ungroup(EMPCB[,4:7]))
EMPallsum = group_by(EMPall, sampleID, taxon) %>% summarize(tCPUE = sum(CPUE, na.rm = T))
EMPallsum2 = merge(EMPall, EMPallsum)
EMPallsum2$Rel = EMPallsum2$CPUE/EMPallsum2$tCPUE
EMPallmean = group_by(EMPallsum2, taxon, net) %>% summarize(mRel = mean(Rel), mCPUE = mean(CPUE))

ggplot(EMPallmean, aes(x = net, y = mCPUE, fill = net)) + 
  geom_bar(stat = "identity") + facet_wrap(~taxon, scales = "free_y")
