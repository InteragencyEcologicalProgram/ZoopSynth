
#Get the FRP data and Louise's data to talk to each other
library(lubridate)
library(ggplot2)
library(dplyr)
library(vegan)
library(reshape2)
library(readxl)

#First look at EMP's pump samples to see what the difference between 150 microns
#and 50 microns will be.

# Load up the EMP samples
EMPpump <- read.csv("EMPpump.csv")
EMPpump$Date = mdy_hm(EMPpump$Date)
EMPpump$SizeFrac = as.factor(EMPpump$SizeFrac)


#make a unique code for each sample
EMPpump$sampleID = paste(EMPpump$SurveyCode, EMPpump$Station)
#need to drop any samples that only have one size fraction
EMPpump1 = group_by(EMPpump, sampleID, SizeFrac) %>% summarise(totCPUE = sum(CPUE))
foo = group_by(EMPpump1, sampleID) %>% summarise(frac = sum(as.numeric(SizeFrac)))
EMPpump = merge(EMPpump, foo)
EMPpump = filter(EMPpump, frac == 3)

#filter the data so I just have the stations near Decker, since Louise and I want to compare Decker Island
#Note: I could filter this to just do station 64, which is the one next to Decker, but I want to increase the replication

EMPpump = filter(EMPpump, Station =="NZ060" |Station =="NZ064"| Station =="NZ086"| 
                   Station =="NZD16"|  Station =="NZ074" )

###############################################################################################
#now let's see what I need to do to get people's data to talk to each other

#see what critters EMP gets:
EMPtax = group_by(EMPpump, taxon) %>% summarize(meanCPUE = mean(CPUE))
#write.csv(EMPtax, "EMPtax.csv")

#see what critters Louise got:
zoop_veg <- read.csv("zoop_veg.csv")
zoop_veg$date = as.Date(zoop_veg$date, format = "%m/%d/%Y")
vegtax = group_by(zoop_veg, category, taxon, genus, stage) %>% summarize(tottax = sum(count))

#Huh. It looks like they don't ID rotifers, they don't ID copepodiids and nauplii that's a 
#comparibility problem, but we can work with it.

#Let's make a new column with taxa + life stage
vegtax$taxon2 = paste(vegtax$taxon, vegtax$stage)
#write.csv(vegtax, "vegtax.csv")

#I developed a crosswalk betwee the EMP species codes, FRP species codes, and Veg study codes:
library(readxl)
crosswalk <- read_excel("FRP_EMP_Veg crosswalk.xlsx")

# Unfortunately, there are a lot of taxa that are only on one of the three lists, and I don't know whether
# they don't occur in the other studies, or are not identified to the same level of resolution, so the
# column "LCDtax" is the lowest common denominator between the three lists. We'll have to use those
# ID's to make the comparison.

#first I'll make the column names line up
EMPpump = rename(EMPpump, EMP = taxon)

#now add the "LCDtax" classification
EMPpump1 = merge(EMPpump, distinct(crosswalk[,c(2,5)]), by = "EMP")

###########################################################################################
# I need the average % catch of each critter than occurs in the large
#size fraction. I could run a MANOVA to see if the difference is signicficant, but I 
#can just use the average proportion to make the adjustment.

#Note: some people (like Karen), don't like this idea. THey would rather 
#throw out any organism that isn't quantitativley sampled.

#Calculate the total CPUE for each taxa in each sample, and the proportion of the total
# in each size fraction:
EMPpump2 = group_by(EMPpump1, sampleID, LCDtax) %>% summarize(tot = sum(CPUE), 
                                                             fract1 = sum(CPUE[which(SizeFrac=="1")])/tot,
                                                             fract2 = sum(CPUE[which(SizeFrac=="2")])/tot)

#Take out anything that doesn't have an "LCDtax" entry. I think it's just the barnalce and crab larvae.
#we wouldn't expect many of them to be in freshwater anyway.
EMPpump2 = filter(EMPpump2, is.na(LCDtax)== F)

# Now take the average proportion in each size fraction

taxprops = group_by(EMPpump2, LCDtax) %>% summarize(frac1 = mean(fract1), frac2 = mean(fract2))
#it sorta hurts to see all that work turned into only 19 taxonomic categories. Sigh. We should
#really have that invertebrate summit where we compare lab methods.

#Add this proportion to my crosswalk
crosswalk2 = merge(crosswalk, taxprops)
write.csv(crosswalk2, "crosswalk2.csv")

###########################################################################################
#Louise's data contains both size fractions. If I multiply by her data by "frac2", that
#should give me the number of critters one would expect in the large size fraction.

#now I'll add the "LCDtax" to Louise's zooplankton data
zoop_veg$Veg = paste(zoop_veg$taxon, zoop_veg$stage)
zoop_veg2 = merge(zoop_veg, distinct(crosswalk2[,c(1,5,7)]), all.x = F)

#Now I'll adjust Louise's counts to see what they would have gotten in a 150ish micron mesh net!!!

zoop_veg2$bignet = zoop_veg2$indiv_per_ml * zoop_veg2$frac2

#Our data has CPUE in individuals per cubic meter, rather than per mL, so that's a quick fix:
zoop_veg2$CPUE = zoop_veg2$bignet * 1000000

########################################################################################
#OMG I"M GOING TO GRAPH THEM ALL TOGETHER!!!!

DeckVeg  = filter(zoop_veg2, island == "DI")
DeckVeg = DeckVeg[, c(2, 4, 5, 15,18)]
DeckVeg$Survey = "Veg"


FRPzoo <- read_excel("FRPzooforLouise.xlsx")
#make the data sets cooperate
FRPzoo = rename(FRPzoo, sample = ZSampleID, date = Date)
FRPDeck = FRPzoo[,c(1,2,4,8,9)]
FRPDeck$date = as.Date(FRPDeck$date)
FRPDeck$Survey = "FRP"

#Take out all the taxa that don't have a conversion factor associated with them
FRPDeck = filter(FRPDeck, LCDtax != "NA")

#put both data sets together
Deckzoo = rbind(FRPDeck, DeckVeg)

#make sure there is only one entry per "LCDtax" per sample
Deckzoo2 = group_by(Deckzoo, sample, stratum, date, LCDtax, Survey) %>% summarize(CPUE = sum(CPUE))

#Quick graph of all teh samples
g1 = ggplot(Deckzoo2, aes(x = sample, y = CPUE))
g1 + geom_bar(stat = "identity",aes(fill = LCDtax)) + 
  theme(axis.text.x = element_text(angle = 90))

#averages by stratum and date
#add zeros in first
Deckwide = dcast(Deckzoo, sample + stratum + date + Survey ~ LCDtax, value.var = "CPUE", sum)
Deck3 = melt(Deckwide, id.var = c("sample", "stratum" , "date", "Survey"), variable.name = "LCDtax", value.name = "CPUE")
Deckave = group_by(Deck3, stratum, date, LCDtax, Survey) %>% summarize(CPUE = mean(CPUE))

#graph again
g2 = ggplot(Deckave, aes(x = stratum, y = CPUE))
g2 + geom_bar(stat = "identity", aes(fill = LCDtax)) +
  facet_grid(Survey~as.factor(date))

