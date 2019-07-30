#Let's try to break Sam's Zooper function. 

source("Zoop synthesizer function.R")

EMPtest = Zooper(Sources = "EMP", Data = "Community")
table(EMPtest$Year)

#So that works fine, but when I ask for the same thing in the Shiny app I get:
EMPtest2 <- read.csv("C:/Users/rhartman/Desktop/data-2019-07-16 EMP.csv")
table(EMPtest2$Year)

EMP1985 = filter(EMPtest, Year == 1985)
#I don't know why shiny doesn't like the 1980s. It doesn't have all the data for other years too

EMP1999a = filter(EMPtest, Year == 1999)
EMP1999b = filter(EMPtest2, Year == 1999)

#All both methods retaining the same samples?
Allc<-Zooper(Data = "Community")
Allt<-Zooper(Data = "Taxa")
all_equal(unique(Allc$SampleID), unique(Allt$SampleID))

#Are the overall CPUEs from the 2 methods equal? 

#(Had to remove some taxa from the "Taxa" method that were removed in the "Community" method before comparing)

sum(Allc$CPUE) == sum(filter(Allt, 
           Taxatype!="Summed group" & !(Taxlifestage%in%c("Annelida_UnID Adult", "Diptera_UnID Pupae", "Lumbricidae_UnID Adult", "UnID Invertebrate Adult", "Actinopterygii_UnID Adult", "Nematoda_UnID Adult", "Gyraulus_UnID Adult", "Hirudinea_UnID Adult", "Polychaeta_UnID Adult", "Hydra_UnID Adult", "Tardigrada_UnID Adult", "Girardia tigrina Adult", "Gastropoda_UnID Adult", "Bivalvia_UnID Adult", "Ferrissia californica Adult", "Bivalvia_UnID Juvenile", "Physa_UnID Adult")))$CPUE)


#Make sure all columns have >1 unique value (should return 0 row tibble)
Unique_valsc<-Allc%>% summarise_all(n_distinct)%>%
  gather("Column", "Distinct_number")%>%
  filter(Distinct_number<2)

nrow(Unique_valsc)<1

Unique_valst<-Allt%>% summarise_all(n_distinct)%>%
  gather("Column", "Distinct_number")%>%
  filter(Distinct_number<2)

nrow(Unique_valst)<1


#Test that there is only 1 entry for each combination of sampleID & Taxlifestage
Allc%>%
  group_by(SampleID, Taxlifestage)%>%
  mutate(N=n())%>%
  ungroup()%>%
  pull(N)%>%
  every(~.==1)

Allt%>%
  group_by(SampleID, Taxlifestage)%>%
  mutate(N=n())%>%
  ungroup()%>%
  pull(N)%>%
  every(~.==1)

#####################################################################################
#Some random thoughts

#1. Is there a reason we don't set the default parameter values to -Inf, Inf (or something like that)?

#   Sam: Filtering from -Inf to Inf would remove all NAs. If users don't care about a particular variable they will probably want the data with NAs in that variable. If they do want to remove data with NAs in a particular variable they can set the range to c(-Inf, Inf) but I do not think I would set this to the default. 