require(tidyverse) 
require(readxl)
require(lubridate)

# Load crosswalk key to convert each dataset's taxonomic codes to a
# unified set of "Taxname" and "Lifestage" values.

crosswalk <- read_excel("Data/new_crosswalk.xlsx", sheet = "Hierarchy2")%>%
  mutate_at(vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~parse_date(as.character(.), format="%Y"))%>%
  mutate_at(vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
  mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(FMWTend = if_else(is.finite(FMWTend), FMWTend+years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(twentymmend = if_else(is.finite(twentymmend), twentymmend+years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)

#Calculate years when taxa were not counted
EMP<-crosswalk%>%
  filter(!is.na(EMP))%>%
  select(Taxname, Lifestage, EMPstart, EMPend, Intro)%>%
  mutate(Taxlifestage=paste(Taxname, Lifestage),
         Years=sapply(1:nrow(.), function(x) ifelse(is.finite(EMPstart[x]), ifelse(is.finite(Intro[x]), paste(year(Intro[x]):year(EMPstart[x]), collapse=","), paste(1950:year(EMPstart[x]), collapse=",")), NA_character_)))%>%
  mutate(Yearsend=sapply(1:nrow(.), function(x) ifelse(is.finite(EMPend[x]), paste(year(EMPend[x]):year(Sys.Date()), collapse=","), NA_character_)))%>%
  mutate(Years=sapply(1:nrow(.), function (x) paste(unique(c(str_split(Years[x], ",")[[1]], str_split(Yearsend[x], ",")[[1]])), collapse=",")))%>%
  select(Taxlifestage, Years)%>%
  group_by(Taxlifestage)%>%
  summarise(Years=ifelse(n()>1, paste(Reduce(intersect, (sapply(1:n(), function(x) str_split(Years[x], ",")[[1]]))), collapse=","), Years))