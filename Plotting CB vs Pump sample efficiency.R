require(tidyverse)
require(readxl)
require(lubridate)
require(dtplyr)

EMP_CB <- read_excel("Data/1972-2018CBMatrix.xlsx", 
                     sheet = "CB CPUE Matrix 1972-2018", 
                     col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                   "text", "text", "text", "numeric", "text", "text",
                                   "text", rep("numeric", 62)))%>%
  rename(Volume=CBVolume)

EMP_Pump <- read_excel("Old data/1972-2018Pump Matrix.xlsx", 
                       sheet = "Pump CPUE Matrix 1972-2018", 
                       col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                     "text", "text", "text", "numeric", 
                                     "text", rep("numeric", 36)))%>%
  rename(Date=SampleDate, Volume=PumpVolume)

crosswalk <- read_excel("Data/new_crosswalk.xlsx", sheet = "Hierarchy2")%>%
  select(EMP, Taxname, Lifestage, EMPstart, EMPend, Intro)%>%
  mutate_at(vars(c("EMPstart", "EMPend", "Intro")), ~parse_date(as.character(.), format="%Y"))%>%
  mutate_at(vars(c("EMPstart", "EMPend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
  mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)

#Select columns in common between two datasets, rename taxa columns with distinct names

#Create vector of taxonomic variables
Taxvars<-c("LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD", "ALLCYCADULTS", "HARPACT", "CYCJUV", "LIMNOJUV", "OITHJUV", "OTHCYCJUV", "ALLCYCJUV", "COPNAUP", "EURYNAUP", "OTHCOPNAUP", "PDIAPNAUP", "SINONAUP", "ALLCOPNAUP", "ASPLANCH", "KERATELA", "OTHROT", "POLYARTH", "SYNCH", "SYNCHBIC", "TRICHO", "ALLROTIFERS", "BARNNAUP")

#create vector of taxonomic variables minus the summed "ALL" categories
Taxvars2<-c("LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD", "HARPACT", "CYCJUV", "LIMNOJUV", "OITHJUV", "OTHCYCJUV", "COPNAUP", "EURYNAUP", "OTHCOPNAUP", "PDIAPNAUP", "SINONAUP", "ASPLANCH", "KERATELA", "OTHROT", "POLYARTH", "SYNCH", "SYNCHBIC", "TRICHO", "BARNNAUP")

EMP_CB2<-EMP_CB%>%
  select(intersect(names(.), names(EMP_Pump)))%>% #select columns in common between two datasets
  select(-Survey, -SurveyCode, -SurveyRep, -EZStation, -Core)%>% #Remove some variables
  mutate(SampleID=paste(Date, Station))%>% #Create stationID identifier
  rename_at(vars(Taxvars),
            ~paste0(., "_CB"))%>% #append "_CB" to taxonomic variables
  select(-Volume)

#Do the same for the Pump samples, except total pump catch shouldn't matter 
EMP_Pump2<-EMP_Pump%>%
  select(intersect(names(.), names(EMP_CB)))%>%
  select(-Survey, -SurveyCode, -SurveyRep, -EZStation, -Core)%>%
  mutate(SampleID=paste(Date, Station))%>%
  rename_at(vars(Taxvars),
            ~paste0(., "_Pump"))%>%
  select(-Volume)%>%
  rename(OTHCYCADPUMP_Pump=OTHCYCAD_Pump)

#Join pump and CB data
#Join pump and CB data
EMP<-EMP_CB2%>%
  inner_join(EMP_Pump2, 
             by = c("Year", "Date", "Station", "Region", "Secchi", "Chl-a", "Temperature", "ECSurfacePreTow", "ECBottomPreTow", "SampleID"))%>%
  select(-Region, -Secchi, -`Chl-a`, -Temperature, -ECSurfacePreTow, -ECBottomPreTow, -Station)%>%
  pivot_longer(cols= c(ends_with("_CB"), ends_with("_Pump")), names_to=c("EMP", "Method"), names_sep = "_", values_to="CPUE")%>%
  left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
              select(EMP, Lifestage, Taxname, Intro, EMPstart, EMPend)%>% #only retain EMP codes
              filter(!is.na(EMP))%>% #Only retain Taxnames corresponding to EMP codes
              distinct(),
            by="EMP")%>%
  filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
  mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
  mutate(CPUE=case_when(
    CPUE!=0 ~ CPUE, 
    CPUE==0 & Date < Intro ~ 0,
    CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
    CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
    CPUE==0 & Date >= EMPend ~ NA_real_
  ))%>% 
  select(-EMP, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes 
  lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
  group_by_at(vars(-CPUE))%>%
  summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
  #so we now add those categories together.
  ungroup()%>%
  as_tibble() #required to finish operation after lazy_dt()

EMP_sum<-EMP%>%
  group_by(Method, Taxlifestage)%>%
  summarise(CPUE=mean(CPUE, na.rm=T))%>%
  ungroup()

ggplot(EMP_sum, aes(x=Method, y=CPUE, fill=Method))+
  geom_bar(stat="identity")+
  facet_wrap(~Taxlifestage, scales="free_y")+
  scale_y_continuous(limits=c(0,NA), expand=expand_scale(mult=c(0,0.05)))+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.background=element_blank())
