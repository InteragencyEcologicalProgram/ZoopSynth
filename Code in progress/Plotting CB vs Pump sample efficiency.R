require(tidyverse)
require(readxl)
require(lubridate)
require(dtplyr)
require(broom)
require(RColorBrewer)

EMP_CB <- read_excel("Data/1972-2018CBMatrix.xlsx", 
                     sheet = "CB CPUE Matrix 1972-2018", 
                     col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                   "text", "text", "text", "numeric", "text", "text",
                                   "text", rep("numeric", 62)))%>%
  rename(Volume=CBVolume)

EMP_Pump <- read_excel("Old data and code/1972-2018Pump Matrix.xlsx", 
                       sheet = "Pump CPUE Matrix 1972-2018", 
                       col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                     "text", "text", "text", "numeric", 
                                     "text", rep("numeric", 36)))%>%
  rename(Date=SampleDate, Volume=PumpVolume)

crosswalk <- zooper::crosswalk%>%
  select(EMP=EMP_Meso, Taxname, Lifestage, EMPstart, EMPend, Intro)%>%
  filter(!is.na(EMP))%>%
  mutate(EMPstart=ifelse(EMP=="OTHCYCAD", 1975, EMPstart))%>% #Pump started counting OTHCYCAD in 1975 while CB started in 1972 so using the later year for both
  mutate_at(vars(c("EMPstart", "EMPend", "Intro")), ~parse_date(as.character(.), format="%Y"))%>%
  mutate_at(vars(c("EMPstart", "EMPend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
  mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)

#Select columns in common between two datasets, rename taxa columns with distinct names

#Create vector of taxonomic variables
Taxvars<-c("LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD", "ALLCYCADULTS", "HARPACT", "CYCJUV", "LIMNOJUV", "OITHJUV", "OTHCYCJUV", "ALLCYCJUV", "COPNAUP", "EURYNAUP", "OTHCOPNAUP", "PDIAPNAUP", "SINONAUP", "ALLCOPNAUP", "ASPLANCH", "KERATELA", "OTHROT", "POLYARTH", "SYNCH", "SYNCHBIC", "TRICHO", "ALLROTIFERS", "BARNNAUP")

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
  select(-Volume)


#Ensure that the same samples are present from both methods
EMP_CB3<-EMP_CB2%>%
  filter(SampleID%in%unique(EMP_Pump2$SampleID))

EMP_Pump3<-EMP_Pump2%>%
  filter(SampleID%in%unique(EMP_CB2$SampleID))

#Join pump and CB data
EMP<-EMP_CB3%>%
  inner_join(EMP_Pump3, 
             by = c("Year", "Date", "Station", "Region", "Secchi", "Chl-a", "Temperature", "ECSurfacePreTow", "ECBottomPreTow", "SampleID"))%>%
  select(-Region, -Secchi, -`Chl-a`, -Temperature, -ECSurfacePreTow, -ECBottomPreTow, -Station)%>%
  pivot_longer(cols= c(ends_with("_CB"), ends_with("_Pump")), names_to=c("EMP", "Method"), names_sep = "_", values_to="CPUE")%>%
  left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
              select(EMP, Lifestage, Taxname, Intro, EMPstart, EMPend)%>% #only retain EMP codes
              filter(!is.na(EMP))%>% #Only retain Taxnames corresponding to EMP codes
              distinct(),
            by="EMP")%>%
  filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
  mutate(Taxname_label=ifelse(Taxname%in%c("Asplanchna", "Keratella", "Limnoithona", "Limnoithona sinensis", "Limnoithona tetraspina", "Oithona", "Oithona davisae", "Oithona similis", "Polyarthra", "Synchaeta", "Synchaeta bicornis", "Trichocerca", "Eurytemora affinis", "Pseudodiaptomus", "Sinocalanus doerrii"), paste0("italic('", Taxname, "')"),
                             Taxname))%>% #create variable for combo taxonomy x life stage and make some names italicized for graph. 
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
  summarise(CPUE=sum(CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
  #so we now add those categories together.
  ungroup()%>%
  as_tibble() #required to finish operation after lazy_dt()

EMP_sum<-EMP%>%
  group_by(Method, Taxname, Lifestage, Taxname_label)%>%
  summarise(CPUE=sum(CPUE, na.rm=T))%>%
  ungroup()%>%
  mutate(Method = recode(Method, CB = "Meso", Pump = "Micro"))%>%
  group_by(Taxname, Lifestage, Taxname_label)%>%
  mutate(Total=sum(CPUE, na.rm=T))%>%
  ungroup%>%
  mutate(Proportion=CPUE/Total)%>%
  arrange(Taxname, Lifestage)%>%
  mutate(Taxname_label=factor(Taxname_label, levels=unique(Taxname_label)))

p<-ggplot(EMP_sum)+
  geom_bar(aes(x=Method, y=Proportion, fill=Method), stat="identity")+
  facet_wrap(~Taxname_label*Lifestage, labeller = label_parsed, ncol=4)+
  scale_fill_manual(values=brewer.pal(3, "PRGn")[c(1,3)])+
  scale_y_continuous(limits=c(0,1), expand=c(0,0))+
  ylab("Proportion of total CPUE")+
  xlab("Collection method")+
  theme_bw()+
  theme(text=element_text(size=14), panel.grid=element_blank(), strip.background=element_blank(), legend.position = "none", strip.text.x = element_text(margin = margin(b = 0, t = 0)))
p
ggsave(p, file="Code in progress/Pump CB CPUEs bar.png", device = "png", units="in", height=8, width=7.5)


EMP_xyplot<-EMP%>%
  spread(key = Method, value = CPUE)%>%
  select(-Taxname, -Lifestage, -Year)%>%
  mutate(CB_l=log(CB+1),
         Pump_l=log(Pump+1))%>%
  select(-CB, -Pump)

EMP_model<- function(df) {
  lm(Pump_l ~ CB_l, data = df)
}

EMP_xyplot_models<-EMP_xyplot%>%
  group_by(Taxlifestage)%>%
  nest()%>% 
  mutate(model = map(data, EMP_model))%>%
  select(-data, )%>%
  mutate(model_sum=map(model, tidy))%>%
  unnest(model_sum)%>%
  select(-p.value, -statistic, -std.error)%>%
  spread(key=term, value=estimate)%>%
  rename(Intercept=`(Intercept)`, Slope=CB_l)%>%
  mutate(model_sum=map(model, glance))%>%
  unnest(model_sum)%>%
  select(-model, -sigma, -statistic, -df, -logLik, -AIC, -BIC, -deviance, -df.residual)

EMP_xyplot_stats<-EMP_xyplot%>%
  group_by(Taxlifestage)%>%
  summarise(CB_l_max=max(CB_l), Pump_l_max=max(Pump_l))%>%
  left_join(EMP_xyplot_models%>%
              select(Taxlifestage, r.squared, p.value),
            by="Taxlifestage")%>%
  mutate(r.squared=round(r.squared, 2),
         p.value=round(p.value, 4))

p<-ggplot(EMP_xyplot, aes(x=CB_l, y=Pump_l, group=Taxlifestage))+
  geom_point(alpha=0.2)+
  geom_abline(data=EMP_xyplot_models, aes(intercept=Intercept, slope=Slope, group=Taxlifestage), color="blue")+
  geom_text(data=EMP_xyplot_stats, aes(x=CB_l_max*0.85, y=Pump_l_max*0.1, label=paste0("R2 = ", r.squared)), size=3)+
  facet_wrap(~Taxlifestage, scales="free")+
  scale_y_continuous(limits=c(0,NA), expand=expand_scale(mult=c(0.01,0.05)))+
  scale_x_continuous(limits=c(0,NA), expand=expand_scale(mult=c(0.01,0.05)))+
  xlab("log(CB CPUE)")+
  ylab("log(Pump CPUE)")+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.background=element_blank())

ggsave(p, file="Pump CB CPUEs.png", device = "png", units="in", height=8, width=10)

