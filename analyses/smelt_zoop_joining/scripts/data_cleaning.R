#data_cleaning script
rm( list = ls()) #clear env

#load zoopsynth data
library(devtools)
library(zooper)
library(tidyverse)

#filter zoopsynth data
target_zoops<-Zoopsynther(Data_type="Taxa",
                          Sources=c("EMP","FMWT","STN","20mm","FRP"),
                          Size_class = c("Micro", "Meso", "Macro"),
                          Date_range = c("2011-08-01", "2018-11-30"))

#load and group smelt data
library(readxl)
smelt_list<- read.csv("data/smelt_data_wwq.csv")
smelt_samples<-smelt_list
#rename TNS to STN
smelt_samples$survey<-as.character(smelt_samples$survey)
smelt_samples$date<-as.character(smelt_samples$date)
smelt_samples$survey[smelt_samples$survey=="TNS"]<-"STN"
smelt_samples$date<-as.Date(smelt_samples$date,"%m/%d/%Y")

 #group zoop samples
target_zoops<-target_zoops%>%
  rename(date=Date,
         station=Station,
         survey=Source,
         lat=Latitude,
         long=Longitude)
target_zoop_samples<-unique(select(target_zoops,station,survey,date,lat,long,SalSurf,SalBott,SampleID)) #this df is used in the joining script to join smelt samples to zoop samples


##rename and restructure zooplankton final output, which contains all the zoop wq and catch data
target_zoops<-target_zoops%>%
  rename(zoop_date=date,
         zoop_sal_surf=SalSurf,
         zoop_sal_bott=SalBott,
         zoop_secchi=Secchi,
         zoop_temp=Temperature,
         zoop_chl=Chl,
         zoop_station=station,
         zoop_survey=survey,
         zoop_lat=lat,
         zoop_long=long,
         zoop_ID=SampleID,
         zoop_DO=DO,
         zoop_pH=pH
  )
#reorder column outputs for ease of use
target_zoops<-target_zoops[c("zoop_ID","zoop_survey","zoop_date","zoop_station","zoop_lat","zoop_long","zoop_sal_surf","zoop_sal_bott","zoop_secchi","zoop_temp","zoop_chl","Tide","BottomDepth","Taxname","SizeClass","Genus","Species","Taxlifestage","CPUE","Volume")]

target_zoops<-as.data.frame(target_zoops)
target_zoop_samples<-as.data.frame(target_zoop_samples)
smelt_samples<-as.data.frame(smelt_samples)

saveRDS(target_zoops,file="data/target_zoops.Rds")
saveRDS(target_zoop_samples,file="data/target_zoop_samples.Rds")
saveRDS(smelt_samples,file="data/smelt_samples.Rds")
