#Spatial/time/salinity joining script
#source("scripts/data_cleaning.R")

target_zoops<-readRDS(file="data/target_zoops.Rds")
smelt_samples<-readRDS("data/smelt_samples.Rds")
target_zoop_samples<-readRDS("data/target_zoop_samples.Rds")
smelt_list<- read.csv("data/smelt_data_wwq.csv")

library(sf)
library(lubridate)
library(tidyverse)
#################################
#Set Working variables
#################################

#r = buffer radius in km
radius= 2

#+- date range in days
daterange=15

#salinity +- range ppt
salrange=1

#################################
#Spatial buffering
#################################

#remove zoop samples with missing coords
target_zoop_samples<-target_zoop_samples[!is.na(target_zoop_samples$lat),]

#make smelt and zoop samples data spatial
smelt_list <- as.data.frame(smelt_samples) %>% 
  st_as_sf(coords=c("long","lat"), crs=4326, remove=FALSE)
target_zoop_samples<-as.data.frame(target_zoop_samples) %>% 
  st_as_sf(coords=c("long","lat"), crs=4326, remove=FALSE)

#reproject to wgs84 but with km units first
smelt_list<-st_transform(smelt_list, "+proj=utm +zone=42N +datum=WGS84 +units=km")

#create point buffer for smelt catchs with radius=2km
smelt_buffer = st_buffer(smelt_list, radius)

#reproject points and buffers in 4326
smelt_list<-st_transform(smelt_list, 4326) #reproject the shapefile
smelt_buffer<-st_transform(smelt_buffer, 4326) #reproject the shapefile

#plot stations to make sure geometry is working
plot(target_zoop_samples$geometry,col="green",axes=T)
plot(smelt_list$geometry,col="red",add=T)
plot(smelt_buffer$geometry,col=adjustcolor("red",alpha=.2),add=T)

#spatial clipping
smelt_zoop_clips<-st_intersection(smelt_buffer,target_zoop_samples)


plot(smelt_zoop_clips$geometry,type='p',add=T,pch=16) #black dots are clipped zoop stations, and should only appear within the red circles

#################################
#date "buffering"
#################################
smelt_zoop_clips$date<-as.character(smelt_zoop_clips$date)
smelt_zoop_clips$date<-as.Date(smelt_zoop_clips$date, "%Y-%m-%d")
smelt_zoop_clips$date_upper<-as.Date(smelt_zoop_clips$date+daterange)
smelt_zoop_clips$date_lower<-as.Date(smelt_zoop_clips$date-daterange)

smelt_zoop_clips$date.1<-as.Date(smelt_zoop_clips$date.1,"%Y-%m-%d")

#strip geometry
df_geometry<-select(smelt_zoop_clips,fishid,SampleID)
str(df_geometry)

#set original geometry to NULL
smelt_zoop<-smelt_zoop_clips
st_geometry(smelt_zoop)<-NULL

#filter by date range
smelt_zoop<-smelt_zoop%>%
  dplyr::filter(date.1<=date_upper & date.1>=date_lower)

#################################
#salinity "buffering"
#################################

#find zooplankton samples average salinity between surface and bottom
smelt_zoop$ZoopSalAvg<-(smelt_zoop$SalBott+smelt_zoop$SalSurf)/2


#set target salinity range
smelt_zoop$ZoopSalUpper<-smelt_zoop$ZoopSalAvg+salrange
smelt_zoop$ZoopSalLower<-smelt_zoop$ZoopSalAvg-salrange

#filter for rows in which delta smelt were caught within salinity range of zoop sample
smelt_zoop_filtered<-smelt_zoop%>%
  filter(sal<=ZoopSalUpper,sal>=ZoopSalLower)

#rename and restructure final output
smelt_zoop_output<-smelt_zoop_filtered%>%
  rename(smelt_date=date,
         smelt_ID=fishid,
         smelt_sal=sal,
         smelt_secchi=secchi,
         smelt_temp=temp,
         smelt_station=station,
         smelt_survey=survey,
         smelt_lat=lat,
         smelt_long=long,
         zoop_station=station.1,
         zoop_date=date.1,
         zoop_lat=lat.1,
         zoop_long=long.1,
         zoop_sal_avg=ZoopSalAvg,
         zoop_ID=SampleID
         )
smelt_zoop_output<-smelt_zoop_output[c("smelt_ID","smelt_date","smelt_survey","smelt_station","smelt_sal","smelt_secchi","smelt_temp","smelt_lat","smelt_long","zoop_ID","zoop_station","zoop_date","zoop_lat","zoop_long","zoop_sal_avg")]

#find related zooplankton catch data
#filters zoop wq and catch data by those zoop samples joined to smelt samples in this script
target_zoop_ID<-smelt_zoop_output[['zoop_ID']]

zoop_output<-target_zoops%>%
  filter(zoop_ID%in%target_zoop_ID)

write.csv(zoop_output,file="outputs/zoop_output.csv",row.names = F)  
write.csv(smelt_zoop_output,"outputs/smelt_zoop_output.csv",row.names = F)
