require(sf)
require(rgdal)
require(raster)
require(tidyverse)
require(dtplyr)
require(leafpop)

#Taxa data: Produce map of individual taxa across delta with abundance indicated by point size. Produce one map per year, should add slider for year so users can move through the years and watch the abundance shift/change
test<-Zooper(Data="Taxa")
mapDelta<-st_read("Data/DeltaShapeFile")

sumtest<-test%>%
  filter(Volume>1)%>%
  lazy_dt()%>%
  group_by(Taxname, Year, Latitude, Longitude)%>%
  summarise(CPUE=mean(CPUE))%>%
  as_tibble()
ggplot() + 
  geom_sf(data=mapDelta, color = "dodgerblue1", fill = "dodgerblue1") + 
  geom_point(data=filter(sumtest, Year==2003 & Taxname=="Pseudodiaptomus marinus"), aes(y=Latitude, x=Longitude, size=CPUE))+
  coord_sf()+
  theme_bw()+
  theme(panel.grid=element_blank())

mapDelta<-as(mapDelta, "Spatial")
test2<-filter(sumtest, Year==2003 & Taxname%in%c("Pseudodiaptomus marinus", "Pseudodiaptomus forbesi"))%>%
  mutate(Label=paste0(Taxname, ": ", round(CPUE)))

Specpal <- colorFactor(brewer.pal(7, "Dark2"), test2$Taxname)

leaflet(data=test2)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addCircles(radius = ~CPUE, weight = 1,
             fillColor = ~Specpal(Taxname), color=~Specpal(Taxname), fillOpacity = 0.7, #label = ~as.character(round(CPUE)),
             label = sapply(popupTable(test2%>%mutate(CPUE=round(CPUE)), c("Taxname", "CPUE"), row.numbers = F, feature.id = F), HTML))%>%
  addLegend("bottomright", pal = Specpal, values = ~Taxname)

#Community: Goal is to produce a plot of community composition in key regions across the delta (regions are those Morgan made from EDSM to use in the water conditions report). Need to think more about 1) what type of graph to put over map and 2) how to do it.
test<-Zooper()

#Load delta regions shapefile from Morgan
Deltaregions<-st_read("Data/Delta regions")
Deltaregions<-as(Deltaregions, "Spatial")

#Match each unique region from zoop dataset to a region from the shapefile
Locations<-test%>%dplyr::select(Latitude, Longitude)%>%distinct()%>%drop_na()
coordinates(Locations) <- ~Longitude+Latitude
proj4string(Locations) <- CRS("+proj=longlat +datum=NAD83")
Locations <- spTransform(Locations, proj4string(Deltaregions))
Locations<-Locations %over% Deltaregions
Locations<-test%>%
  dplyr::select(Latitude, Longitude)%>%
  distinct()%>%
  drop_na()%>%
  bind_cols(Locations%>%
              dplyr::select(Region=Stratum))%>%
  mutate(Region=replace_na(as.character(Region), "San Pablo Bay")) #San Pablo Bay not included in shapefile and all points outside the shapefile polygons are in San Pablo Bay


#Get average abundance of each Taxlifestage in each region and year
test2<-test%>%
  left_join(Locations, by=c("Latitude", "Longitude"))%>%
  group_by(Region)%>%
  mutate(Latitude=mean(range(Latitude)), Longitude=mean(range(Longitude)))%>%
  group_by(Region, Year, Taxlifestage, Latitude, Longitude)%>%
  summarise(CPUE=mean(CPUE))%>%
  ungroup()

##Need to compute centroids for each region for subsequent plot


#Taxa by regions
ggplot(filter(test2, Year==2000 & Taxlifestage=="Pseudodiaptomus marinus Adult"), aes(x=Region, y=CPUE))+
  geom_bar(stat="identity")+
  coord_cartesian(expand=0)+
  theme_bw()+
  theme(panel.grid=element_blank())
