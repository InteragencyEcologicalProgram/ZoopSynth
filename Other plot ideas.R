#Taxa
test<-Zooper(Data="Taxa")
require(sf)
mapDelta<-st_read("Data/DeltaShapeFile")

ggplot() + 
  geom_sf(data=mapDelta, color = "dodgerblue1", fill = "dodgerblue1") + 
  geom_point(data=filter(test, Year==2000 & Taxname=="Pseudodiaptomus marinus"), aes(y=Latitude, x=Longitude, size=CPUE))+
  coord_sf()+
  theme_bw()+
  theme(panel.grid=element_blank())