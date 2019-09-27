require(leaflet)
require(mapview)
require(RColorBrewer)
require(readxl)
require(tidyverse)

Stations <- read_excel("Data/zoop_stations.xlsx", sheet="lat_long")%>%
  rename(Source=Project)%>%
  filter(Source!="YBFMP")%>%
  drop_na()

pal<-colorFactor(brewer.pal(5, "Dark2"), Stations$Source)

p<-leaflet(data=Stations)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  setView(lng=-121.873, lat=38.190039, zoom=11)%>%
  addCircleMarkers(lat=~Latitude, lng=~Longitude, fillColor = ~pal(Source), color = "Black", radius=5, weight=2, fillOpacity = 0.4)%>%
  addLegend("topleft", pal = pal, values = ~Source)

mapview::mapshot(p, file="Samples map.png", vheight=1333, vwidth=1690)
