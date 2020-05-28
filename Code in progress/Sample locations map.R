require(leaflet)
require(mapview)
require(RColorBrewer)
require(readxl)
require(tidyverse)
require(ggplot2)
require(ggspatial)
require(sf)

Stations <- zooper::stations%>%
  filter(Source!="YBFMP")%>%
  #bind_rows(zooper::stationsEMPEZ%>%
  #            mutate(Station=paste(Station, Date),
  #                   Source="EMP")%>%
  #            select(-Date))%>%
  mutate(Source=recode(Source, twentymm="20mm"))%>%
  drop_na()

pal<-colorFactor(brewer.pal(5, "Dark2"), Stations$Source)

addLegendCustom <- function(map, sizes, ...){
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, 
                   opacity = opacity, position=position, title=title))
}

#Zoomed in
p<-leaflet(data=Stations)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  setView(lng=-121.873, lat=38.190039, zoom=11)%>%
  addCircleMarkers(lat=~Latitude, lng=~Longitude, fillColor = ~pal(Source), color = "Black", radius=5, weight=2, fillOpacity = 0.4)%>%
  addLegend("topleft", pal = pal, values = ~Source)

mapview::mapshot(p, file="Samples map in.png", vheight=1333, vwidth=1690)

tags$head(
  tags$style(
    ".sam{
          line-height: 30px;
          font-size: 30px;
          width: 30px;
          height: 30px;
    }"
  )
)

## To increase legend size, add the following to R/R-3.6.1/library/leaflet/htmlwidgets/lib/leafletfix/leafletfix.css
#.legend-big {
#  line-height: 30px;
#  font-size: 30px;
#  color: #555;
#}

#.legend-big i {
#  width: 30px;
#  height: 30px;
#  margin-right: 4px;
#  opacity: 0.7;
#  display: inline-block;
#  vertical-align: top;
#  /*For IE 7*/
#    zoom: 1;
#  *display: inline;
#}

#Zoomed out
p<-leaflet(data=Stations)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  setView(lng=-121.873, lat=38.190039, zoom=10)%>%
  addCircleMarkers(lat=~Latitude, lng=~Longitude, fillColor = ~pal(Source), color = "Black", radius=5, weight=2, fillOpacity = 0.4)%>%
  addLegend("topleft", pal = pal, values = ~Source, className = "info legend, legend-big")
p
mapview::mapshot(p, file="Code in progress/Samples map out.png", vheight=800, vwidth=1000)



# ggplot2 version ---------------------------------------------------------

Stations<-Stations%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)

base<-spacetools::Delta%>%
  st_transform(crs=4326)

labels<-tibble(label=c("San Francisco Bay", "San Pablo Bay", "Suisun Bay", "Confluence"), Lat=c(37.9, 38.12, 38.02, 37.98), Lon=c(-122.3, -122.4, -122.05, -121.8))

ggplot() +
  annotation_spatial(base, fill="lightgray", color="lightgray")+
  layer_spatial(Stations, aes(fill = Source), alpha=0.7, color="black", stroke=0.1, shape=21, size=2.5)+
  geom_spatial_label(data=labels, aes(label=label, x=Lon, y=Lat), crs=4326)+
  scale_fill_brewer(type="qual", palette="Dark2")+
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "tr", which_north = "true")+
  theme_bw()
