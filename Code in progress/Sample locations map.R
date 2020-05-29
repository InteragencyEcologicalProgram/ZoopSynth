require(leaflet)
require(mapview)
require(RColorBrewer)
require(readxl)
require(tidyverse)
require(ggplot2)
require(ggspatial)
require(sf)
require(ggrepel)
library(maps)

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
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

base<-spacetools::Delta%>%
  st_transform(crs=4326)

labels<-tibble(label=c("San Francisco Bay", "San Pablo Bay", "Suisun Bay", "Confluence"), Latitude=c(37.9, 38.07, 38.08, 38.046), Longitude=c(-122.4, -122.4, -122.05, -121.9),
               label_lat=c(37.9, 38.11, 38.15, 38), label_lon=c(-122.25, -122.38, -122.18, -122))#%>%
  #bind_rows(Stations%>%
  #            st_drop_geometry()%>%
  #            select(Latitude, Longitude)%>%
  #            mutate(label=""))

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))%>%
  st_transform(crs=st_crs(base))
california<-filter(states, ID=="california")

base2<-base%>%
  st_crop(st_bbox(Stations))

station_lims<-st_bbox(Stations)

pout<-ggplot(states)+
  geom_sf(color="dodgerblue3")+
  geom_sf(data=base2, color="dodgerblue3", fill="dodgerblue3")+
  geom_rect(xmin = station_lims["xmin"]-0.2, xmax = station_lims["xmax"]+0.2, ymin = station_lims["ymin"]-0.2, ymax = station_lims["ymax"]+0.2, 
            fill = NA, colour = "black", size = 1)+
  coord_sf(xlim=c(st_bbox(california)["xmin"], st_bbox(california)["xmax"]), ylim=c(st_bbox(california)["ymin"], st_bbox(california)["ymax"]))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "dodgerblue3"), axis.text.x=element_text(angle=45, hjust=1))
pout

p<-ggplot() +
  geom_sf(data=base, fill="gray95", color="lightgray")+
  geom_point(data=Stations, aes(fill = Source, x=Longitude, y=Latitude), alpha=0.5, color="black", stroke=0.1, shape=21, size=2.5)+
  geom_segment(data=labels, aes(x=label_lon, y=label_lat, xend=Longitude, yend=Latitude))+
  geom_label(data=labels, aes(label=label, x=label_lon, y=label_lat))+
  coord_sf(xlim=range(Stations$Longitude), ylim=range(Stations$Latitude))+
  scale_fill_brewer(type="qual", palette="Set1", guide=guide_legend(override.aes = list(alpha=1)), name="Survey")+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", pad_y=unit(0.05, "npc"), which_north = "true")+
  theme_bw()+
  theme(legend.background = element_rect(color="black"), legend.position=c(0.925,0.85))+
  annotation_custom(
    grob = ggplotGrob(pout),
    xmin = -Inf,
    xmax = -122.2,
    ymin = 38.3,
    ymax = Inf
  )
p
ggsave("Code in progress/Samples map improved.png", plot=p, device="png", width=8, height=8, units = "in")
