require(tidyverse)
require(ggspatial)
require(sf)
require(maps)

Stations<-zooper::stations%>%
  filter(Source!="YBFMP")%>%
  mutate(Source=recode(Source, twentymm="20mm"))%>%
  drop_na()%>%
  mutate(Source=factor(Source, levels=c("EMP", "20mm", "FMWT", "STN", "FRP")))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

base<-spacetools::Delta%>%
  st_transform(crs=4326)

labels<-tibble(label=c("San Francisco Bay", "San Pablo Bay", "Suisun Bay", "Suisun Marsh", 
                       "Confluence", "Cache Slough", "Sacramento River", "San Joaquin River", "Napa River"), 
               Latitude=c(37.9, 38.07, 38.08, 38.2, 38.046, 38.23, 38.5, 37.9, 38.23), 
               Longitude=c(-122.4, -122.4, -122.05, -122.05, -121.9, -121.675, -121.56, -121.325, -122.3),
               label_lat=c(37.9, 38.11, 38.15, 38.25, 38, 38.2, 38.5, 37.85, 38.25), 
               label_lon=c(-122.25, -122.38, -122.18, -122.18, -122, -121.8, -121.73, -121.42, -122.37))

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
            fill = NA, colour = "black", size = 0.7)+
  coord_sf(xlim=c(st_bbox(california)["xmin"], st_bbox(california)["xmax"]), ylim=c(st_bbox(california)["ymin"], st_bbox(california)["ymax"]))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "dodgerblue3"), axis.text.x=element_text(angle=45, hjust=1))
pout

p<-ggplot() +
  geom_sf(data=base, fill="gray95", color="lightgray")+
  geom_point(data=Stations, aes(fill = Source, x=Longitude, y=Latitude, shape=Source), alpha=0.5, color="black", stroke=0.1, size=2.5)+
  geom_segment(data=labels, aes(x=label_lon, y=label_lat, xend=Longitude, yend=Latitude))+
  geom_label(data=labels, aes(label=label, x=label_lon, y=label_lat))+
  coord_sf(xlim=range(Stations$Longitude), ylim=range(Stations$Latitude))+
  scale_fill_brewer(type="qual", palette="Set1", name="Survey")+
  scale_shape_manual(values=21:25, name="Survey")+
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
ggsave("C:/Users/sbashevkin/OneDrive - deltacouncil/Zooplankton synthesis/Data paper/Figures/Figure 1.png", plot=p, device="png", width=8, height=8, units = "in")

p_FFT<-ggplot() +
  geom_sf(data=base, fill="gray95", color="lightgray")+
  geom_point(data=Stations, aes(x=Longitude, y=Latitude), shape=21, alpha=0.5, color="black", fill="#984EA3", stroke=0.1, size=2.5)+
  geom_segment(data=labels, aes(x=label_lon, y=label_lat, xend=Longitude, yend=Latitude))+
  geom_label(data=labels, aes(label=label, x=label_lon, y=label_lat))+
  coord_sf(xlim=range(Stations$Longitude), ylim=range(Stations$Latitude))+
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
p_FFT
ggsave("C:/Users/sbashevkin/OneDrive - deltacouncil/Zooplankton synthesis/Food for thought/Map.png", plot=p_FFT, device="png", width=8, height=8, units = "in")
