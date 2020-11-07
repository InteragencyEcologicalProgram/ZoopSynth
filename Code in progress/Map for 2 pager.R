require(tidyverse)
require(ggspatial)
require(sf)
require(maps)
require(readxl)
require(RColorBrewer)
require(stringr)
require(units)


Stations<-zooper::stations%>%
  filter(Source!="YBFMP")%>%
  mutate(Source=recode(Source, twentymm="20mm"))%>%
  bind_rows(read_excel("~/zoop sampling locations/UCDavis_CentforWatershedSci_ArcProject_ZoopSampleSites_102920.xlsx")%>%
              select(Station = `Site Code`, Latitude, Longitude)%>%
              mutate(Source="UCD"))%>%
  bind_rows(read_csv("~/zoop sampling locations/YB_StationCoordinates.csv", 
                     col_types=cols_only(StationCode="c", Latitude="d", Longitude="d"))%>%
              rename(Station=StationCode)%>%
              mutate(Source="YBFMP"))%>%
  drop_na()%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)%>%
  bind_rows(tibble(Source=c("ICF", "SFSU")))%>%
  mutate(Source=factor(Source, levels=c("EMP", "20mm", "FMWT", "STN", "FRP", "UCD", "YBFMP", "ICF", "SFSU")))%>%
  arrange(desc(Source))

DOP<-st_read("~/zoop sampling locations/doc.kml")%>%
  mutate(Source="ICF")

base<-deltamapr::WW_Watershed%>%
  st_transform(crs=4326)

Kim<-st_intersection(base, read_csv("~/zoop sampling locations/Kimmerer0607.csv")%>%
                       rename(Station=station)%>%
                       mutate(Station=as.character(Station),
                              Survey="0607")%>%
                       drop_na(lat.dd, long.dd)%>%
                       st_as_sf(coords=c("long.dd", "lat.dd"), crs=4326)%>%
                       st_union()%>%
                       st_transform(crs=26910)%>%
                       st_buffer(dist = set_units(1000, m))%>%
                       st_transform(crs=4326)%>%
                       st_convex_hull())%>%
  st_union()%>%
  st_transform(crs=26910)%>%
  st_buffer(dist = set_units(500, m))%>%
  st_transform(crs=4326)%>%
  st_sf()%>%
  bind_rows(read_csv("~/zoop sampling locations/Kimmerer1012.csv")%>%
              rename(Station=station)%>%
              mutate(Survey="1012",
                     Transect=str_extract(Station, "[A-Z]+"))%>%
              st_as_sf(coords=c("long.dd", "lat.dd"), crs=4326)%>%
              group_by(Transect)%>%
              summarise(geometry=st_union(geometry))%>%
              st_cast("MULTILINESTRING")%>%
              st_transform(crs=26910)%>%
              st_buffer(dist = set_units(500, m))%>%
              st_transform(crs=4326))%>%
  st_union()%>%
  st_sf()%>%
  mutate(Source="SFSU")

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
  st_make_valid()%>%
  st_crop(Stations)

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

pal<-RColorBrewer::brewer.pal(9, "Set1")
pal<-c(pal[1:5], pal[8], pal[9], pal[7], pal[6])

p<-ggplot() +
  geom_sf(data=base, fill="gray95", color="lightgray")+
  geom_point(data=Stations, aes(fill = Source, color=Source, x=Longitude, y=Latitude, 
                                shape=Source, stroke=if_else(Source%in%c("UCD", "YBFMP"), "1.5", "0.1"), alpha=if_else(Source%in%c("UCD", "YBFMP"), "0.8", "0.5")), 
             size=2.5)+
  geom_sf(data=Kim, aes(fill=Source), alpha=0.4, color=NA, show.legend = FALSE)+
  geom_sf(data=DOP, aes(fill=Source), alpha=0.1, color=NA, show.legend = FALSE)+
  geom_segment(data=labels, aes(x=label_lon, y=label_lat, xend=Longitude, yend=Latitude))+
  geom_label(data=labels, aes(label=label, x=label_lon, y=label_lat))+
  coord_sf(xlim=range(Stations$Longitude, na.rm=T), ylim=range(Stations$Latitude, na.rm=T))+
  scale_fill_manual(values=pal, name="Survey", aesthetics = c("fill", "color"))+
  scale_shape_manual(values=c(21:25, 13, 11, 22, 22), name="Survey", 
                     guide=guide_legend(override.aes = list(color=c(pal[1:7], NA, NA), size=c(rep(3, 7), 6,6), alpha=c(rep(1, 7), 0.1, 0.4), stroke=c(rep(0.1, 5), c(1.5,1.5), 0,0))))+
  discrete_scale("stroke", "Survey", palette=function (x) c(0.1, 1.5), guide=guide_none())+
  scale_alpha_discrete(name="Survey", palette=function (x) c(0.5, 0.8), guide=guide_none())+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", pad_y=unit(0.05, "npc"), which_north = "true")+
  theme_bw()+
  theme(legend.background = element_rect(color="black"), legend.position=c(0.925,0.78))+
  annotation_custom(
    grob = ggplotGrob(pout),
    xmin = -Inf,
    xmax = -122.2,
    ymin = 38.3,
    ymax = Inf
  )
p
ggsave("Code in progress/2-pager map.png", plot=p, device="png", width=8, height=8, units = "in")
