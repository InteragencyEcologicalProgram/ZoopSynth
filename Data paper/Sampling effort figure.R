require(zooper)
require(dplyr)
require(lubridate)
require(ggplot2)
require(patchwork)

Effort<-zoopEnvComb%>%
  mutate(Month=month(Date, label=T),
         Source=recode(Source, twentymm="20mm"))%>%
  group_by(Source, Month, Station)%>%
  summarise(N=n(), N_years=length(unique(Year)), .groups="drop")%>%
  mutate(N_yearly=N/N_years)%>%
  group_by(Source, Month)%>%
  summarise(N=mean(N_yearly), .groups="drop")

p_effort<-ggplot(Effort, aes(x=Month, y=Source, fill=N))+
  geom_tile()+
  geom_text(aes(label=format(round(N,2)), color=if_else(N>4, TRUE, FALSE)), show.legend = FALSE)+
  scale_x_discrete(expand=expansion(0,0))+
  scale_y_discrete(expand=expansion(0,0), limits=c("FRP", "STN","FMWT", "20mm", "EMP"))+
  ylab("Survey")+
  scale_fill_viridis_c(name="Average number \nof samples per \nyear and station")+
  scale_color_manual(values=c("White", "black"))+
  theme_bw()+
  theme(panel.grid=element_blank(), legend.position = "none")
  
Years<-zoopEnvComb%>%
  mutate(Source=recode(Source, twentymm="20mm"))%>%
  group_by(Source)%>%
  summarise(min_year=min(Year), max_year=max(Year), .groups="drop")

p_years<-ggplot(Years, aes(y=Source, x=min_year, xend=max_year, yend=Source))+
  geom_segment(size=1)+
  geom_point(size=2)+
  scale_y_discrete(limits=c("FRP", "STN","FMWT", "20mm", "EMP"))+
  xlab("Year")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major.y =element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())

p<-p_effort+p_years+plot_layout(widths=c(0.8, 0.2))+plot_annotation(tag_levels = "A", tag_suffix = ")")

ggsave(plot=p, filename="Data paper/Sampling effort.png", device="png", units="in", height=5, width=10)
