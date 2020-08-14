require(zooper)
require(dplyr)
require(lubridate)
require(ggplot2)
require(patchwork)
require(purrr)

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


# Plotting monthly sampling effort by year and survey ---------------------

Effort2<-zoopEnvComb%>%
  mutate(Month=month(Date, label=T),
         Source=recode(Source, twentymm="20mm"))%>%
  group_by(Source, Month, Year, Station)%>%
  summarise(N=n(), .groups="drop")%>%
  group_by(Source, Month, Year)%>%
  summarise(N=mean(N), .groups="drop")%>%
  mutate(Source=factor(Source, levels=c("EMP", "20mm","FMWT", "STN", "FRP")))%>%
  complete(Source, Month, Year)

axis_lims<-tibble(Source=c("EMP", "20mm","FMWT", "STN", "FRP"), N=c(2.7, 2.7, 2.7, 2.7, 7.2), Year=2000)%>%
  mutate(Source=factor(Source, levels=c("EMP", "20mm","FMWT", "STN", "FRP")))#%>%
  #complete(Source, Year, Month=unique(Effort2$Month))%>%
  #fill(N)

effort_plot<-function(source){
  Color<-case_when(source=="EMP" ~ "#e41a1c",
                   source=="20mm" ~ "#377eb8",
                   source=="FMWT" ~ "#4daf4a",
                   source=="STN" ~ "#984ea3",
                   source=="FRP" ~ "#ff7f00")
  
  p<-ggplot(filter(Effort2, Source==source), aes(x=Year, y=N))+
    geom_line(size=1, color=Color)+
    geom_point(size=1, color=Color)+
    geom_blank(data=filter(axis_lims, Source==source))+
    facet_grid(Month~Source)+
    scale_color_brewer(type="qual", palette="Set1")+
    scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010))+
    {if(source%in%c("FRP")){
      scale_y_continuous(expand=expansion(0,0), limits=c(0,NA))
    } else{
      scale_y_continuous(expand=expansion(0,0), limits=c(0,NA), breaks=c(0,1,2))
    }}+
    ylab("Average number of samples per station")+
    theme_bw()+
    {if(source%in%c("20mm", "FMWT", "STN")){
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
    }}+
    {if(source%in%c("20mm", "FMWT", "STN", "EMP")){
      theme(strip.text.y=element_blank())
    }}+
    {if(source%in%c("FRP")){
      theme(axis.title.y = element_blank())
    }}+
    theme(strip.background=element_blank(), text=element_text(size=14), legend.position="none", axis.text.x=element_text(angle=45, hjust=1))
  return(p)
}

p<-purrr::map(c("EMP", "20mm","FMWT", "STN", "FRP"), effort_plot)

p2<-wrap_plots(p)+plot_layout(nrow=1)

ggsave(plot=p2, filename="Data paper/Sampling effort2.png", device="png", units="in", height=10, width=9)
