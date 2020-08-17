require(zooper)
require(dplyr)
require(lubridate)
require(ggplot2)
require(patchwork)
require(purrr)

# Plotting monthly sampling effort by year and survey ---------------------

Effort<-zoopEnvComb%>%
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
  #complete(Source, Year, Month=unique(Effort$Month))%>%
  #fill(N)

effort_plot<-function(source){
  Color<-case_when(source=="EMP" ~ "#e41a1c",
                   source=="20mm" ~ "#377eb8",
                   source=="FMWT" ~ "#4daf4a",
                   source=="STN" ~ "#984ea3",
                   source=="FRP" ~ "#ff7f00")
  
  p<-ggplot(filter(Effort, Source==source), aes(x=Year, y=N))+
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

ggsave(plot=p2, filename="Data paper/Sampling Effort.png", device="png", units="in", height=10, width=9)
