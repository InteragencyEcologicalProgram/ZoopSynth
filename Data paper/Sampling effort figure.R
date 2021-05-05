require(zooper)
require(tidyr)
require(dplyr)
require(lubridate)
require(ggplot2)
require(patchwork)
require(purrr)
require(ggstance)

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

# Alternative effort plot
Effort2_year<-Effort%>%
  group_by(Source, Year)%>%
  summarise(N=mean(N, na.rm=T), .groups="drop")%>%
  filter(!is.na(N))

p_year<-ggplot(Effort2_year, aes(y=Source, x=Year, fill=Source, height=N/10))+
  annotate(geom="rect", xmin=1970, xmax=1989, ymin=1, ymax=2.5, color="black", fill="white", size=1.5)+
  annotate(geom="text", x=c(seq(1975, 1984, by=3), 1979.5), y=c(rep(1.2, 4), 2.1), label=c(1:4, "Mean monthly\nsampling frequency"), lineheight=1)+
  annotate("tile", x=seq(1975, 1984, by=3), y=1.5, height=(1:4)/10, width=2)+
  geom_tile(width=1, show.legend=T)+
  ylab("Survey")+
  scale_y_discrete(limits=rev)+
  scale_fill_brewer(type="qual", palette="Set1", guide="none", na.value="black")+
  theme_bw()+
  theme(legend.position = c(0.3, 0.2), panel.grid.minor = element_blank())

Effort2_month<-Effort%>%
  mutate(N=replace_na(N, 0))%>%
  left_join(startDates%>%
              group_by(Source)%>%
              summarise(StartYear=year(min(Startdate)), .groups="drop")%>%
              mutate(Source=recode(Source, twentymm="20mm")),
            by="Source")%>%
  filter(Year>=StartYear)%>%
  group_by(Source, Month)%>%
  summarise(N=mean(N, na.rm=T), .groups="drop")%>%
  filter(N>0)%>%
  mutate(Month=as.integer(Month))%>%
  mutate(Source=factor(Source, levels=c("EMP", "20mm","FMWT", "STN", "FRP")))

p_month<-ggplot(Effort2_month, aes(y=Source, x=Month, fill=Source, height=N/10))+
  annotate(geom="rect", xmin=0.6, xmax=6, ymin=2.35, ymax=3.75, color="black", fill="white", size=1.5)+
  annotate(geom="tile", x=1:5+0.3, y=2.8, height=seq(0.5, 2.5, by=0.5)/10, width=0.8)+
  annotate(geom="text", x=c(1:5, 3)+0.3, y=c(rep(2.55, 5), 3.35), label=c(seq(0.5, 2.5, by=0.5), "Mean monthly\nsampling frequency"), lineheight=1)+
  geom_tile(width=1, show.legend=T)+
  ylab("Survey")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(breaks=c(1:12+0.5, 1:12), labels=c(rep("", 12), as.character(month(1:12, label=T))),
                     expand=expansion(0,0), limits=c(0.5, 12.5))+
  scale_fill_brewer(type="qual", palette="Set1", guide="none", na.value="black")+
  theme_bw()+
  theme(legend.position = c(0.3, 0.2), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c(rep("black", 12), rep(NA, 12))), panel.grid.major.x=element_line(color = c(rep("grey92", 12), rep(NA, 12))))

p_effort<-p_year/p_month

ggsave(plot=p_effort, filename="Data paper/Sampling Effort_improved.png", device="png", units="in", height=7, width=6)

# Another improved version

p<-ggplot(Effort, aes(y=Month, x=Year, fill=Source, group=Source, height=N/15))+
  geom_tile(aes(y=as.integer(Month)+as.integer(Source)/8), width=1, show.legend=T, alpha=0.6)+
  scale_y_reverse(breaks=c(1:12, 1:12+0.5), labels=c(rep("", 12), as.character(month(1:12, label=T))),
                  expand=expansion(0,0), limits=c(13, 1))+
  scale_fill_brewer(type="qual", palette="Set1", na.value="black")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), text=element_text(size=18),
        axis.ticks.y = element_line(color = c(rep("black", 12), rep(NA, 12))), panel.grid.major.y=element_line(color = c(rep("grey92", 12), rep(NA, 12))))
ggsave(plot=p, filename="Data paper/Sampling Effort_improved2.png", device="png", units="in", height=15, width=8)
