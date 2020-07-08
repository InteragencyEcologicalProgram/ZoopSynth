require(tidyverse)
require(zooper)
require(broom)
require(RColorBrewer)

EMP<-zooper::zoopComb%>%
  left_join(zooper::zoopEnvComb, by=c("Source", "SampleID"))%>%
  filter(Source=="EMP" & SizeClass%in%c("Micro", "Meso"))

Taxa<-EMP%>%
  group_by(SizeClass)%>%
  summarise(Taxa=list(unique(Taxlifestage)), .groups="drop")

Taxa<-intersect(Taxa$Taxa[1][[1]], Taxa$Taxa[2][[1]])

EMP<-EMP%>%
  filter(Taxlifestage%in%Taxa)

Samples<-EMP%>%
  group_by(SizeClass)%>%
  summarise(Samples=list(unique(SampleID)), .groups="drop")

Samples<-intersect(Samples$Samples[1][[1]], Samples$Samples[2][[1]])

EMP<-EMP%>%
  filter(SampleID%in%Samples)%>%
  mutate(Taxname_label=ifelse(Taxname%in%c("Asplanchna", "Keratella", "Limnoithona", "Limnoithona sinensis", "Limnoithona tetraspina", "Oithona", "Oithona davisae", "Oithona similis", "Polyarthra", "Synchaeta", "Synchaeta bicornis", "Trichocerca", "Eurytemora affinis", "Pseudodiaptomus", "Sinocalanus doerrii"), paste0("italic('", Taxname, "')"),
                              Taxname)) #create variable for combo taxonomy x life stage and make some names italicized for graph. 

EMP_sum<-EMP%>%
  group_by(SizeClass, Taxname, Lifestage, Taxname_label)%>%
  summarise(N_detect=length(which(CPUE>0)),
            CPUE=sum(CPUE, na.rm=T), .groups="drop")%>%
  group_by(Taxname, Lifestage, Taxname_label)%>%
  mutate(Total=sum(CPUE, na.rm=T),
         N_detect=sum(N_detect))%>%
  ungroup%>%
  mutate(Proportion=CPUE/Total)%>%
  arrange(Taxname, Lifestage)%>%
  mutate(Taxname_label=factor(Taxname_label, levels=unique(Taxname_label)),
         N_detect=format(N_detect, big.mark=","))

p<-ggplot(EMP_sum)+
  geom_bar(aes(x=SizeClass, y=Proportion, fill=SizeClass), stat="identity")+
  geom_text(aes(label=format(round(CPUE), big.mark=","), x=SizeClass, y=1.1), size=3)+
  facet_wrap(~Taxname_label*Lifestage, labeller = label_parsed, ncol=4)+
  scale_fill_manual(values=brewer.pal(3, "PRGn")[c(1,3)])+
  scale_y_continuous(limits=c(0,1.2), expand=c(0,0), breaks=c(0,0.25, 0.5, 0.75, 1))+
  ylab("Proportion of total CPUE")+
  xlab("Collection method")+
  theme_bw()+
  theme(text=element_text(size=14), panel.grid=element_blank(), strip.background=element_blank(), legend.position = "none", strip.text.x = element_text(margin = margin(b = 0, t = 0)))
p
ggsave(p, file="Code in progress/Pump CB CPUEs bar.png", device = "png", units="in", height=8, width=7.5)


# using zooper data -------------------------------------------------------

EMP2<-zooper::zoopComb%>%
  left_join(zooper::zoopEnvComb, by=c("Source", "SampleID"))%>%
  filter(Source=="EMP" & SizeClass%in%c("Micro", "Meso"))

Taxa<-EMP2%>%
  group_by(SizeClass)%>%
  summarise(Taxa=list(unique(Taxlifestage)), .groups="drop")

Taxa<-intersect(Taxa$Taxa[1][[1]], Taxa$Taxa[2][[1]])

EMP2<-EMP2%>%
  filter(Taxlifestage%in%Taxa)

Samples<-EMP2%>%
  group_by(SizeClass)%>%
  summarise(Samples=list(unique(SampleID)), .groups="drop")

Samples<-intersect(Samples$Samples[1][[1]], Samples$Samples[2][[1]])

EMP2<-EMP2%>%
  filter(SampleID%in%Samples)

EMP3<-EMP2%>%
  mutate(Taxname_label=ifelse(Taxname%in%c("Asplanchna", "Keratella", "Limnoithona", "Limnoithona sinensis", "Limnoithona tetraspina", "Oithona", "Oithona davisae", "Oithona similis", "Polyarthra", "Synchaeta", "Synchaeta bicornis", "Trichocerca", "Eurytemora affinis", "Pseudodiaptomus", "Sinocalanus doerrii"), paste0("italic('", Taxname, "')"),
                              Taxname)) #create variable for combo taxonomy x life stage and make some names italicized for graph. 
  
EMP_sum2<-EMP3%>%
  group_by(SizeClass, Taxname, Lifestage, Taxname_label)%>%
  summarise(N_detect=length(which(CPUE>0)),
            CPUE=sum(CPUE, na.rm=T))%>%
  ungroup()%>%
  group_by(Taxname, Lifestage, Taxname_label)%>%
  mutate(Total=sum(CPUE, na.rm=T),
         N_detect=sum(N_detect))%>%
  ungroup%>%
  mutate(Proportion=CPUE/Total)%>%
  arrange(Taxname, Lifestage)%>%
  mutate(Taxname_label=factor(Taxname_label, levels=unique(Taxname_label)),
         N_detect=format(N_detect, big.mark=","))

# Statistics --------------------------------------------------------------



EMP_xyplot<-EMP%>%
  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
  spread(key = Method, value = CPUE)%>%
  select(-Taxname, -Lifestage, -Year)%>%
  mutate(CB_l=log(CB+1),
         Pump_l=log(Pump+1))%>%
  select(-CB, -Pump)

EMP_model<- function(df) {
  lm(Pump_l ~ CB_l, data = df)
}

EMP_xyplot_models<-EMP_xyplot%>%
  group_by(Taxlifestage)%>%
  nest()%>% 
  mutate(model = map(data, EMP_model))%>%
  select(-data, )%>%
  mutate(model_sum=map(model, tidy))%>%
  unnest(model_sum)%>%
  select(-p.value, -statistic, -std.error)%>%
  spread(key=term, value=estimate)%>%
  rename(Intercept=`(Intercept)`, Slope=CB_l)%>%
  mutate(model_sum=map(model, glance))%>%
  unnest(model_sum)%>%
  select(-model, -sigma, -statistic, -df, -logLik, -AIC, -BIC, -deviance, -df.residual)

EMP_xyplot_stats<-EMP_xyplot%>%
  group_by(Taxlifestage)%>%
  summarise(CB_l_max=max(CB_l), Pump_l_max=max(Pump_l))%>%
  left_join(EMP_xyplot_models%>%
              select(Taxlifestage, r.squared, p.value),
            by="Taxlifestage")%>%
  mutate(r.squared=round(r.squared, 2),
         p.value=round(p.value, 4))

p<-ggplot(EMP_xyplot, aes(x=CB_l, y=Pump_l, group=Taxlifestage))+
  geom_point(alpha=0.2)+
  geom_abline(data=EMP_xyplot_models, aes(intercept=Intercept, slope=Slope, group=Taxlifestage), color="blue")+
  geom_text(data=EMP_xyplot_stats, aes(x=CB_l_max*0.85, y=Pump_l_max*0.1, label=paste0("R2 = ", r.squared)), size=3)+
  facet_wrap(~Taxlifestage, scales="free")+
  scale_y_continuous(limits=c(0,NA), expand=expand_scale(mult=c(0.01,0.05)))+
  scale_x_continuous(limits=c(0,NA), expand=expand_scale(mult=c(0.01,0.05)))+
  xlab("log(CB CPUE)")+
  ylab("log(Pump CPUE)")+
  theme_bw()+
  theme(panel.grid=element_blank(), strip.background=element_blank())

ggsave(p, file="Pump CB CPUEs.png", device = "png", units="in", height=8, width=10)


# stats -------------------------------------------------------------------

EMP_stats<-EMP%>%
  select(-Taxname_label)%>%
  pivot_wider(names_from=Method)
