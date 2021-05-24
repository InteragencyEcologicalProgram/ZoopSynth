require(dplyr)
require(zooper)
require(ggplot2)
require(stringr)
options(scipen=999)

data_com<-Zoopsynther("Community", Shiny = T)

Removed<-unlist(str_split(str_split(data_com$Caveats, ", ")[[1]], ": "))

Data_unfiltered<-data_com$Data%>%
  group_by(SampleID)%>%
  summarise(CPUE_com=sum(CPUE), .groups="drop")%>%
  full_join(zooper::zoopComb%>%
              group_by(SampleID)%>%
              summarise(CPUE_base=sum(CPUE), .groups="drop"),
            by="SampleID")%>%
  filter(SampleID!="FMWT 912 2016-12-07") # Sample with only an amphipod data point that would not be present in the community dataset



# Now try removing problematic taxa and ensuring that total CPUEs match exactly
Data_base_filtered<-zooper::zoopComb%>%
  filter(!Taxlifestage%in%Removed)

data_com_filtered<-Zoopsynther("Community", Zoop=Data_base_filtered)

Data_filtered<-data_com_filtered%>%
  group_by(SampleID)%>%
  summarise(CPUE_com=sum(CPUE), .groups="drop")%>%
  full_join(Data_base_filtered%>%
              group_by(SampleID)%>%
              summarise(CPUE_base=sum(CPUE), .groups="drop"),
            by="SampleID")
all(near(Data_filtered$CPUE_com, Data_filtered$CPUE_base))



m<-lm(CPUE_com~CPUE_base, data=Data_unfiltered)
m_sum<-summary(m)

m_filtered<-lm(CPUE_com~CPUE_base, data=Data_filtered)
m_filtered_sum<-summary(m_filtered)

p<-ggplot(Data_unfiltered, aes(x=CPUE_base, y=CPUE_com))+
  geom_point(size=1, alpha=0.1)+
  scale_x_continuous(labels = function(x) format(x, big.mark = ","), expand=expansion(0.01,0))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ","), expand=expansion(0.01,0))+
  annotate("label", x=2000000, y=6000000, label=paste0("R^2 == ", m_sum$r.sq), parse=T)+
  xlab("Raw data total CPUE per sample")+
  ylab("Community-corrected data total CPUE per sample")+
  theme_bw()
p
ggsave("C:/Users/sbashevkin/OneDrive - deltacouncil/Zooplankton synthesis/Data paper/Figures/Figure 6.png", plot=p, device="png", width=4, height=4, units = "in")
