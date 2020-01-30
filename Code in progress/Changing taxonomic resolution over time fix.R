require(tidyverse) 
require(readxl)
require(lubridate)
require(zooper)

# Load crosswalk key to convert each dataset's taxonomic codes to a
# unified set of "Taxname" and "Lifestage" values.

crosswalk <- zooper::crosswalk%>%
  mutate_at(vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~parse_date(as.character(.), format="%Y"))%>%
  mutate_at(vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
  mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(FMWTend = if_else(is.finite(FMWTend), FMWTend+years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(twentymmend = if_else(is.finite(twentymmend), twentymmend+years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
  mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)

#Calculate years when taxa were not counted but should have been

Uncountedyears<- function(Source, SizeClass, Crosswalk){
  start<-sym(paste0(Source, "start"))
  start<-enquo(start)
  end<-sym(paste0(Source, "end"))
  end<-enquo(end)
  dataset<-sym(paste(Source, SizeClass, sep="_"))
  dataset<-enquo(dataset)
  
  start2<-sym(paste0(Source, "start2"))
  start2<-enquo(start2)
  
  out<-Crosswalk%>%
    filter(!is.na(!!dataset))%>%
    mutate(!!start := if_else(!is.finite(!!start), Sys.Date(), !!start))%>%
    mutate(Intro = if_else(!is.finite(Intro) | Intro > !!start, !!start, Intro),
           !!end := if_else(!is.finite(!!end), Sys.Date(), !!end))%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
    {if(quo_name(start2)%in%names(Crosswalk)){
      select(., Taxname, Lifestage, Taxlifestage, !!start, !!start2, !!end, Intro, !!dataset)%>%
        group_by(Taxname, Lifestage, !!dataset, Taxlifestage, !!start, !!start2, !!end, Intro, n = row_number())%>%
        do(tibble(Years = list(c(
          seq(year(.$Intro), year(.[[quo_name(start)]]), by = 1), 
          seq(year(.[[quo_name(end)]]), 
              if_else(is.finite(.[[quo_name(start2)]]), year(.[[quo_name(start2)]])-1, year(Sys.Date())), by=1)))))
    } else{
      select(., Taxname, Lifestage, Taxlifestage, !!start, !!end, Intro, !!dataset)%>%
        group_by(Taxname, Lifestage, !!dataset, Taxlifestage, !!start, !!end, Intro, n = row_number())%>%
        do(tibble(Years = list(c(
          seq(year(.$Intro), year(.[[quo_name(start)]]), by = 1), 
          seq(year(.[[quo_name(end)]]), year(Sys.Date()), by=1)))))
    }}%>%
    ungroup()%>%
    select(-n)%>%
    unnest(cols=c(Years))%>%
    mutate(Years=ifelse(Years==year(Intro) | Years==year(Sys.Date()), NA, Years))%>%
    group_by(Taxlifestage, Taxname, Lifestage, !!dataset)%>%
    summarise(Years=list(unique(Years)))%>%
    group_by(Taxlifestage, Taxname, Lifestage)%>%
    summarise(Years=list(Reduce(intersect, Years)))%>%
    ungroup()%>%
    unnest(cols=c(Years))%>%
    filter(!is.na(Years))%>%
    mutate(Source=Source,
           SizeClass=SizeClass)%>%
    group_by(Taxlifestage, Taxname, Lifestage, Source, SizeClass)%>%
    summarise(Years=list(unique(Years)))
  return(out)
}

datasets<-zoopComb%>%
  mutate(names=paste(Source, SizeClass, sep="_"))%>%
  select(names, Source, SizeClass)%>%
  filter(Source%in%c("EMP", "FMWT", "twentymm"))%>%
  distinct()

test<-map2_dfr(datasets$Source, datasets$SizeClass, Uncountedyears, Crosswalk=crosswalk)

