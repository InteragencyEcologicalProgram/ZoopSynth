Zoopdownloader <- function(path="Data/zoopforzooper.Rds", ReDownloadData=F){
  
  # Setup -------------------------------------------------------------------
  require(tidyverse) 
  require(readxl)
  
  #Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
  require(dtplyr)
  require(lubridate)
  
  # Load crosswalk key to convert each dataset's taxonomic codes to a
  # unified set of "Taxname" and "Lifestage" values.
  
  crosswalk <- read_excel("Data/new_crosswalk.xlsx", sheet = "Hierarchy2")%>%
    mutate_at(vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~parse_date(as.character(.), format="%Y"))%>%
    mutate_at(vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
    mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(FMWTend = if_else(is.finite(FMWTend), FMWTend+years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(twentymmend = if_else(is.finite(twentymmend), twentymmend+years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)
  
  # Load station key to later incorporate latitudes and longitudes
  
  stations <- read_excel("Data/zoop_stations.xlsx", sheet="lat_long")%>%
    rename(Source=Project)
  
  # Initialize list of dataframes
  
  data.list<-list()
  
  
  
  # EMP ---------------------------------------------------------------------
  
  
  #download the file
  if (!file.exists("Data/1972-2018CBMatrix.xlsx") | ReDownloadData) {
    download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx", 
                  "Data/1972-2018CBMatrix.xlsx", mode="wb")
  }
  
  
  # Import the EMP data
  
  zoo_EMP <- read_excel("Data/1972-2018CBMatrix.xlsx", 
                        sheet = "CB CPUE Matrix 1972-2018", 
                        col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                      "text", "text", "text", "numeric", "text", "text",
                                      "text", rep("numeric", 62)))
  
  # Tranform from "wide" to "long" format, add some variables, 
  # alter data to match other datasets
  
  data.list[["EMP"]] <- zoo_EMP%>%
    mutate(Datetime=suppressWarnings(parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
    gather(key="EMP", value="CPUE", -SurveyCode, -Year, -Survey, -SurveyRep, 
           -Date, -Station, -EZStation, -DWRStation, 
           -Core, -Region, -Secchi, -`Chl-a`, -Temperature,
           -ECSurfacePreTow, -ECBottomPreTow, -CBVolume, -Datetime, -Time, -TideCode)%>% #transform from wide to long
    mutate(Source="EMP")%>% #add variable for data source
    select(Source, Year, Date, Datetime, Tide=TideCode, 
           Station, Region, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, 
           Temperature, Volume = CBVolume, EMP, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(EMP, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                filter(!is.na(EMP))%>% #Only retain Taxnames corresponding to EMP codes
                distinct(),
              by="EMP")%>%
    filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
    mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
           SampleID=paste(Source, Station, Date))%>% #Create identifier for each sample
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
      CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
      CPUE==0 & Date >= EMPend ~ NA_real_
    ))%>% 
    select(-EMP, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes 
    lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    group_by_at(vars(-CPUE))%>%
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    ungroup()%>%
    as_tibble() #required to finish operation after lazy_dt()
  
  
  # FMWT --------------------------------------------------------------------
  
  #download the file
  if (!file.exists("Data/FMWT_TNSZooplanktonDataCPUEOct2017.xls") | ReDownloadData) {
    download.file("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSZooplanktonDataCPUEOct2017.xls", 
                  "Data/FMWT_TNSZooplanktonDataCPUEOct2017.xls", mode="wb")
  }
  
  # Import the FMWT data
  
  suppressWarnings(zoo_FMWT <- read_excel("Data/FMWT_TNSZooplanktonDataCPUEOct2017.xls", 
                                          sheet = "FMWT&TNS ZP CPUE", 
                                          col_types=c("text", rep("numeric", 3), "date", "text", "text", 
                                                      "text", "numeric", rep("text", 3), rep("numeric", 3), 
                                                      "text", rep("numeric", 5), "text", rep("numeric", 55))))
  
  # Tranform from "wide" to "long" format, add some variables, 
  # alter data to match other datasets
  
  data.list[["FMWT"]] <- zoo_FMWT%>%
    mutate(Datetime=suppressWarnings(parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
    gather(key="FMWT", value="CPUE", -Project, -Year, -Survey, -Month, -Date, -Datetime,
           -Station, -Index, -Time, -TowDuration, 
           -Region, -FLaSHRegionGroup, -TideCode, 
           -DepthBottom, -CondSurf, -PPTSurf, 
           -SurfSalinityGroup, -CondBott, -PPTBott, 
           -TempSurf, -Secchi, -Turbidity, -Microcystis, 
           -TotalMeter, -Volume)%>% #transform from wide to long
    select(Source=Project, Year, Date, Datetime, Station, Region, Tide=TideCode, BottomDepth=DepthBottom, CondSurf, CondBott, Temperature = TempSurf, Secchi, Turbidity, Microcystis, Volume, FMWT, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(FMWT, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, FMWTstart, FMWTend)%>% #only retain FMWT codes
                filter(!is.na(FMWT))%>% #Only retain Taxnames corresponding to FMWT codes
                distinct(),
              by = "FMWT")%>%
    filter(!is.na(Taxname))%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
           Microcystis=if_else(Microcystis=="6", "2", Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
           SampleID=paste(Source, Station, Datetime))%>% #Create identifier for each sample
    ungroup()%>%
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < FMWTstart ~ NA_real_,
      CPUE==0 & Date >= FMWTstart & Date < FMWTend ~ 0,
      CPUE==0 & Date >= FMWTend ~ NA_real_
    ))%>%
    filter(!is.na(CPUE))%>%
    select(-FMWT, -FMWTstart, -FMWTend, -Intro) #Remove FMWT taxa codes
  
  
  # twentymm ----------------------------------------------------------------
  
  #download the file
  if (!file.exists("Data/CDFW 20-mm Zooplankton Catch Matrix.xlsx") | ReDownloadData) {
    download.file("ftp://ftp.dfg.ca.gov/Delta%20Smelt/20mm%20Zooplankton%20Catch%20Matrix_1995-2017.xlsx", 
                  "Data/CDFW 20-mm Zooplankton Catch Matrix.xlsx", mode="wb")
  }
  
  # Import and modify 20mm data
  
  zoo_20mm<-read_excel("Data/CDFW 20-mm Zooplankton Catch Matrix.xlsx", 
                       sheet="20-mm CB CPUE Data", 
                       col_types = c("date", "numeric", "numeric", "date", rep("numeric", 80)))
  
  data.list[["twentymm"]]<-zoo_20mm%>%
    mutate(SampleID = paste(Station, SampleDate#, TowNum #ADD IN TOWNUM WHEN FLAT FILE IS FIXED
    ),
    Datetime=parse_date_time(paste0(SampleDate, " ", hour(TowTime), ":", minute(TowTime)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
    gather(key="twentymm", value="CPUE", -SampleDate, -Survey, -Station, -TowTime, -Temp, -TopEC, 
           -BottomEC, -Secchi, -Turbidity, -Tide, -BottomDepth, -Duration, -MeterCheck, -Volume, 
           -Dilution, -SampleID, -Datetime)%>% #transform from wide to long
    select(Date=SampleDate, Station, Temperature=Temp, CondSurf=TopEC, CondBott=BottomEC, Secchi, 
           Turbidity, Tide, BottomDepth, Volume, SampleID, Datetime, twentymm, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(twentymm, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, twentymmstart, twentymmend, twentymmstart2)%>% #only retain FMWT codes
                filter(!is.na(twentymm))%>% #Only retain Taxnames corresponding to FMWT codes
                distinct(),
              by = "twentymm")%>%
    filter(!is.na(Taxname))%>%
    mutate(Source="20mm",
           Station=as.character(Station),
           Tide=as.character(Tide),
           Taxlifestage=paste(Taxname, Lifestage))%>% #add variable for data source, create variable for combo taxonomy x life stage
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < twentymmstart ~ NA_real_,
      CPUE==0 & Date >= twentymmstart & Date < twentymmend ~ 0,
      CPUE==0 & Date >= twentymmend & Date < twentymmstart2 ~ NA_real_,
      CPUE==0 & Date >= twentymmstart2 ~ 0 #20mm dataset had one case of a taxa starting, ending, and starting again
    ))%>%
    select(-twentymmend, -twentymmstart, -twentymmstart2, -Intro)%>%
    lazy_dt()%>% #Speed up
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    as_tibble()
  
  
  # FRP ---------------------------------------------------------------------
  
  # Import the FRP data
  
  #download the file
  if (!file.exists("Data/zoopsFRP2018.csv") | ReDownloadData) {
    download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=d4c76f209a0653aa86bab1ff93ab9853",
                  "Data/zoopsFRP2018.csv", mode="wb")
  }
  
  zoo_FRP <- read_csv("Data/zoopsFRP2018.csv",
                      col_types = paste0("c","c", "t", "d", "d", "d", "d", "d", "d", "d", "d", 
                                         "c", "c", "c", 
                                         "d", "d","d","d",
                                         "d", "c"), na=c("", "NA"))
  
  #Already in long format
  data.list[["FRP"]] <- zoo_FRP%>%
    mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
    mutate(Station=replace(Station, Station=="Lindsey Tules", "Lindsey tules"),
           Station=replace(Station, Station=="LinBR", "LinBr"))%>% #Rename inconsistent station names to match
    mutate(Datetime=parse_date_time(paste0(Date, " ", hour(time), ":", minute(time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
    mutate(Source="FRP")%>% #add variable for data source
    select(Source, Date, Datetime, 
           Station, CondSurf = SC, Secchi, pH, DO, Turbidity, Tide, Microcystis,
           Temperature = Temp, Volume = volume, FRP = CommonName, CPUE, SampleID)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(FRP, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain FRP codes
                filter(!is.na(FRP))%>% #Only retain Taxnames corresponding to FRP codes
                distinct(),
              by = "FRP")%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
    select(-FRP)%>% #Remove FRP taxa codes
    lazy_dt()%>% #Speed up code
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    as_tibble()
  
  
  
  # YBFMP -------------------------------------------------------------
  
  
  #NO IDEA WHAT TO DO ABOUT INCONSISTENT TAXONOMIC RESOLUTION WITH NO DOCUMENTATION AND LACK OF LIFE STAGE INFORMATION
  
#   zoo_YBFMP<-read_csv("Data/yolo_zoop_public.csv", col_types = "ctddcccdddddddccccccccccccccccccdd")
  
#  data.list[["YBFMP"]]<-zoo_YBFMP%>%
#    mutate(SampleDate=parse_date_time(SampleDate, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
#    mutate(Datetime=parse_date_time(paste(SampleDate, SampleTime), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
#           Source="YBFMP",
#           YBFMP=paste(TaxonName, LifeStage),
#           SampleID=paste(SampleDate, StationCode))%>%
#    select(SampleID, Date=SampleDate, Station=StationCode, Temperature=WaterTemperature, Secchi, Turbidity, CondSurf=Conductivity, SpCnd, pH, DO, YBFMP, Source, Datetime, NetSize, CPUE)%>%
#    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
#                select(YBFMP, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain YBFMP codes
#                filter(!is.na(YBFMP))%>% #Only retain Taxnames corresponding to YBFMP codes
#                distinct(),
#              by = "YBFMP")%>%
#    mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
#    select(-YBFMP)%>% #Remove YBFMP taxa codes
#    lazy_dt()%>% #Speed up code
#    group_by_at(vars(-CPUE))%>% #In case some taxa names are repeated as in EMP so 
#    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
#    ungroup()%>%
#    mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
#    as_tibble()
  
  # Combine data ----------------------------------------
  
  zoop<-bind_rows(data.list)%>% # Combine data
    filter(!is.na(Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
    mutate(SalSurf=((0.36966/(((CondSurf*0.001)^(-1.07))-0.00074))*1.28156),
           SalBott=((0.36966/(((CondBott*0.001)^(-1.07))-0.00074))*1.28156),#Convert conductivity to salinity using formula in FMWT metadata
           Year=year(Date), #add variables for year and month
           Month=month(Date))%>%
    left_join(stations, by=c("Source", "Station"))%>% #Add lat and long
    select(-Region, -CondBott, -CondSurf)%>% #Remove some extraneous variables to save memory
    mutate(Tide=recode(Tide, "1"="High slack", "2"="Ebb", "3"="Low slack", "4"="Flood", "1=high slack"="High slack", "2=ebb"="Ebb", "3=low slack"="Low slack", "4=flood"="Flood")) #Rename tide codes to be consistent
  
  
  saveRDS(zoop, file=path)
  
}