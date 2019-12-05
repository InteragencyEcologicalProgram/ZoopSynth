#' Downloads and combines zooplankton datasets from the Sacramento San Joaquin Delta
#' 
#' This function downloads datasets from the internet, converts them to a consistent format, binds them together, and exports the combined dataset as .Rds R data files.
#' @param Data_folder Path to folder in which source datasets are stored, and to which you would like datasets to be downloaded if you set `Redownload_data = TRUE`.
#' @param Save_object Should the combined data be saved to disk? Defaults to `Save_object = TRUE`.
#' @param Return_object Should data be returned as an R object? If `TRUE`, the function will return the full combined dataset. Defaults to `Return_object = FALSE`.
#' @param Redownload_data Should source datasets be redownloaded from the internet? Defaults to `Redownload_data = FALSE`.
#' @param Zoop_path File path specifying the folder and filename of the zooplankton dataset. Defaults to `Zoop_path = file.path(Data_folder, "zoopforzooper")`.
#' @param Env_path File path specifying the folder and filename of the dataset with accessory environmental parameters. Defaults to `Env_path = file.path(Data_folder, "zoopenvforzooper")`.
#' @keywords download, integration, synthesis, zooplankton
#' @import tidyverse, readxl, dtplyr, lubridate
#' @return If `Return_object = TRUE`, returns the combined dataset as a tibble. If `Save_object = TRUE`, writes 2 .Rds files to disk: one with the zooplankton catch data and another with accessary environmental parameters. 
#' @author Sam Bashevkin
#' @example 
#' Data <- Zoopdownloader(Return_object = TRUE, Save_object = FALSE)
#' 

Zoopdownloader <- function(
  Data_folder = "Data",
  Save_object = TRUE,
  Return_object = FALSE,
  Redownload_data=FALSE,
  Zoop_path = file.path(Data_folder, "zoopforzooper"), 
  Env_path = file.path(Data_folder, "zoopenvforzooper")){
  
  # Setup -------------------------------------------------------------------
  require(tidyverse) 
  require(readxl)
  require(dtplyr)
  require(lubridate)
  
  # Load crosswalk key to convert each dataset's taxonomic codes to a
  # unified set of "Taxname" and "Lifestage" values.
  
  crosswalk <- read_excel(file.path(Data_folder, "new_crosswalk.xlsx"), sheet = "Hierarchy2")%>%
    mutate_at(vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~parse_date(as.character(.), format="%Y"))%>%
    mutate_at(vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
    mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(FMWTend = if_else(is.finite(FMWTend), FMWTend+years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(twentymmend = if_else(is.finite(twentymmend), twentymmend+years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)
  
  # Load station key to later incorporate latitudes and longitudes
  
  stations <- read_excel(file.path(Data_folder, "zoop_stations.xlsx"), sheet="lat_long")%>%
    rename(Source=Project)
  
  # Initialize list of dataframes
  
  data.list<-list()
  
  
  
  # EMP Meso ---------------------------------------------------------------------
  
  
  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018CBMatrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx", 
                  file.path(Data_folder, "1972-2018CBMatrix.xlsx"), mode="wb")
  }
  
  
  # Import the EMP data
  
  zoo_EMP_Meso <- read_excel(file.path(Data_folder, "1972-2018CBMatrix.xlsx"), 
                             sheet = "CB CPUE Matrix 1972-2018", 
                             col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                           "text", "text", "text", "numeric", "text", "text",
                                           "text", rep("numeric", 62)))
  
  # Tranform from "wide" to "long" format, add some variables, 
  # alter data to match other datasets
  
  data.list[["EMP_Meso"]] <- zoo_EMP_Meso%>%
    mutate(Datetime=suppressWarnings(parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
    gather(key="EMP_Meso", value="CPUE", -SurveyCode, -Year, -Survey, -SurveyRep, 
           -Date, -Station, -EZStation, -DWRStation, 
           -Core, -Region, -Secchi, -`Chl-a`, -Temperature,
           -ECSurfacePreTow, -ECBottomPreTow, -CBVolume, -Datetime, -Time, -TideCode)%>% #transform from wide to long
    mutate(Source="EMP",
           SizeClass="Meso")%>% #add variable for data source
    select(Source, Year, Date, Datetime, Tide=TideCode, 
           Station, Region, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, SizeClass,
           Temperature, Volume = CBVolume, EMP_Meso, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(EMP_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                filter(!is.na(EMP_Meso))%>% #Only retain Taxnames corresponding to EMP codes
                distinct(),
              by="EMP_Meso")%>%
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
    select(-EMP_Meso, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes 
    select(-Datetime)%>% #Add this back in when other EMP data have time
    lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    group_by_at(vars(-CPUE))%>%
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    ungroup()%>%
    as_tibble() #required to finish operation after lazy_dt()
  
  
  # FMWT Meso --------------------------------------------------------------------
  
  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls")) | Redownload_data) {
    download.file("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSZooplanktonDataCPUEOct2017.xls", 
                  file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls"), mode="wb")
  }
  
  # Import the FMWT data
  
  suppressWarnings(zoo_FMWT_Meso <- read_excel(file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls"), 
                                               sheet = "FMWT&TNS ZP CPUE", 
                                               col_types=c("text", rep("numeric", 3), "date", "text", "text", 
                                                           "text", "numeric", rep("text", 3), rep("numeric", 3), 
                                                           "text", rep("numeric", 5), "text", rep("numeric", 55))))
  
  # Tranform from "wide" to "long" format, add some variables, 
  # alter data to match other datasets
  
  data.list[["FMWT_Meso"]] <- zoo_FMWT_Meso%>%
    mutate(Datetime=suppressWarnings(parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
    gather(key="FMWT_Meso", value="CPUE", -Project, -Year, -Survey, -Month, -Date, -Datetime,
           -Station, -Index, -Time, -TowDuration, 
           -Region, -FLaSHRegionGroup, -TideCode, 
           -DepthBottom, -CondSurf, -PPTSurf, 
           -SurfSalinityGroup, -CondBott, -PPTBott, 
           -TempSurf, -Secchi, -Turbidity, -Microcystis, 
           -TotalMeter, -Volume)%>% #transform from wide to long
    select(Source=Project, Year, Date, Datetime, Station, Region, Tide=TideCode, BottomDepth=DepthBottom, CondSurf, CondBott, Temperature = TempSurf, Secchi, Turbidity, Microcystis, Volume, FMWT_Meso, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(FMWT_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, FMWTstart, FMWTend)%>% #only retain FMWT codes
                filter(!is.na(FMWT_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                distinct(),
              by = "FMWT_Meso")%>%
    filter(!is.na(Taxname))%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
           Microcystis=if_else(Microcystis=="6", "2", Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
           SampleID=paste(Source, Station, Datetime),
           SizeClass="Meso")%>% #Create identifier for each sample
    ungroup()%>%
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < FMWTstart ~ NA_real_,
      CPUE==0 & Date >= FMWTstart & Date < FMWTend ~ 0,
      CPUE==0 & Date >= FMWTend ~ NA_real_
    ))%>%
    filter(!is.na(CPUE))%>%
    select(-FMWT_Meso, -FMWTstart, -FMWTend, -Intro) #Remove FMWT taxa codes
  
  
  # twentymm Meso ----------------------------------------------------------------
  
  #download the file
  if (!file.exists(file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/Delta%20Smelt/20mm%20Zooplankton%20Catch%20Matrix_1995-2017.xlsx", 
                  file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx"), mode="wb")
  }
  
  # Import and modify 20mm data
  
  zoo_20mm_Meso<-read_excel(file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx"), 
                            sheet="20-mm CB CPUE Data", 
                            col_types = c("date", rep("numeric", 3), "date", rep("numeric", 80)))
  
  data.list[["twentymm_Meso"]]<-zoo_20mm_Meso%>%
    mutate(SampleID = paste(Station, SampleDate, TowNum),
    Datetime=parse_date_time(paste0(SampleDate, " ", hour(TowTime), ":", minute(TowTime)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
    gather(key="twentymm_Meso", value="CPUE", -SampleDate, -Survey, -Station, -TowTime, -Temp, -TopEC, 
           -BottomEC, -Secchi, -Turbidity, -Tide, -BottomDepth, -Duration, -MeterCheck, -Volume, 
           -Dilution, -SampleID, -Datetime)%>% #transform from wide to long
    select(Date=SampleDate, Station, Temperature=Temp, CondSurf=TopEC, CondBott=BottomEC, Secchi, 
           Turbidity, Tide, BottomDepth, Volume, SampleID, Datetime, twentymm_Meso, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(twentymm_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, twentymmstart, twentymmend, twentymmstart2)%>% #only retain FMWT codes
                filter(!is.na(twentymm_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                distinct(),
              by = "twentymm_Meso")%>%
    filter(!is.na(Taxname))%>%
    mutate(Source="twentymm",
           SizeClass="Meso",
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
    select(-twentymmend, -twentymmstart, -twentymmstart2, -Intro, -twentymm_Meso)%>%
    lazy_dt()%>% #Speed up
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    as_tibble()
  
  
  # FRP Meso ---------------------------------------------------------------------
  
  # Import the FRP data
  
  #download the file
  if (!file.exists(file.path(Data_folder, "zoopsFRP2018.csv")) | Redownload_data) {
    download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=d4c76f209a0653aa86bab1ff93ab9853",
                  file.path(Data_folder, "zoopsFRP2018.csv"), mode="wb")
  }
  
  zoo_FRP_Meso <- read_csv(file.path(Data_folder, "zoopsFRP2018.csv"),
                           col_types = "cctddddddddcccdddddc", na=c("", "NA"))
  
  #Already in long format
  data.list[["FRP_Meso"]] <- zoo_FRP_Meso%>%
    mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
    mutate(Station=recode(Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr"))%>% #Rename inconsistent station names to match
    mutate(Datetime=parse_date_time(paste0(Date, " ", hour(time), ":", minute(time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
    mutate(Source="FRP",
           SizeClass="Meso")%>% #add variable for data source
    select(Source, Date, Datetime, 
           Station, CondSurf = SC, Secchi, pH, DO, Turbidity, Tide, Microcystis, SizeClass,
           Temperature = Temp, Volume = volume, FRP_Meso = CommonName, CPUE, SampleID)%>% #Select for columns in common and rename columns to match
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    spread(key=FRP_Meso, value=CPUE, fill=0)%>%
    gather(-Source, -Date, -Datetime, 
           -Station, -CondSurf, -Secchi, -pH, -DO, -Turbidity, -Tide, -Microcystis, -SizeClass,
           -Temperature, -Volume, -SampleID, key="FRP_Meso", value="CPUE")%>%
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(FRP_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain FRP codes
                filter(!is.na(FRP_Meso))%>% #Only retain Taxnames corresponding to FRP codes
                distinct(),
              by = "FRP_Meso")%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
    select(-FRP_Meso)%>% #Remove FRP taxa codes
    lazy_dt()%>% #Speed up code
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    as_tibble()
  
  
  
  # YBFMP Meso/Micro -------------------------------------------------------------
  
  
  #NO IDEA WHAT TO DO ABOUT INCONSISTENT TAXONOMIC RESOLUTION WITH NO DOCUMENTATION AND LACK OF LIFE STAGE INFORMATION
  
  #   zoo_YBFMP<-read_csv(file.path(Data_folder, "yolo_zoop_public.csv"), col_types = "ctddcccdddddddccccccccccccccccccdd")
  
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
  
  
  # EMP Micro ---------------------------------------------------------------
  
  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018PumpMatrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2018Pump%20Matrix.xlsx", 
                  file.path(Data_folder, "1972-2018PumpMatrix.xlsx"), mode="wb")
  }
  
  
  # Import the EMP data
  
  zoo_EMP_Micro <- read_excel(file.path(Data_folder, "1972-2018PumpMatrix.xlsx"), 
                              sheet = " Pump CPUE Matrix 1972-2018", 
                              col_types = c(rep("numeric", 4), "date", rep("text", 3), "numeric", "text", rep("numeric", 36)))
  
  # Tranform from "wide" to "long" format, add some variables, 
  # alter data to match other datasets
  
  data.list[["EMP_Micro"]] <- zoo_EMP_Micro%>%
    select(-Year, -SurveyCode, -Survey, -SurveyRep, -EZStation, -DWRStationNo, -Core, -Region)%>%
    rename(OTHCYCADPUMP=OTHCYCAD)%>%
    gather(key="EMP_Micro", value="CPUE", -SampleDate, -Station, -Secchi, -`Chl-a`, -Temperature,
           -ECSurfacePreTow, -ECBottomPreTow, -PumpVolume)%>% #transform from wide to long
    mutate(Source="EMP",
           SizeClass="Micro")%>% #add variable for data source
    select(Source, Date=SampleDate, Station, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, 
           Temperature, SizeClass, Volume = PumpVolume, EMP_Micro, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(EMP_Micro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                filter(!is.na(EMP_Micro))%>% #Only retain Taxnames corresponding to EMP codes
                distinct(),
              by="EMP_Micro")%>%
    filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
    mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
           SampleID=paste(Source, Station, Date),
           Tide="1")%>% #Create identifier for each sample
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
      CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
      CPUE==0 & Date >= EMPend ~ NA_real_
    ))%>% 
    select(-EMP_Micro, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes 
    lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    group_by_at(vars(-CPUE))%>%
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    ungroup()%>%
    as_tibble() #required to finish operation after lazy_dt()
  
  
  # FRP Macro ---------------------------------------------------------------
  
  # Import the FRP data
  
  #download the file
  if (!file.exists(file.path(Data_folder, "bugsFRP2018.csv")) | Redownload_data) {
    download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=630f16b33a9cbf75f1989fc18690a6b3",
                  file.path(Data_folder, "bugsFRP2018.csv"), mode="wb")
  }
  
  zoo_FRP_Macro <- read_csv(file.path(Data_folder, "bugsFRP2018.csv"),
                            col_types = "cctcddddddddccdddcddc", na=c("", "NA"))
  
  #Already in long format
  data.list[["FRP_Macro"]] <- zoo_FRP_Macro%>%
    filter(Sampletype=="trawl")%>%
    mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
    mutate(Station=recode(Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr", MINSLO1="MinSlo1", ProBR="ProBr", WinBR="WinBr"))%>% #Rename inconsistent station names to match
    mutate(Datetime=parse_date_time(paste0(Date, " ", hour(time), ":", minute(time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
    mutate(Source="FRP",
           SizeClass="Macro",
           CPUE=AdjCount/volume)%>% #add variable for data source
    select(Source, Date, Datetime, 
           Station, CondSurf = SC, Secchi, pH, DO, Turbidity, Tide, Microcystis, SizeClass,
           Temperature = Temp, Volume = volume, FRP_Macro = CommonName, CPUE, SampleID)%>% #Select for columns in common and rename columns to match
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    spread(key=FRP_Macro, value=CPUE, fill=0)%>%
    gather(-Source, -Date, -Datetime, 
           -Station, -CondSurf, -Secchi, -pH, -DO, -Turbidity, -Tide, -Microcystis, -SizeClass,
           -Temperature, -Volume, -SampleID, key="FRP_Macro", value="CPUE")%>%
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(FRP_Macro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain FRP codes
                filter(!is.na(FRP_Macro))%>% #Only retain Taxnames corresponding to FRP codes
                distinct(),
              by = "FRP_Macro")%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
    select(-FRP_Macro)%>% #Remove FRP taxa codes
    lazy_dt()%>% #Speed up code
    group_by_at(vars(-CPUE))%>% #Some taxa names are repeated as in EMP so 
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    ungroup()%>%
    mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    as_tibble()
  
  
  # EMP Macro ---------------------------------------------------------------
  
  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018MysidMatrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2018MysidMatrix.xlsx", 
                  file.path(Data_folder, "1972-2018MysidMatrix.xlsx"), mode="wb")
  }
  
  
  # Import the EMP data
  
  zoo_EMP_Macro <- read_excel(file.path(Data_folder, "1972-2018MysidMatrix.xlsx"), 
                              sheet = "Mysid CPUE Matrix 1972-2018 ", 
                              col_types = c(rep("numeric", 4), "date", rep("text", 3), "numeric", "text", rep("numeric", 14)))
  
  # Tranform from "wide" to "long" format, add some variables, 
  # alter data to match other datasets
  
  data.list[["EMP_Macro"]] <- zoo_EMP_Macro%>%
    select(-Year, -SurveyCode, -Survey, -SurveyRep, -EZStation, -DWRStation, -Core, -Region)%>%
    gather(key="EMP_Macro", value="CPUE", -SampleDate, -Station, -Secchi, -`Chl-a`, -Temperature,
           -ECSurfacePreTow, -ECBottomPreTow, -MysidVolume)%>% #transform from wide to long
    mutate(Source="EMP",
           SizeClass="Macro")%>% #add variable for data source
    select(Source, Date=SampleDate, Station, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, SizeClass,
           Temperature, Volume = MysidVolume, EMP_Macro, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(EMP_Macro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                filter(!is.na(EMP_Macro))%>% #Only retain Taxnames corresponding to EMP codes
                distinct(),
              by="EMP_Macro")%>%
    filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
    mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
           SampleID=paste(Source, Station, Date), #Create identifier for each sample
           Tide="1")%>% 
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
      CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
      CPUE==0 & Date >= EMPend ~ NA_real_
    ))%>% 
    select(-EMP_Macro, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes 
    lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    group_by_at(vars(-CPUE))%>%
    summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    ungroup()%>%
    as_tibble() #required to finish operation after lazy_dt()
  
  
  # FMWT Macro --------------------------------------------------------------
  
  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSMysidCPUEJuly2019.xlsx", 
                  file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx"), mode="wb")
  }
  
  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSAmphipodCPUEJuly2019.xls", 
                  file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls"), mode="wb")
  }
  
  zoo_FMWT_Macro_Mysid <- read_excel(file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx"), 
                                               sheet = "FMWT STN Mysid CPUE Matrix")
  
  zoo_FMWT_Macro_Amph <- read_excel(file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls"), 
                                               sheet = "FMWT STN amphipod CPUE")
  
  data.list[["FMWT_Macro"]] <- zoo_FMWT_Macro_Mysid%>%
    rename(Date=SampleDate, `PPT Surface`=PPTSurf)%>%
    mutate(Tax="Mysid")%>%
    bind_rows(zoo_FMWT_Macro_Amph%>%
                mutate(Tax="Amph"))%>%
    mutate(Datetime=parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
           Microcystis=as.character(Microcystis))%>% #create a variable for datetime
    gather(key="FMWT_Macro", value="CPUE", -Project, -Year, -Survey, -Date, -Datetime,
           -Station, -Index, -SMSCG, -Time, -TowDuration, 
           -Region, -FLaSHRegionGroup, -TideCode, 
           -DepthBottom, -ConductivityTop, -`PPT Surface`, 
           -ConductivityBottom, -`PPT Bottom`, 
           -WaterTemperature, -Secchi, -Turbidity, -Microcystis, 
           -TotalMeter, -Volume, -Tax)%>% #transform from wide to long
    select(Source=Project, Date, Datetime, Station, SMSCG, Tide=TideCode, BottomDepth=DepthBottom, CondSurf=ConductivityTop, CondBott=ConductivityBottom, Temperature = WaterTemperature, Secchi, Turbidity, Microcystis, Volume, FMWT_Macro, CPUE)%>% #Select for columns in common and rename columns to match
    left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                select(FMWT_Macro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, FMWTstart, FMWTend)%>% #only retain FMWT codes
                filter(!is.na(FMWT_Macro))%>% #Only retain Taxnames corresponding to FMWT codes
                distinct(),
              by = "FMWT_Macro")%>%
    filter(!is.na(Taxname))%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
           Microcystis=if_else(Microcystis=="6", "2", Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
           SampleID=paste(Source, Station, SMSCG, Datetime), #Create identifier for each sample
           SizeClass="Macro",
           Tide=as.character(Tide))%>%
    mutate(CPUE=case_when(
      CPUE!=0 ~ CPUE, 
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < FMWTstart ~ NA_real_,
      CPUE==0 & Date >= FMWTstart & Date < FMWTend ~ 0,
      CPUE==0 & Date >= FMWTend ~ NA_real_
    ))%>%
    filter(!is.na(CPUE))%>%
    select(-FMWT_Macro, -FMWTstart, -FMWTend, -Intro, -SMSCG)%>% #Remove FMWT taxa codes
    mutate(Source=recode(Source, STN="TNS"))
  
  # Combine data ----------------------------------------
  
  zoop<-bind_rows(data.list)%>% # Combine data
    filter(!is.na(Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
    mutate(SalSurf=((0.36966/(((CondSurf*0.001)^(-1.07))-0.00074))*1.28156),
           SalBott=((0.36966/(((CondBott*0.001)^(-1.07))-0.00074))*1.28156),#Convert conductivity to salinity using formula in FMWT metadata
           Year=year(Date))%>%
    left_join(stations, by=c("Source", "Station"))%>% #Add lat and long
    select(-Region, -CondBott, -CondSurf)%>% #Remove some extraneous variables to save memory
    mutate(Tide=recode(Tide, "1"="High slack", "2"="Ebb", "3"="Low slack", "4"="Flood", "1=high slack"="High slack", "2=ebb"="Ebb", "3=low slack"="Low slack", "4=flood"="Flood"))#Rename tide codes to be consistent
  
  zoopEnv<-zoop%>%
    select(Source, Year, Date, Datetime, Tide, Station, Chl, Secchi, Temperature, BottomDepth, Turbidity, Microcystis, pH, DO, SalSurf, SalBott, Latitude, Longitude, SampleID)%>%
    distinct()
  
  zoop<-zoop%>%
    select(-Year, -Date, -Datetime, -Tide, -Station, -Chl, -Secchi, -Temperature, -BottomDepth, -Turbidity, -Microcystis, -pH, -DO, -SalSurf, -SalBott, -Latitude, -Longitude)
  
  saveRDS(zoop, file=paste0(Zoop_path, ".Rds"))
  
  saveRDS(zoopEnv, file=paste0(Env_path, ".Rds"))
  
  if(Return_object){
    zoop_full <- left_join(zoop, select(zoopEnv, -Source), by="SampleID")
    return(zoop_full)
  }
  
}