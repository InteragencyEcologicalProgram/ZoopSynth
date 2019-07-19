Zoopdownloader <- function(path="zoopforzooper.Rdata"){
  
  # Setup -------------------------------------------------------------------
  require(tidyverse) 
  require(readxl)
  
  #Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
  require(dtplyr)
  
  require(data.table)
  require(lubridate)
  
  # Load crosswalk key to convert each dataset's taxonomic codes to a
  # unified set of "Taxname" and "Lifestage" values.
  
  crosswalk <- read_excel("new_crosswalk.xlsx", sheet = "Hierarchy2")%>%
    mutate_at(vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~parse_date(as.character(.), format="%Y"))%>%
    mutate_at(vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
    mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(FMWTend = if_else(is.finite(FMWTend), FMWTend+years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(twentymmend = if_else(is.finite(twentymmend), twentymmend+years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)
  
  # Load station key to later incorporate latitudes and longitudes
  
  stations <- read_excel("zoop_stations.xlsx", sheet="lat_long")%>%
    rename(Source=Project)
  
  # Initialize list of dataframes
  
  data.list<-list()
  
  
  
  # EMP ---------------------------------------------------------------------
  
    
    #download the file
    if (!file.exists("1972-2018CBMatrix.xlsx")) {
      download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx", 
                    "1972-2018CBMatrix.xlsx", mode="wb")
    }
    
    
    # Import the EMP data
    
    zoo_EMP <- read_excel("1972-2018CBMatrix.xlsx", 
                          sheet = "CB CPUE Matrix 1972-2018", 
                          col_types = c("numeric","numeric", "numeric", "numeric", "date", 
                                        "text", "text", "text", "numeric", "text", "text",
                                        "text", rep("numeric", 62)))
    
    # Tranform from "wide" to "long" format, add some variables, 
    # alter data to match other datasets
    
    data.list[["EMP"]] <- zoo_EMP%>%
      mutate(Datetime=suppressWarnings(parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M")))%>% #create a variable for datetime
      gather(key="EMP", value="CPUE", -SurveyCode, -Year, -Survey, -SurveyRep, 
             -Date, -Station, -EZStation, -DWRStation, 
             -Core, -Region, -Secchi, -`Chl-a`, -Temperature,
             -ECSurfacePreTow, -ECBottomPreTow, -CBVolume, -Datetime, -Time, -TideCode)%>% #transform from wide to long
      mutate(Source="EMP")%>% #add variable for data source
      select(Source, Year, Date, Datetime, Tide=TideCode, 
             Station, Region, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, 
             Temperature, Volume = CBVolume, EMP, CPUE)%>% #Select for columns in common and rename columns to match
      left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                  select(-FMWT, -twentymm, -FRP, -Level, -FMWTstart, -FMWTend, -twentymmstart, -twentymmend, -twentymmstart2)%>% #only retain EMP codes
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
    if (!file.exists("FMWT_TNSZooplanktonDataCPUEOct2017.xls")) {
      download.file("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSZooplanktonDataCPUEOct2017.xls", 
                    "FMWT_TNSZooplanktonDataCPUEOct2017.xls", mode="wb")
    }
    
    # Import the FMWT data
    
    suppressWarnings(zoo_FMWT <- read_excel("FMWT_TNSZooplanktonDataCPUEOct2017.xls", 
                                            sheet = "FMWT&TNS ZP CPUE", 
                                            col_types=c("text", rep("numeric", 3), "date", "text", "text", 
                                                        "text", "numeric", rep("text", 3), rep("numeric", 3), 
                                                        "text", rep("numeric", 5), "text", rep("numeric", 55))))
  
    # Tranform from "wide" to "long" format, add some variables, 
    # alter data to match other datasets
    
    data.list[["FMWT"]] <- zoo_FMWT%>%
      mutate(Datetime=suppressWarnings(parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M")))%>% #create a variable for datetime
      gather(key="FMWT", value="CPUE", -Project, -Year, -Survey, -Month, -Date, -Datetime,
             -Station, -Index, -Time, -TowDuration, 
             -Region, -FLaSHRegionGroup, -TideCode, 
             -DepthBottom, -CondSurf, -PPTSurf, 
             -SurfSalinityGroup, -CondBott, -PPTBott, 
             -TempSurf, -Secchi, -Turbidity, -Microcystis, 
             -TotalMeter, -Volume)%>% #transform from wide to long
      select(Source=Project, Year, Date, Datetime, Station, Region, Tide=TideCode, DepthBottom, CondSurf, CondBott, Temperature = TempSurf, Secchi, Turbidity, Microcystis, Volume, FMWT, CPUE)%>% #Select for columns in common and rename columns to match
      left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                  select(-EMP, -twentymm, -FRP, -Level, -EMPstart, -EMPend, -twentymmstart, -twentymmend, -twentymmstart2)%>% #only retain FMWT codes
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
  
  # Import and modify 20mm data
  
    suppressWarnings(zoopquery20mm <- read_excel("zoopquery20mm.xlsx", 
                                                 col_types = c("date", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "text")))
    zoopquery20mm <- zoopquery20mm%>%
      mutate(SampleID = paste(Station20mm, SampleDate, TowNum))
    
    #Rosie's code to calculate CPUE for 20mm, modified to dplyr format by Sam
    
    zoo_20mm<-zoopquery20mm%>%
      group_by(SampleID)%>% 
      summarise(totcells = max(CellNumber), totCountall = sum(ZooCount))%>%
      right_join(zoopquery20mm%>%
                   group_by(Station20mm, SampleDate, SampleID, Dilution, ZooCode, CommonName,
                            MeterStart, MeterEnd)%>% 
                   summarise(totCount = sum(ZooCount)), by="SampleID")%>%
      ungroup()%>%
      mutate(atotal = (totCount/totcells)*Dilution, #adjust for subsampling
             Volume = NA)%>%
      mutate(Volume=(MeterEnd-MeterStart)*0.026873027*0.0167)%>% #Volume sampled is the difference in flowmeter readings times the flowmeter constant times the mouth area
      mutate(Volume = ifelse(Volume<0, ((1000000 - MeterStart)+ MeterEnd)*0.026873027*0.0167, Volume))%>% #I've used the factory calibration here
      mutate(Volume=ifelse(is.na(Volume), mean(Volume, na.rm = T), Volume))%>% #some of the samples didn't have flowmeters, so we'll use the average Volume for those
      mutate(CPUE=atotal/Volume)%>% #Calculate CPUE (total critters divided by Volume sampled)
      rename(Station=Station20mm, Date=SampleDate)
    ###SMB Confirmed creates exact same dataframe as Rosie's code (before I added in TowNum as a grouping term)
    
    
    # merge 20mm CPUE with environmental data
    # CPUE for 20mm are in zoo20.R file that was created in zoo20mil_w20mil.R
    
    #merge environmental data for 20mm w/ CPUE data
    # import survey and station informationf from excel files (these excel files are from 20mm mdb - requires a lot of different
    # packages to import directly from mdb so just converted them to excel files for now)
    
    enviro20mm<-left_join(suppressWarnings(read_excel("20mm_Survey.xlsx")),
                          suppressWarnings(read_excel("20mm_Station.xlsx", col_types = c(rep("numeric", 14), "text"))),
                          by = "SurveyID",
                          suffix=c("_Survey", "_Station"))%>% #merge based on survey ID - unique for every date (date not in station data)
      rename(Date=SampleDate)
    
    # then merge with CPUE data
    data.list[["twentymm"]] <-zoo_20mm%>%
      left_join(enviro20mm, by = c("Station", "Date"))%>%
      rename(twentymm=CommonName)%>%
      left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                  select(-EMP, -FMWT, -FRP, -Level, -FMWTstart, -FMWTend, -EMPstart, -EMPend)%>% #only retain 20mm codes
                  filter(!is.na(twentymm))%>% #Only retain Taxnames corresponding to 20mm codes
                  distinct(),
                by="twentymm")%>%
      filter(!is.na(Taxname))%>%
      select(Date, Station, Volume, Temperature = Temp, CondSurf = TopEC, CondBott = BottomEC, Secchi, Turbidity, CPUE, Taxname, Lifestage, Phylum, Class, Order, Family, Genus, Species, SampleID, twentymmend, twentymmstart, twentymmstart2, Intro)%>% #Select for columns in common and rename columns to match
      mutate(Source="20mm")%>% #add variable for data source
      mutate(Station=as.character(Station))%>%
      mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
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
    if (!file.exists("zoopsFRP2018.csv")) {
      download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=d4c76f209a0653aa86bab1ff93ab9853",
                    "zoopsFRP2018.csv", mode="wb")
    }
  
    zoo_FRP <- read_csv("~/test.csv",
                        col_types = paste0("c","c", "t", "d", "d", "d", "d", "d", "d", "d", "d", 
                                           "c", "c", "c", 
                                           "d", "d","d","d",
                                           "d", "c"), na=c("", "NA"))
    
    #Already in long format
    data.list[["FRP"]] <- zoo_FRP%>%
      mutate(Date=parse_date_time(Date, "%m/%d/%Y"))%>%
      mutate(Station=replace(Station, Station=="Lindsey Tules", "Lindsey tules"),
             Station=replace(Station, Station=="LinBR", "LinBr"))%>% #Rename inconsistent station names to match
      mutate(Datetime=parse_date_time(paste0(Date, " ", hour(time), ":", minute(time)), "%Y-%m-%d %%H:%M"))%>% #Create a variable for datetime
      mutate(Source="FRP")%>% #add variable for data source
      select(Source, Date, Datetime, 
             Station, CondSurf = SC, Secchi, pH, DO, Turbidity, Tide, Microcystis,
             Temperature = Temp, Volume = volume, FRP = CommonName, CPUE, SampleID)%>% #Select for columns in common and rename columns to match
      left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                  select(-EMP, -twentymm, -FMWT, -Level, -FMWTstart, -FMWTend, -twentymmstart, -twentymmend, -twentymmstart2, -EMPstart, -EMPend, -Intro)%>% #only retain FRP codes
                  filter(!is.na(FRP))%>% #Only retain Taxnames corresponding to 20mm codes
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
    
  
  
  # Combine data ----------------------------------------
  
  zoop<-bind_rows(data.list)%>% # Combine data
    filter(!is.na(Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
    mutate(SalSurf=((0.36966/(((CondSurf*0.001)^(-1.07))-0.00074))*1.28156),
           SalBott=((0.36966/(((CondBott*0.001)^(-1.07))-0.00074))*1.28156),#Convert conductivity to salinity using formula in FMWT metadata
           Year=year(Date), #add variables for year and month
           Month=month(Date))%>%
    left_join(stations, by=c("Source", "Station")) #Add lat and long
  
    
    save(zoop, file=path)
  
}