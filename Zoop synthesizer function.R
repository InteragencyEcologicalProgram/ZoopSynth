Zooper<-function(Sources=c("EMP", "FRP", "FMWT", "TNS", "20mm"), Data="Community", Daterange=c(NA, NA), Months=NA, Years=NA, SalBottrange=NA, SalSurfrange=NA, Temprange=NA, Latrange=NA, Longrange=NA){
  
  
  # Documentation -----------------------------------------------------------
  
  # This function combines any combination of the zoo datasets (included as
  # parameters) and calculates least common denominator taxa to facilitate
  # comparisons across datasets with differing levels of taxonomic
  # resolution.
  
  # Option "Data" allows you to choose a final output dataset for either 
  # community (Data="Community"; the default) or Taxa-specific (Data="Taxa) 
  # analyses.
  
  # If you want all available data on given Taxa, use Data="Taxa"
  # If you want to conduct a community analysis, use Data="Community"
  
  # Briefly, Data="Community" optimizes for community-level analyses by
  # taking all taxa x life stage combinations that are not measured in 
  # every input dataset, and summing them up taxonomic levels to the lowest 
  # taxonomic level they belong to that is covered by all datasets. Remaining
  # Taxa x life stage combos that are not covered in all datasets up to the 
  # phylum level (usually something like Annelida or Nematoda or Insect Pupae)
  # are removed from the final dataset.
  
  # Data="Taxa" optimizes for the Taxa-level user by maintaining all data at
  # the original taxonomic level (but it outputs warnings for taxa not measured
  # in all datasets, which we call "orphans"). To facilitate comparions across 
  # datasets, this option also sums data into general categories that are 
  # comparable across all datasets and years: "summed groups." The new variable 
  # "Taxatype" identifies which taxa are summed groups (Taxatype="Summed group"),
  # which are measured to the species level (Taxatype = "Species"), and which 
  # are higher taxonomic groupings with the species designation unknown:
  # (Taxatype = "UnID species").
  
  # Includes options to filter by Date, Month, Year, Bottom salinity,
  # Surface salinity, Temperature, Latitude, or Longitude.
  
  # If you do not wish to filter by any variable, leave it as the default
  # value (NA or c(NA, NA)). 
  
  # To filter by Month or year, include a numeric vector of the months or
  # years you wish to include. 
  
  # To filter by salinity or temperature, include a vector of length 2 
  # specifying the minimum and maximum values you wish to include. To 
  # include all values above or below a limit, utilize Inf or -Inf for the
  # upper or lower bound respectively.
  
  # To filter within a range of dates, include a character vector of 2 
  # dates formatted in the yyyy-mm-dd format exactly, specifying the upper
  # and lower bounds. To specify an infinite upper or lower bound (to
  # include all values above or below a limit) utilize "NA"
  
  # To filter within a range of latitudes or longitudes, include a vector 
  # of length 2 specifying the minimum and maximum values you wish to 
  # include, in decimal degree format. Don't forget Longitudes should be 
  # negative
  
  
  # Setup -------------------------------------------------------------------
  
  ##NOTE THIS FUNCTION IS SUPER SLOW UNLESS YOU INSTALL THE DEVELOPER VERSION OF DPLYR DUE TO AN ISSUE WITH THE SUMMARISE FUNCTION
  #devtools::install_github("tidyverse/dplyr")
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
  
  
  if("EMP"%in%Sources){
    
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
  }
  
  
  # FMWT --------------------------------------------------------------------
  
  
  if("FMWT"%in%Sources | "TNS"%in%Sources){
    
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
    #Allow users to select TNS or FMWT data separately
    if(!("TNS"%in%Sources)){
      zoo_FMWT <- filter(zoo_FMWT, Project!="TNS")
    }
    
    if(!("FMWT"%in%Sources)){
      zoo_FMWT <- filter(zoo_FMWT, Project!="FMWT")
    }
      
    
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
  }
  
  
  # twentymm ----------------------------------------------------------------
  
  # Import and modify 20mm data
  
  if("20mm"%in%Sources){
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
  }
  
  
  # FRP ---------------------------------------------------------------------
  
  # Import the FRP data
  
  if("FRP"%in%Sources){
    zoo_FRP <- read_excel("zoopsFRP2018.xlsx",
                          col_types = c("text","date", "date", rep("numeric", 8), 
                                        "text", "text", "text", 
                                        "numeric", "numeric","numeric","numeric",
                                        "numeric", "text"), na=c("", "NA"))
    
    #Already in long format
    data.list[["FRP"]] <- zoo_FRP%>%
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
    
  }
  
  
  # Combine data ----------------------------------------
  
  zoop<-bind_rows(data.list)%>% # Combine data
    filter(!is.na(Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
    mutate(SalSurf=((0.36966/(((CondSurf*0.001)^(-1.07))-0.00074))*1.28156),
           SalBott=((0.36966/(((CondBott*0.001)^(-1.07))-0.00074))*1.28156),#Convert conductivity to salinity using formula in FMWT metadata
           Year=year(Date), #add variables for year and month
           Month=month(Date))%>%
    left_join(stations, by=c("Source", "Station")) #Add lat and long
  
  
  # Filter data -------------------------------------------------------------
  
  #Filter data by specified variables if users provide appropriate ranges
  
  if(!every(SalBottrange, is.na)) {
    if(some(SalBottrange, is.na)) {
      stop("One element of SalBottrange cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoop<-filter(zoop, between(SalBott, min(SalBottrange), max(SalBottrange)))
  }
  
  if(!every(SalSurfrange, is.na)) {
    if(some(SalSurfrange, is.na)) {
      stop("One element of SalSurfrange cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoop<-filter(zoop, between(SalSurf, min(SalSurfrange), max(SalSurfrange)))
  }
  
  if(!every(Temprange, is.na)) {
    if(some(Temprange, is.na)) {
      stop("One element of Temprange cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoop<-filter(zoop, between(Temperature, min(Temprange), max(Temprange)))
  }
  
  if(!is.na(Daterange[1])){
    Datemin<-as_date(Daterange[1])
    if(is.na(Datemin)) {stop("Daterange[1] in incorrect format. Reformat to `yyyy-mm-dd`")}
    zoop<-filter(zoop, Date>Datemin)
  }
  
  if(!is.na(Daterange[2])){
    Datemax<-as_date(Daterange[2])
    if(is.na(Datemax)) {stop("Daterange[2] in incorrect format. Reformat to `yyyy-mm-dd`")}
    zoop<-filter(zoop, Date<Datemax)
  }
  
  if(!every(Months, is.na)) {
    zoop<-filter(zoop, Month%in%Months)
  }
  
  if(!every(Years, is.na)) {
    zoop<-filter(zoop, Year%in%Years)
  }
  
  if(!every(Latrange, is.na)) {
    if(some(Latrange, is.na)) {
      stop("One element of Latrange cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoop<-filter(zoop, between(Latitude, min(Latrange), max(Latrange)))
  }
  
  if(!every(Longrange, is.na)) {
    if(some(Longrange, is.na)) {
      stop("One element of Longrange cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    if(some(Longrange<0)) {warning("Longitudes should be negative for the Delta")}    
    zoop<-filter(zoop, between(Longitude, min(Longrange), max(Longrange)))
  }
  
  
  # Apply LCD approach ------------------------------------------------------
  
  
  # Make list of taxa x life stage combos present in all original datasets
  
  #Function to detect common taxonomic names across all source datasets
  Commontaxer<-function(Taxagroup){
    Taxagroup<-sym(Taxagroup) #unquote input
    Taxagroup<-enquo(Taxagroup) #capture expression to pass on to functions below
    zoop%>%
      filter(!is.na(!!Taxagroup))%>%
      select(!!Taxagroup, Lifestage, Source)%>%
      distinct()%>%
      group_by(!!Taxagroup, Lifestage)%>%
      summarise(n=n())%>% #Create index of number of data sources in which each taxagroup x lifestage combo appears
      ungroup()%>%
      filter(n==length(unique(zoop$Source)))%>% #only retain taxagroup x lifestage combos that appear in all datasets
      select(!!Taxagroup, Lifestage)
  }
  
  #Find all common Taxname x life stage combinations, turn into vector of Taxlifestages
  Commontax<-Commontaxer("Taxname")
  Commontax<-paste(Commontax$Taxname, Commontax$Lifestage)

  # Make list of taxalifestages that do not appear in all datasets
  
  Lumped<-setdiff(unique(zoop$Taxlifestage), Commontax)
  
  
  #Make vector of taxonomic categories that we will use later
  Taxcats<-c("Genus", "Family", "Order", "Class", "Phylum")

  

# Apply LCD approach for taxa-level data user ----------------------

  
  
  if(Data=="Taxa"){
    
    #Make vector of _g Taxonomic variables
    Taxcats_g<-paste0(Taxcats, "_g")
    
    # Define function to calculate least common denominator for each taxonomic level
    LCD<-function(df, Taxagroup){
      Taxagroup2<-sym(Taxagroup) #unquote input
      Taxagroup2<-enquo(Taxagroup2) #capture expression to pass on to functions below
      out<-df%>%
        filter(!is.na(!!Taxagroup2))%>% #filter to include only data belonging to the taxonomic grouping
        lazy_dt()%>%
        group_by_at(vars(-c("Taxname", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxlifestage", "CPUE",Taxcats_g[!Taxcats_g%in%Taxagroup])))%>% #Group data by relavent grouping variables (including taxonomic group) for later data summation
        summarise(CPUE=sum(CPUE, na.rm=T))%>% #Add up all members of each grouping taxon
        ungroup()%>%
        as_tibble()%>%
        mutate(Taxname=!!Taxagroup2, #Add summarized group names to Taxname
               Taxatype="Summed group")%>% #Add a label to these summed groups so they can be removed later if users wish
        mutate(Taxname=paste0(Taxname, "_all")) #Differentiate grouped Taxnames from others
      return(out)
    }
    
    # Select higher level groupings that correspond to Taxnames, i.e., are a 
    # classification category in one of the original datasets, and rename 
    # them as "Taxonomiclevel_g"
    
  zoop<-zoop%>%
    mutate_at(c("Genus", "Family", "Order", "Class", "Phylum"), list(g=~ifelse(.%in%unique(Taxname), ., NA)))
  
  #Extract vector of grouping taxa (i.e. all unique taxa retained in the above step)
  
  Groups<-zoop%>%
    select(Genus_g, Family_g, Order_g, Class_g, Phylum_g)%>%
    gather("Level", "Species")%>%
    filter(!is.na(Species))%>%
    pull(Species)%>%
    unique()
  
  # Output list of taxa that were not measured in all datasets, and are 
  # not higher taxa that can be calculated by summing lower taxa, i.e. 
  # "orphan taxa"
  
  Orphans<-paste(Lumped[-str_which(paste0("[", paste(Groups, collapse="|"), "]"), word(Lumped, 1, -2))], collapse=", ")
  print(paste("NOTE: These species are not counted in all datasets:", Orphans), quote=F)
  
  # Calculate summed groups and create a final dataset
  
  zoop<-map_dfr(Taxcats_g, .f=LCD, df=zoop)%>% #Taxonomic level by level, summarise for each of these grouping categories and bind them all together
    bind_rows(zoop%>% #Bind these summarized groupings to the original taxonomic categories in the original dataset
                mutate(Taxatype=ifelse(Taxname%in%Groups, "UnID species", "Species")))%>% 
    ungroup()%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage), #add back in the Taxlifestage variable (removed by the LCD function)
           Orphan=ifelse(Taxlifestage%in%Orphans, T, F), #add an identifier for orphan taxa (species not counted in all data sources)
           Taxname = ifelse(Taxatype=="UnID species", paste0(Taxname, "_UnID"), Taxname),
           Taxlifestage=paste(Taxname, Lifestage))%>%
    select_at(vars(-Taxcats_g))
  
  print("NOTE: Do not use this data to make additional higher-level taxonomic summaries or any other operations to add together taxa above the species level unless you first filter out all rows with Taxatype==`Summed group` and, depending on your purpose, Orphan==TRUE. Do not compare UnID categories across data sources.", quote=F)
  }
  

# Apply LCD approach for community data user ------------------------------

  
  if(Data=="Community"){
    
    #Create taxonomy table for all taxonomic levels present in all datasets
    Commontaxkey<-map_dfr(Taxcats, Commontaxer)%>%
      mutate_at(c("Genus", "Family", "Order", "Class", "Phylum"), list(lifestage=~ifelse(is.na(.), NA, paste(., Lifestage))))%>% #Create taxa x life stage variable for each taxonomic level
      select(Genus_lifestage, Family_lifestage, Order_lifestage, Class_lifestage, Phylum_lifestage) #only retain columns we need
    
    #Create taxonomy table for taxa not present in all datasets, then select their new names corresponding to taxa x life stage combinations that are measured in all datasets
    Lumpedkey<-tibble(Taxlifestage=Lumped)%>%
      left_join(crosswalk%>%
                  select(Taxname, Lifestage, Phylum, Class, Order, Family, Genus)%>%
                  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
                  distinct(),
                by="Taxlifestage")%>%
      mutate_at(c("Genus", "Family", "Order", "Class", "Phylum"), list(lifestage=~ifelse(is.na(.), NA, paste(., Lifestage))))%>% #Create taxa x life stage variable for each taxonomic level
      mutate(Taxname_new=case_when( #This will go level by leve, look for matches with the Commontaxkey, and assign the taxonomic level that matches. The "TRUE" at end specifies what to do if no conditions are met. 
        !is.na(Genus_lifestage) & Genus_lifestage%in%Commontaxkey$Genus_lifestage ~ Genus,
        !is.na(Family_lifestage) & Family_lifestage%in%Commontaxkey$Family_lifestage ~ Family,
        !is.na(Order_lifestage) & Order_lifestage%in%Commontaxkey$Order_lifestage ~ Order,
        !is.na(Class_lifestage) & Class_lifestage%in%Commontaxkey$Class_lifestage ~ Class,
        !is.na(Phylum_lifestage) & Phylum_lifestage%in%Commontaxkey$Phylum_lifestage ~ Phylum,
        TRUE ~ "REMOVE" #If no match is found, change to the Taxname REMOVE so we know to later remove these data from the final database
      ))
    
    
    #Create a names vector to expedite renaming Taxnames in the next step
    LCDnames<-setNames(Lumpedkey$Taxname_new, Lumpedkey$Taxlifestage) 
    
    #
    zoop<-zoop%>%
      mutate(Taxname=recode(Taxlifestage, !!!LCDnames, .default=Taxname))%>%
      filter(Taxname!="REMOVE")%>%
      mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
      select(-Phylum, -Class, -Order, -Family, -Genus, -Species)%>%
      lazy_dt()%>%
      group_by_at(vars(-CPUE))%>%
      summarise(CPUE=sum(CPUE, na.rm=T))%>%
      as_tibble()%>%
      left_join(crosswalk%>%
                  select(Taxname, Lifestage, Phylum, Class, Order, Family, Genus, Species)%>%
                  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
                  select(-Taxname, -Lifestage)%>%
                  bind_rows(Lumpedkey%>%
                              select(Taxlifestage, Phylum, Class, Order, Family, Genus))%>%
                  distinct(),
                by="Taxlifestage"
                  )
  }
  
  
  return(zoop)
}

# Ideas for testing -------------------------------------------------------

# 1)  Make sure rows aren't repeated by looking for repeats of 
#   paste(SurveyID, Taxlifestage)



# Issues ------------------------------------------------------------------



# TODO --------------------------------------------------------------------

# 1)  Test case_when statements that are assigning appropriate NAs and 0s 
#   depending on intro, start, and end years to make sure they are working 
#   correctly

# 2)  Do we need to make some of these case_when statements for FRP data or 
#   are 0s and NAs already assigned appropriately?

# 3)  If (or when) possible, add download link for FRP and 20mm data

# 4)  Try speeding up code more. Bottleneck now is probably read_excel()

# 5)  Do we want to append something like "_UnID" to the taxnames of Lumped 
# (LCDed) taxa under option Data=="Community"???

# 6)  How do we apply the LCD approach for community data to issues arrising
#   from changing taxonomic resolution over time in individual datasets
