Zooper<-function(Sources=c("EMP", "FRP", "FMWT", "TNS", "20mm"), Data="Community", Daterange=c(NA, NA), Months=NA, Years=NA, SalBottrange=NA, SalSurfrange=NA, Temprange=NA, Latrange=NA, Longrange=NA, Shiny=F, ReLoadData=F, ReDownloadData=F){
  
  
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
  
  # To re-load data from local files, use option ReLoadData=T. To redownload
  # data from the internet, use option ReDownloadData=T.
  
  
  # Setup -------------------------------------------------------------------
  
  #devtools::install_github("tidyverse/dplyr")
  require(tidyverse)
  require(readxl)
  
  #Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
  require(dtplyr)
  
  
  require(lubridate)
  
  #Warnings for improper arguments
  if (!every(Sources, ~.%in%c("EMP", "FRP", "FMWT", "TNS", "20mm"))){
    stop("Sources must contain one or more of the following options: EMP, FRP, FMWT, TNS, 20mm")
  }
  
  
  if (!(Data=="Taxa" | Data=="Community")){
    stop("Data must be either `Taxa` or `Community`")
  }
  
  if(!every(list(Shiny, ReLoadData, ReDownloadData), is.logical)){
    stop("Shiny, ReLoadData, and ReDownloadData must all have logical arguments.")
  }
  
  # Load crosswalk key to convert each dataset's taxonomic codes to a
  # unified set of "Taxname" and "Lifestage" values.
  
  crosswalk <- read_excel("Data/new_crosswalk.xlsx", sheet = "Hierarchy2")%>%
    mutate_at(vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~parse_date(as.character(.), format="%Y"))%>%
    mutate_at(vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~replace_na(., as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
    mutate(EMPend = if_else(is.finite(EMPend), EMPend+years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(FMWTend = if_else(is.finite(FMWTend), FMWTend+years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(twentymmend = if_else(is.finite(twentymmend), twentymmend+years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
    mutate(Intro=replace_na(Intro, as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)
  
  #Make it possible to re-download data if desired
  if(ReLoadData){
    source("Zoop data downloader.R")
    Zoopdownloader("Data/zoopforzooper.Rds", ReDownloadData)
  }
  
  # Read in data if not already loaded
  
  if(!exists("zoop")){
  
    zoop<-readRDS("Data/zoopforzooper.Rds")
    
  }
  
  # Filter data -------------------------------------------------------------
  
  #Filter to desired data sources
  
  zoop<-filter(zoop, Source%in%Sources)
  
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
    if(some(Longrange>0, isTRUE)) {warning("Longitudes should be negative for the Delta")}    
    zoop<-filter(zoop, between(Longitude, min(Longrange), max(Longrange)))
  }
  
  
  # Apply LCD approach ------------------------------------------------------
  
  
  # Make list of taxa x life stage combos present in all original datasets
  
  #Function to detect common taxonomic names across all source datasets
  Commontaxer<-function(Taxagroup, data){
    Taxagroup<-sym(Taxagroup) #unquote input
    Taxagroup<-enquo(Taxagroup) #capture expression to pass on to functions below
    N<-data%>%
      pull(Source)%>%
      unique()%>%
      length()
    data%>%
      filter(!is.na(!!Taxagroup))%>%
      select(!!Taxagroup, Lifestage, Source)%>%
      distinct()%>%
      group_by(!!Taxagroup, Lifestage)%>%
      summarise(n=n())%>% #Create index of number of data sources in which each taxagroup x lifestage combo appears
      ungroup()%>%
      filter(n==N)%>% #only retain taxagroup x lifestage combos that appear in all datasets
      select(!!Taxagroup, Lifestage)
  }
  
  #Find all common Taxname x life stage combinations, turn into vector of Taxlifestages
  Commontax<-Commontaxer("Taxname", zoop)
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
    mutate_at(Taxcats, list(g=~ifelse(.%in%unique(Taxname), ., NA)))
  
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
  
  rm(Lumped)
  
  if(Shiny==F){
  print(paste("NOTE: These species are not counted in all datasets:", Orphans), quote=F)
  }
  
  if(Shiny==T) {
    shiny::showNotification(paste("NOTE: These species are not counted in all datasets:", Orphans), type = "message", duration = NULL)
  }
  
  # Calculate summed groups and create a final dataset
  
  zoop<-map_dfr(Taxcats_g, .f=LCD, df=zoop)%>% #Taxonomic level by level, summarise for each of these grouping categories and bind them all together
    bind_rows(zoop%>% #Bind these summarized groupings to the original taxonomic categories in the original dataset
                mutate(Taxatype=ifelse(Taxname%in%Groups, "UnID species", "Species")))%>% 
    ungroup()%>%
    mutate(Taxlifestage=paste(Taxname, Lifestage), #add back in the Taxlifestage variable (removed by the LCD function)
           Orphan=ifelse(Taxlifestage%in%strsplit(Orphans, ", ")[[1]], T, F), #add an identifier for orphan taxa (species not counted in all data sources)
           Taxname = ifelse(Taxatype=="UnID species", paste0(Taxname, "_UnID"), Taxname),
           Taxlifestage=paste(Taxname, Lifestage))%>%
    select_at(vars(-Taxcats_g))
  
  rm(Groups)
  rm(Orphans)
  
  if(Shiny==F){
  print("NOTE: Do not use this data to make additional higher-level taxonomic summaries or any other operations to add together taxa above the species level unless you first filter out all rows with Taxatype==`Summed group` and, depending on your purpose, Orphan==TRUE. Do not compare UnID categories across data sources.", quote=F)
  }
  
  if(Shiny==T) {
    shiny::showNotification("NOTE: Do not use this data to make additional higher-level taxonomic summaries or any other operations to add together taxa above the species level unless you first filter out all rows with Taxatype==`Summed group` and, depending on your purpose, Orphan==TRUE. Do not compare UnID categories across data sources.", type = "message", duration = NULL)
  }
  
  }
  

# Apply LCD approach for community data user ------------------------------

  
  if(Data=="Community"){
    
    if(!(length(Lumped)>0)){
      return(zoop)
    }
    
    #Create taxonomy table for all taxonomic levels present in all datasets
    Commontaxkey<-map_dfr(Taxcats, Commontaxer, zoop)%>%
      mutate_at(Taxcats, list(lifestage=~ifelse(is.na(.), NA, paste(., Lifestage))))%>% #Create taxa x life stage variable for each taxonomic level
      select(Genus_lifestage, Family_lifestage, Order_lifestage, Class_lifestage, Phylum_lifestage) #only retain columns we need
    
    #Create taxonomy table for taxa not present in all datasets, then select their new names corresponding to taxa x life stage combinations that are measured in all datasets
    Lumpedkey<-tibble(Taxlifestage=Lumped)%>%
      left_join(crosswalk%>%
                  select(Taxname, Lifestage, Phylum, Class, Order, Family, Genus)%>%
                  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
                  distinct(),
                by="Taxlifestage")%>%
      mutate_at(c("Genus", "Family", "Order", "Class", "Phylum"), list(lifestage=~ifelse(is.na(.), NA, paste(., Lifestage))))%>% #Create taxa x life stage variable for each taxonomic level
      mutate(Taxname_new=case_when( #This will go level by level, look for matches with the Commontaxkey, and assign the taxonomic level that matches. The "TRUE" at end specifies what to do if no conditions are met. 
        !is.na(Genus_lifestage) & Genus_lifestage%in%Commontaxkey$Genus_lifestage ~ paste0(Genus, "_Genus"),
        !is.na(Family_lifestage) & Family_lifestage%in%Commontaxkey$Family_lifestage ~ paste0(Family, "_Family"),
        !is.na(Order_lifestage) & Order_lifestage%in%Commontaxkey$Order_lifestage ~ paste0(Order, "_Order"),
        !is.na(Class_lifestage) & Class_lifestage%in%Commontaxkey$Class_lifestage ~ paste0(Class, "_Class"),
        !is.na(Phylum_lifestage) & Phylum_lifestage%in%Commontaxkey$Phylum_lifestage ~ paste0(Phylum, "_Phylum"),
        TRUE ~ "REMOVE_" #If no match is found, change to the Taxname REMOVE so we know to later remove these data from the final database
      ))%>%
      mutate(Genus=if_else(str_detect(Taxname_new, "\\_Genus"), Genus, NA_character_),
             Family=if_else(str_detect(Taxname_new, "\\_Genus|\\_Family"), Family, NA_character_),
             Order=if_else(str_detect(Taxname_new, "\\_Genus|\\_Family|\\_Order"), Order, NA_character_),
             Class=if_else(str_detect(Taxname_new, "\\_Genus|\\_Family|\\_Order|\\_Class"), Class, NA_character_)
             )%>%
      mutate(Taxname_new=str_extract(Taxname_new, "^[^_]+(?=_)"))
    
    rm(Lumped)
    rm(Commontaxkey)
    
    if("REMOVE"%in%Lumpedkey$Taxname_new){
      
      Removed<-Lumpedkey%>%
        filter(Taxname_new=="REMOVE")%>%
        pull(Taxlifestage)%>%
        unique()
      
      Removed<-paste(Removed, collapse=", ")
      
      if(Shiny==F){
        print(paste("These species have no relatives common to all datasets and have been removed: ", Removed), quote=F)
      }
      
      if(Shiny==T) {
        shiny::showNotification(paste("These species have no relatives common to all datasets and have been removed: ", Removed), type = "message", duration = NULL)
      }
    }
    
    
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
      ungroup()%>%
      as_tibble()%>%
      left_join(crosswalk%>%
                  select(Taxname, Lifestage, Phylum, Class, Order, Family, Genus, Species)%>%
                  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
                  select(-Taxname, -Lifestage)%>%
                  bind_rows(Lumpedkey%>%
                              filter(Taxname_new!="REMOVE")%>%
                              select(Taxname_new, Lifestage, Phylum, Class, Order, Family, Genus)%>%
                              mutate(Taxlifestage=paste(Taxname_new, Lifestage))%>%
                              select(-Taxname_new, -Lifestage))%>%
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

