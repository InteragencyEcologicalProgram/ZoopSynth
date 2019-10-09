Zooper<-function(Sources=c("EMP", "FRP", "FMWT", "TNS", "twentymm"), Size_class=c("Micro", "Meso", "Macro"), Data="Community", Date_range=c(NA, NA), Months=NA, Years=NA, Sal_bott_range=NA, Sal_surf_range=NA, Temp_range=NA, Lat_range=NA, Long_range=NA, Shiny=F, Reload_data=F, Redownload_data=F, All_env=T){
  
  
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
  
  # To re-load data from local files, use option Reload_data=T. To redownload
  # data from the internet, use option Redownload_data=T.
  
  
  # Setup -------------------------------------------------------------------
  
  require(tidyverse)
  require(readxl)
  
  #Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
  require(dtplyr)
  
  require(lubridate)
  
  #Warnings for improper arguments
  if (!every(Sources, ~.%in%c("EMP", "FRP", "FMWT", "TNS", "twentymm"))){
    stop("Sources must contain one or more of the following options: EMP, FRP, FMWT, TNS, twentymm")
  }
  
  
  if (!(Data=="Taxa" | Data=="Community")){
    stop("Data must be either `Taxa` or `Community`")
  }
  
  if(!every(list(Shiny, Reload_data, Redownload_data), is.logical)){
    stop("Shiny, Reload_data, and Redownload_data must all have logical arguments.")
  }
  
  # Load crosswalk key to convert each dataset's taxonomic codes to a
  # unified set of "Taxname" and "Lifestage" values.
  
  crosswalk <- read_excel("Data/new_crosswalk.xlsx", sheet = "Hierarchy2")
  
  #Make it possible to re-download data if desired
  if(Reload_data){
    source("Zoop data downloader.R")
    Zoopdownloader("Data/zoopforzooper.Rds", Redownload_data)
  }
  
  # Read in data if not already loaded
  
  zoop<-readRDS("Data/zoopforzooper.Rds")
  zoopEnv<-readRDS("Data/zoopenvforzooper.Rds")
  
  # Filter data -------------------------------------------------------------
  
  #Filter to desired data sources
  
  zoopEnv<-filter(zoopEnv, Source%in%Sources)
  
  #Filter data by specified variables if users provide appropriate ranges
  
  if(!every(Sal_bott_range, is.na)) {
    if(some(Sal_bott_range, is.na)) {
      stop("One element of Sal_bott_range cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoopEnv<-filter(zoopEnv, between(SalBott, min(Sal_bott_range), max(Sal_bott_range)))
  }
  
  if(!every(Sal_surf_range, is.na)) {
    if(some(Sal_surf_range, is.na)) {
      stop("One element of Sal_surf_range cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoopEnv<-filter(zoopEnv, between(SalSurf, min(Sal_surf_range), max(Sal_surf_range)))
  }
  
  if(!every(Temp_range, is.na)) {
    if(some(Temp_range, is.na)) {
      stop("One element of Temp_range cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoopEnv<-filter(zoopEnv, between(Temperature, min(Temp_range), max(Temp_range)))
  }
  
  if(!is.na(Date_range[1])){
    Datemin<-as_date(Date_range[1])
    if(is.na(Datemin)) {stop("Date_range[1] in incorrect format. Reformat to `yyyy-mm-dd`")}
    zoopEnv<-filter(zoopEnv, Date>Datemin)
  }
  
  if(!is.na(Date_range[2])){
    Datemax<-as_date(Date_range[2])
    if(is.na(Datemax)) {stop("Date_range[2] in incorrect format. Reformat to `yyyy-mm-dd`")}
    zoopEnv<-filter(zoopEnv, Date<Datemax)
  }
  
  if(!every(Months, is.na)) {
    zoopEnv<-filter(zoopEnv, month(Date)%in%Months)
  }
  
  if(!every(Years, is.na)) {
    zoopEnv<-filter(zoopEnv, Year%in%Years)
  }
  
  if(!every(Lat_range, is.na)) {
    if(some(Lat_range, is.na)) {
      stop("One element of Lat_range cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    zoopEnv<-filter(zoopEnv, between(Latitude, min(Lat_range), max(Lat_range)))
  }
  
  if(!every(Long_range, is.na)) {
    if(some(Long_range, is.na)) {
      stop("One element of Long_range cannot be NA, use Inf or -Inf to set limitless bounds")
    }
    if(some(Long_range>0, isTRUE)) {warning("Longitudes should be negative for the Delta")}
    zoopEnv<-filter(zoopEnv, between(Longitude, min(Long_range), max(Long_range)))
  }
  
  zoop<-filter(zoop, SampleID%in%unique(zoopEnv$SampleID) & SizeClass%in%Size_class)
  # Apply LCD approach ------------------------------------------------------
  
  #Make vector of taxonomic categories that we will use later
  Taxcats<-c("Genus", "Family", "Order", "Class", "Phylum")
  
  # Make list of taxa x life stage combos present in all original datasets
  SourceTaxaKeyer<-function(source, crosswalk){
    source2<-sym(source) #unquote input
    source2<-enquo(source2) #capture expression to pass on to functions below
    crosswalk%>%
      filter(!is.na(!!source2))%>%
      select_at(c(Taxcats, "Taxname", "Lifestage"))%>%
      distinct()%>%
      mutate(Source=source)
  }
  
  SourceTaxaKey<-map_dfr(unique(paste(zoop$Source, zoop$SizeClass, sep="_")), SourceTaxaKeyer, crosswalk)%>%
    separate(Source, into=c("Source", "SizeClass"), sep="_")
  
  
  #Function to detect common taxonomic names across all source datasets
  Commontaxer<-function(Taxagroup, sourcetaxakey){
    Taxagroup<-sym(Taxagroup) #unquote input
    Taxagroup<-enquo(Taxagroup) #capture expression to pass on to functions below
    N<-sourcetaxakey%>%
      pull(Source)%>%
      unique()%>%
      length()
    sourcetaxakey%>%
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
  Commontax<-sapply(unique(zoop$SizeClass), function(x) 
    filter(SourceTaxaKey, SizeClass==x)%>%
      Commontaxer("Taxname", .)%>%
      mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
      pull(Taxlifestage), 
    simplify=F)
  
  # Make list of taxalifestages that do not appear in all datasets
  
  Lumped<-sapply(unique(zoop$SizeClass), function(x) 
    setdiff(unique(filter(zoop, SizeClass==x)$Taxlifestage), Commontax[[x]]), simplify=F)
  
  
  # Apply LCD approach for taxa-level data user ----------------------
  
  
  
  if(Data=="Taxa"){
    
    #Make vector of _g Taxonomic variables
    Taxcats_g<-paste0(Taxcats, "_g")
    
    # Define function to calculate least common denominator for each taxonomic level
    LCD_Taxa<-function(df, Taxagroup){
      Taxagroup2<-sym(Taxagroup) #unquote input
      Taxagroup2<-enquo(Taxagroup2) #capture expression to pass on to functions below
      out<-df%>%
        filter(!is.na(!!Taxagroup2))%>% #filter to include only data belonging to the taxonomic grouping
        group_by(!!Taxagroup2)%>%
        mutate(N=length(unique(Taxname)))%>%
        filter(N>1)%>%
        ungroup()%>%
        lazy_dt()%>%
        group_by_at(vars(-c("Taxname", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxlifestage", "CPUE",Taxcats_g[!Taxcats_g%in%Taxagroup])))%>% #Group data by relavent grouping variables (including taxonomic group) for later data summation
        summarise(CPUE=sum(CPUE, na.rm=T))%>% #Add up all members of each grouping taxon
        ungroup()%>%
        as_tibble()%>%
        mutate(Taxname=!!Taxagroup2, #Add summarized group names to Taxname
               Taxatype="Summed group")%>% #Add a label to these summed groups so they can be removed later if users wish
        left_join(crosswalk%>%
                    select_at(vars(Taxname, Taxcats))%>%
                    distinct(), by="Taxname")%>%
        mutate(Taxname=paste0(Taxname, "_all")) #Differentiate grouped Taxnames from others
      return(out)
    }
    
    # Select higher level groupings that correspond to Taxnames, i.e., are a 
    # classification category in one of the original datasets, and rename 
    # them as "Taxonomiclevel_g"
    
    zoop<-zoop%>%
      mutate_at(Taxcats, list(g=~ifelse(.%in%unique(Taxname), ., NA)))
    
    
    #Extract vector of grouping taxa (i.e. all unique taxa retained in the above step)
    
    Groups<-sapply(unique(zoop$SizeClass), function(x)
      zoop%>%
        filter(SizeClass==x)%>%
        select(Genus_g, Family_g, Order_g, Class_g, Phylum_g)%>%
        gather("Level", "Species")%>%
        filter(!is.na(Species))%>%
        pull(Species)%>%
        unique(), simplify=F)
    
    # Output list of taxa that were not measured in all datasets, and are 
    # not higher taxa that can be calculated by summing lower taxa, i.e. 
    # "orphan taxa"
    
    Orphans<-sapply(unique(zoop$SizeClass), function(x) 
      paste(Lumped[[x]][-str_which(paste0("[", paste(Groups[[x]], collapse="|"), "]"), word(Lumped[[x]], 1, -2))], collapse=", "), 
      simplify=F)
    
    rm(Lumped)
    
    Orphansdf<-Orphans[which(nchar(Orphans)>0)]
    Orphansdf<-map(set_names(names(Orphansdf)), ~strsplit(Orphansdf[[.x]], ", ")[[1]])%>%
      enframe(name = "SizeClass", value = "Taxlifestage")%>%
      unnest(Taxlifestage)%>%
      mutate(Orphan=TRUE)
    
    
    # Calculate summed groups and create a final dataset
    
    zoop<-map_dfr(Taxcats_g, .f=LCD_Taxa, df=zoop)%>% #Taxonomic level by level, summarise for each of these grouping categories and bind them all together
      bind_rows(zoop%>% #Bind these summarized groupings to the original taxonomic categories in the original dataset
                  mutate(Taxatype=ifelse(Taxname%in%unique(unlist(Groups)), "UnID species", "Species")))%>% 
      ungroup()%>%
      mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #add back in the Taxlifestage variable (removed by the LCD_Taxa function)
      left_join(Orphansdf, by=c("Taxlifestage", "SizeClass"))%>%
      mutate(Orphan=replace_na(Orphan, FALSE))%>%#add an identifier for orphan taxa (species not counted in all data sources)
      mutate(Taxname = ifelse(Taxatype=="UnID species", paste0(Taxname, "_UnID"), Taxname))%>%
      select_at(vars(-Taxcats_g))%>%
      left_join(zoopEnv%>%
                  {if(!All_env){
                    select(., Year, Date, SalSurf, Latitude, Longitude, SampleID)
                  } else{
                    select(., -Source)
                  }}, by="SampleID")
    
    caveats<-c(paste0("These species are not counted in all datasets: ", paste(unique(unlist(sapply(names(Orphans), function(x) strsplit(Orphans[[x]], ", ")[[1]]))), collapse=" "), "NOTE: Do not use this data to make additional higher-level taxonomic summaries or any other operations to add together taxa above the species level unless you first filter out all rows with Taxatype==`Summed group` and, depending on your purpose, Orphan==TRUE. Orphan status varies with size class. Do not compare UnID categories across data sources."))
    
    rm(Groups)
    rm(Orphans)
    
  }
  
  
  # Apply LCD approach for community data user ------------------------------
  
  
  if(Data=="Community"){
    
    if(!some(sapply(Lumped, length), function(x) x>0)){
      out<-list(Data=zoop%>%
                  left_join(zoopEnv%>%
                              {if(!All_env){
                                select(., Year, Date, SalSurf, Latitude, Longitude, SampleID)
                              } else{
                                select(., -Source)
                              }}, by="SampleID"), Caveats="No disclaimers here! Enjoy the clean data!")
      
      if(Shiny){
        return(out)
      } else{
        print(out$Caveats)
        return(out$Data)
      }
    }
    
    #Create taxonomy table for all taxonomic levels present in all datasets
    Commontaxkey<-map2_dfr(set_names(rep(unique(zoop$SizeClass), each=length(Taxcats))),
                           rep(Taxcats, length(unique(zoop$SizeClass))), 
                           ~Commontaxer(.y, SourceTaxaKey%>%filter(SizeClass==.x)), 
                           .id = "SizeClass")%>%
      mutate_at(Taxcats, list(lifestage=~ifelse(is.na(.), NA, paste(., Lifestage))))%>% #Create taxa x life stage variable for each taxonomic level
      select(Genus_lifestage, Family_lifestage, Order_lifestage, Class_lifestage, Phylum_lifestage, SizeClass) #only retain columns we need
    
    #Create taxonomy table for taxa not present in all datasets, then select their new names corresponding to taxa x life stage combinations that are measured in all datasets
    LCD_Com<-function(Lumped, crosswalk, Commontaxkey){
      tibble(Taxlifestage=Lumped)%>%
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
    }
    
    Lumpedkey<-map_dfr(set_names(unique(zoop$SizeClass)), 
                       ~LCD_Com(Lumped[[.]], crosswalk, filter(Commontaxkey, SizeClass==.)),
                       .id = "SizeClass")
    
    rm(Lumped)
    rm(Commontaxkey)
    
    if("REMOVE"%in%Lumpedkey$Taxname_new){
      
      Removed<-Lumpedkey%>%
        filter(Taxname_new=="REMOVE")%>%
        pull(Taxlifestage)%>%
        unique()
      
      Removed<-paste(Removed, collapse=", ")
      
      caveats<-paste0("These species have no relatives in their size class common to all datasets and have been removed from one or more size classes: ", Removed)
    } else{
      caveats<-"No disclaimers here! Enjoy the clean data!"
    }
    
    #
    zoop<-zoop%>%
      left_join(Lumpedkey%>%
                  select(Taxlifestage, Taxname_new, SizeClass),
                by=c("Taxlifestage", "SizeClass"))%>%
      mutate(Taxname=if_else(is.na(Taxname_new), Taxname, Taxname_new))%>%
      select(-Taxname_new)%>%
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
      )%>%
      left_join(zoopEnv%>%
                  {if(!All_env){
                    select(., Year, Date, SalSurf, Latitude, Longitude, SampleID)
                  } else{
                    select(., -Source)
                  }}, by="SampleID")
  }
  
  out<-list(Data=zoop, Caveats=caveats)
  
  if(Shiny){
    return(out)
  } else{
    print(out$Caveats)
    return(out$Data)
  }
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

# 4)  Try speeding up code more. Bottleneck now is probably read_excel()

# 5)  Do we want to append something like "_UnID" to the taxnames of Lumped 
# (LCDed) taxa under option Data=="Community"???


# 6)  How do we apply the LCD approach for community data to issues arrising
#   from changing taxonomic resolution over time in individual datasets

