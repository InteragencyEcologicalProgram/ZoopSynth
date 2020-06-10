# Install the version of zooper used to create these data
#remotes::install_github("InteragencyEcologicalProgram/zooper", "v1.0.0")

library(zooper)
library(dplyr)
library(lubridate)


# Zooplankton community data, with taxonomic resolution fixed and all variables retained
zooplankton_community<-Zoopsynther(Data_type="Community")%>% # Create dataset with zooper
  select(Source, Station, Latitude, Longitude, Year, Date, Datetime, SampleID, Tide, #Re-order variables
         BottomDepth, Chl, Secchi, Temperature, Turbidity, Microcystis, pH, DO, SalSurf, SalBott, 
         SizeClass, Volume, Phylum, Class, Order, Family, Genus, Species, Taxname, Lifestage, Taxlifestage, 
         CPUE, Undersampled)

# Raw zooplankton catch data
zooplankton<-zooper::zoopComb%>% #Pull data from zooper
  select(-Phylum, -Class, -Order, -Family, -Genus, -Species, -Source, -Taxlifestage)%>% # Remove variables present in other tables
  select(SampleID, SizeClass, Volume, Taxname, Lifestage, CPUE) # Re-order variables

environment<-zooper::zoopEnvComb%>% #Pull data from zooper
  select(-Year, -Latitude, -Longitude)%>% # Remove variables present in other tables
  select(Source, Station, Date, Datetime, SampleID, Tide, # Re-order variables
         BottomDepth, Chl, Secchi, Temperature, Turbidity, Microcystis, pH, DO, SalSurf, SalBott)

stations<-zooper::stations%>% #Pull data from zooper
  filter(Source!="YBFMP")%>% # Remove station locations for dataset not included in integration
  select(Source, Station, Latitude, Longitude) # Re-order variables

stations_EMP_EZ<-zooper::stationsEMPEZ%>% #Pull data from zooper
  select(Station, Date, Latitude, Longitude) # Re-order variables

taxonomy <- zooper::crosswalk%>% #Pull data from zooper
  select(Phylum, Class, Order, Family, Genus, Species, Taxname, Level)%>% # Re-order variables and remove variables present in other tables
  distinct() # Remove any duplicated rows after selecting only taxonomic variables

taxa_lists<-zooper::crosswalk%>% #Pull data from zooper
  select(-YBFMP, -Level, -Phylum, -Class, -Order, -Family, -Genus, -Species)%>% # Remove variables present in other tables
  mutate(across(c(EMP_Micro, EMP_Meso, EMP_Macro, STN_Meso, STN_Macro, FMWT_Meso, FMWT_Macro, twentymm_Meso, FRP_Meso, FRP_Macro),
            ~if_else(is.na(.x), FALSE, TRUE)))%>% # Convert organism codes into a binary TRUE/FALSE indicating if taxa are counted in each survey and size class.
  distinct()%>% # Remove duplicated rows now that organism codes are removed
  mutate(across(c(Intro, EMPstart, EMPend, FMWTstart, FMWTend, twentymmstart, twentymmend, twentymmstart2), ~year(.x)))%>% # Convert dates to a numeric year
  select(Taxname, Lifestage, EMP_Micro, EMP_Meso, EMP_Macro, STN_Meso, STN_Macro, FMWT_Meso, FMWT_Macro, twentymm_Meso, FRP_Meso, FRP_Macro,
         Intro, EMPstart, EMPend, FMWTSTNstart=FMWTstart, FMWTSTNend=FMWTend, twentymmstart, twentymmend, twentymmstart2) #Re-oroder and rename variables for consistency

undersampled <- zooper::undersampled%>% #Pull data from zooper
  select(SizeClass, Taxname, Lifestage) # Re-order variables

# The study_metadata tables and biomass_mesomicro tables are pulled directly from their spreadsheets and not manipulated in any way. They are also not present in zooper.
