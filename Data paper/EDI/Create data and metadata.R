library(zooper)
require(dplyr)
require(readr)
require(googlesheets4)
library(EMLassemblyline)
library(stringr)
require(purrr)
require(readxl)
require(EML)
require(lubridate)

root<-"C:/Users/sbashevkin/OneDrive - deltacouncil/Zooplankton synthesis/Zooplankton EDI/V2"
path_templates <- file.path(root, "metadata_templates")
path_data <- file.path(root, "data_objects")
path_eml <- file.path(root, "eml")

data_files <- c("zooplankton_community.csv", "zooplankton.csv", "environment.csv", "taxonomy.csv", "taxa_lists.csv", "undersampled.csv", "stations.csv", "stations_EMP_EZ.csv", "study_metadata.csv", "biomass_mesomicro.csv")

# Create data -------------------------------------------------------------

zoop_com<-Zoopsynther(Data_type="Community")%>%
  mutate(Date=as.character(Date, format="%Y-%m-%d"),
         Datetime=as.character(Datetime))%>%
  select(Source, Station, Latitude, Longitude, Year, Date, Datetime, SampleID, Tide, 
         BottomDepth, Chl, Secchi, Temperature, Turbidity, Microcystis, pH, DO, SalSurf, SalBott, 
         SizeClass, Volume, Phylum, Class, Order, Family, Genus, Species, Taxname, Lifestage, Taxlifestage, 
         CPUE, Undersampled)

zoop<-zooper::zoopComb%>%
  select(-Phylum, -Class, -Order, -Family, -Genus, -Species, -Source, -Taxlifestage)%>%
  select(SampleID, SizeClass, Volume, Taxname, Lifestage, CPUE)

env<-zooper::zoopEnvComb%>%
  select(-Year, -Latitude, -Longitude)%>%
  mutate(Date=as.character(Date, format="%Y-%m-%d"),
         Datetime=as.character(Datetime))%>%
  select(Source, Station, Date, Datetime, SampleID, Tide, 
         BottomDepth, Chl, Secchi, Temperature, Turbidity, Microcystis, pH, DO, SalSurf, SalBott)

stations<-zooper::stations%>%
  filter(Source!="YBFMP")%>%
  select(Source, Station, Latitude, Longitude)

stations_EMP_EZ<-zooper::stationsEMPEZ%>%
  mutate(Date=as.character(Date, format="%Y-%m-%d"))%>%
  select(Station, Date, Latitude, Longitude)

taxonomy <- zooper::crosswalk%>%
  select(Phylum, Class, Order, Family, Genus, Species, Taxname, Level)%>%
  distinct()

taxa_lists<-zooper::crosswalk%>%
  select(-YBFMP, -Level, -Phylum, -Class, -Order, -Family, -Genus, -Species)%>%
  mutate_at(vars(EMP_Micro, EMP_Meso, EMP_Macro, STN_Meso, STN_Macro, FMWT_Meso, FMWT_Macro, twentymm_Meso, FRP_Meso, FRP_Macro),
            ~if_else(is.na(.), FALSE, TRUE))%>%
  distinct()%>%
  mutate_at(vars(Intro, EMPstart, EMPend, FMWTstart, FMWTend, twentymmstart, twentymmend, twentymmstart2), ~year(.))%>%
  select(Taxname, Lifestage, EMP_Micro, EMP_Meso, EMP_Macro, STN_Meso, STN_Macro, FMWT_Meso, FMWT_Macro, twentymm_Meso, FRP_Meso, FRP_Macro,
         Intro, EMPstart, EMPend, FMWTSTNstart=FMWTstart, FMWTSTNend=FMWTend, twentymmstart, twentymmend, twentymmstart2)

undersampled <- zooper::undersampled%>%
  select(SizeClass, Taxname, Lifestage)

# Zooplankton metadata document
meta <- read_sheet("https://docs.google.com/spreadsheets/d/1O4jg6j9ksS5VTWtm_k-iqUe8WPIBSGfrkx82CMzK8Ak/edit#gid=0", col_types="c", .name_repair="universal")

meta2<-meta%>%
  rename_all(~str_replace_all(., fixed("."), "_"))%>%
  mutate_at(c("Start_year", "Sample_duration_minutes", "Length_of_net_cm", "Mesh_size_microns"), ~parse_number(.))%>%
  filter(Study_name!="Yolo Bypass Fisheries Monitoring Survey: Lower Trophic")%>%
  rename(Survey_name=Study_name)

biomass_mesomicro<-read_excel("Data paper/Biomass conversions.xlsx", sheet=1)%>%
  select(Taxname, Level, Lifestage, Carbon_mass_micrograms=starts_with("Carbon"), Reference)

#biomass_macro<-read_excel("Data paper/Biomass conversions.xlsx", sheet=2)

walk2(list(zoop_com, zoop, env, taxonomy, taxa_lists, undersampled, stations, stations_EMP_EZ, meta2, biomass_mesomicro), data_files, ~write_csv(.x, file.path(path_data, .y)))

file.copy(from=file.path("Data paper", "EDI", "Data publication code.R"),
          to=file.path(path_data, "Data_processing.R"),
          overwrite = TRUE)

# Create EML --------------------------------------------------------------

template_core_metadata(
  path=path_templates,
  license="CCBY",
  file.type=".docx"
)
# Create provenance template

EMLassemblyline::template_provenance(
  path=path_templates,
)

template_table_attributes(
  path=path_templates,
  data.path = path_data,
  data.table = data_files
)

# First fill in attributes table, then run next step

template_categorical_variables(
  path = path_templates, 
  data.path = path_data
)

stations%>%
  bind_rows(stations_EMP_EZ%>%
              mutate(Source="EMP",
                     Station=paste(Station, Date))%>%
              select(-Date))%>%
  mutate(Unique_station=paste(Source, Station))%>%
  write_csv(file.path(tempdir(), "stations2.csv"))

# Template geographic coverage
template_geographic_coverage(
  path = path_templates, 
  data.path = tempdir(), 
  data.table = 'stations2.csv',
  site.col = 'Unique_station',
  lat.col = 'Latitude',
  lon.col = 'Longitude'
)
file.remove(file.path(tempdir(), "stations2.csv"))

template_taxonomic_coverage(
  path = path_templates,
  data.path = path_data,
  taxa.table = 'taxonomy.csv',
  taxa.col = 'Taxname',
  taxa.authority = c(9,3,11),
  taxa.name.type = 'scientific'
)



#ID<-"edi.230.2" # Sandbox EDI
ID<-"edi.539.2" # Real EDI

zoop_eml<-make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml, 
  dataset.title = 'Interagency Ecological Program: Zooplankton abundance in the Upper San Francisco Estuary from 1972-2020, an integration of 5 long-term monitoring programs',
  temporal.coverage = c('1972-01-01', '2020-07-10'),
  maintenance.description = 'ongoing',
  data.table = data_files,
  data.table.name = data_files,
  data.table.description = c("The full dataset optimized for a community ecology analysis. This includes data from the rest of the tables, except the biomass table. So users should either use this file or construct their own dataset with the other tables. This dataset was optimized for community analysis by forcing each survey to have the same set of measured taxa. Any taxa not counted by all datasets was either summed to a higher level that would be comparable with all datasets, or removed if it had no close relatives in all datasets. However, taxonomic resolution is not consistent over time. This dataset was created using the zooper R package v2.2.0 with zoopSynther(Data_type='Community'). To create a more customized dataset and read more information about how this dataset was created, visit https://github.com/InteragencyEcologicalProgram/zooper.",
                             "Catch per unit effort of micro, meso, and macro zooplankton from the Upper San Francisco Estuary.", 
                             "Datetime, environmental, and water quality data from the zooplankton surveys. Not all surveys collect data on all variables. This table can be merged to the zooplankton table using the 'SampleID' column as a key.",
                             "Taxonomic heirarchy for each species in this dataset, validated primarily with the World Registry of Marine Species.",
                             "List of taxa identified by each survey and net size and dates of any changes.",
                             "The taxa and life stages sampled in the indicated size class in this table should be treated with caution, as they are likely under sampled, i.e. the reported number is lower than their actual abundance. More details can be found in the methods but these zooplankton are likely either small enought to escape through the mesh (for the taxa and life stages under sampled in the meso sample) or large enough to swim away from the pump (for the taxa and life stages under sampled in the micro sample).",
                             "Latitude and longitude for each fixed sampling station.",
                             "Latitude and longitude for moving EMP EZ sampling locations on each sampling date since 2004",
                             "A comprehensive table of information on the 5 component studies included in this integrated dataset.",
                             "Average carbon mass of zooplankton species and life stages obtained from the literature. Not all taxa and life stages are represented due to gaps in the literature."),
  data.table.quote.character = rep("\"", length(data_files)),
  other.entity = "Data_processing.R",
  other.entity.name = "Data processing code",
  other.entity.description = "R code used to process data created with the R package zooper (v2.2.0) into the format published here. Data processing mostly involved removing duplicative variables",
  user.domain = "EDI",
  user.id="sbashevkin",
  return.obj=TRUE,
  #write.file=FALSE,
  package.id=ID
)

changelog<-list(changeScope="Metadata and data",
                oldValue="See previous revision 1",
                changeDate=Sys.Date(),
                comment="1) Added 2018-2020 20mm data and 2019 EMP, FMWT, and STN data. 
                         2) Corrected timezone error that was shifting the date of some STN, FMWT, and 20mm samples by 1 day relative to the datetime.
                         3) Updated FMWT station location coordinates to the newest version from the FMWT database.
                         4) Corrected inconsistent capitalization of the STN Mont and Honk stations, resulting in correct coordinates for all those samples.
                         5) Removed EMP Meso Sample corresponding to a mostly empty line at the end of the csv.
                         6) Added station location for FMWT 520 (=STN 520)
                         7) Fixed 20mm bottom depths (formally in feet, now correcting to meters like all other studies are converted).
                         8) Changed the name of the 20mm dataset to '20mm' almost everywhere (except column names) for consistency. It was previously 'twentymm' in some tables.")
class(changelog)<-c("emld", "list")

zoop_eml$dataset$maintenance$changeHistory<-changelog
write_eml(zoop_eml, file.path(path_eml, paste0(ID, ".xml")))
eml_validate(file.path(path_eml, paste0(ID, ".xml")))
