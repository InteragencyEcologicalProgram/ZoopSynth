library(zooper)
require(dplyr)
require(readr)
require(googlesheets4)
library(EMLassemblyline)
library(stringr)
require(purrr)
require(readxl)


# Create data -------------------------------------------------------------

zoop<-zooper::zoopComb%>%
  select(-Phylum, -Class, -Order, -Family, -Genus, -Species, -Source, -Taxlifestage)

env<-zooper::zoopEnvComb%>%
  select(-Year, -Latitude, -Longitude)%>%
  mutate(Date=as.character(Date, format="%Y-%m-%d"),
         Datetime=as.character(Datetime))

stations<-zooper::stations%>%
  filter(Source!="YBFMP")

stations_EMP_EZ<-zooper::stationsEMPEZ%>%
  mutate(Date=as.character(Date, format="%Y-%m-%d"))

taxonomy <- zooper::crosswalk%>%
  select(Taxname, Phylum, Class, Order, Family, Genus, Species)%>%
  distinct()

undersampled <- zooper::undersampled

# Zooplankton metadata document
meta <- read_sheet("https://docs.google.com/spreadsheets/d/1O4jg6j9ksS5VTWtm_k-iqUe8WPIBSGfrkx82CMzK8Ak/edit#gid=0", col_types="c", .name_repair="universal")

meta2<-meta%>%
  rename_all(~str_replace_all(., fixed("."), "_"))%>%
  mutate_at(c("Start_year", "Sample_duration_minutes", "Length_of_net_cm", "Mesh_size_microns"), ~parse_number(.))%>%
  filter(Study_name!="Yolo Bypass Fisheries Monitoring Survey: Lower Trophic")

biomass_mesomicro<-read_excel("Data paper/Biomass conversions.xlsx", sheet=1)%>%
  rename(Carbon_mass_micrograms=starts_with("Carbon"))

#biomass_macro<-read_excel("Data paper/Biomass conversions.xlsx", sheet=2)

data_files <- c("zooplankton.csv", "environment.csv", "taxonomy.csv", "undersampled.csv", "stations.csv", "stations_EMP_EZ.csv", "study_metadata.csv", "biomass_mesomicro.csv")

walk2(list(zoop, env, taxonomy, undersampled, stations, stations_EMP_EZ, meta2, biomass_mesomicro), data_files, ~write_csv(.x, file.path("~", "Zooplankton EDI", .y)))


# Create EML --------------------------------------------------------------

template_core_metadata(
  path="~/Zooplankton EDI",
  license="CCBY",
  file.type=".md"
)

template_table_attributes(
  path="~/Zooplankton EDI",
  data.table = data_files
)

# First fill in attributes table, then run next step

template_categorical_variables(
  path = "~/Zooplankton EDI"
)

stations%>%
  bind_rows(stations_EMP_EZ%>%
              mutate(Source="EMP",
                     Station=paste(Station, Date))%>%
              select(-Date))%>%
  mutate(Unique_station=paste(Source, Station))%>%
  write_csv(file.path("~", "Zooplankton EDI", "stations2.csv"))

# Template geographic coverage
template_geographic_coverage(
  path = "~/Zooplankton EDI",
  data.table = 'stations2.csv',
  site.col = 'Unique_station',
  lat.col = 'Latitude',
  lon.col = 'Longitude'
)
file.remove(file.path("~", "Zooplankton EDI", "stations2.csv"))

template_taxonomic_coverage(
  path = "~/Zooplankton EDI",
  taxa.table = 'taxonomy.csv',
  taxa.col = 'Taxname',
  taxa.authority = c(9,3,11),
  taxa.name.type = 'scientific'
)

make_eml(
  path = "~/Zooplankton EDI",
  dataset.title = 'Interagency Ecological Program: Zooplankton abundance in the Upper San Francisco Estuary from 1972-2018, an integration of 5 long-term monitoring programs',
  temporal.coverage = c('1972-01-01', '2018-12-19'),
  maintenance.description = 'ongoing',
  data.table = data_files,
  data.table.name = data_files,
  data.table.description = c("Catch per unit effort of micro, meso, and macro zooplankton from the Upper San Francisco Estuary.", 
                             "Datetime, environmental, and water quality data from the zooplankton surveys. Not all surveys collect data on all variables. This table can be merged to the zooplankton table using the 'SampleID' column as a key.",
                             "Taxonomic heirarchy for each species in this dataset, validated primarily with the World Registry of Marine Species.",
                             "The taxa and life stages sampled in the indicated size class in this table should be treated with caution, as they are likely under sampled, i.e. the reported number is lower than their actual abundance. More details can be found in the methods but these zooplankton are likely either small enought to escape through the mesh (for the taxa and life stages under sampled in the meso sample) or large enough to swim away from the pump (from the taxa and life stages under sampled in the micro sample).",
                             "Latitude and longitude for each fixed sampling station.",
                             "Latitude and longitude for moving EMP EZ sampling locations on each sampling date since 2004",
                             "A comprehensive table of information on the 5 component studies included in this integrated dataset.",
                             "Average carbon mass of zooplankton species and life stages obtained from the literature. Not all taxa and life stages are represented due to gaps in the literature."),
  provenance = "edi.269.2",
  user.domain = "EDI",
  user.id="sbashevkin"
)
