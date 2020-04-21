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
  select(-Year, -Latitude, -Longitude)

stations<-zooper::stations

taxonomy <- zooper::crosswalk%>%
  select(Taxname, Lifestage, Phylum, Class, Order, Family, Genus, Species)%>%
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

biomass_macro<-read_excel("Data paper/Biomass conversions.xlsx", sheet=2)

data_files <- c("zooplankton.csv", "environment.csv", "taxonomy.csv", "undersampled.csv", "stations.csv", "study_metadata.csv", "biomass_mesomicro.csv", "biomass_macro.csv")

walk2(list(zoop, env, taxonomy, undersampled, stations, meta2, biomass_mesomicro, biomass_macro), data_files, ~write_csv(.x, file.path("~", "Zooplankton EDI", .y)))


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
