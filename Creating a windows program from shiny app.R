require(RInno)

create_app(
  app_name = "Zooper", 
  pkgs = c("RColorBrewer", "shiny", "tidyverse", "readxl", "dtplyr", "lubridate", "ggiraph", "leaflet", "webshot", "mapview", "shinyWidgets", "leaflet.minicharts", "randomcoloR")
)

# Somehow must use ggiraph v. 0.6.1

# add to the file: utils\launch_app.R, line 87 (before # start electron)
# Sys.sleep(3) (3 second for me to be sure, 1 could be enough)

compile_iss()