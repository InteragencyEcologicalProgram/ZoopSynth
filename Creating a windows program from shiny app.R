
# WARNING -----------------------------------------------------------------

# BE CAREFUL WHEN RUNNING THIS BECAUSE IT WILL OVERWRITE FILES


# Transfer files ----------------------------------------------------------

#devtools::install_github("sbashevkin/RInno") #Must use my modified version of RInno
require(RInno)

file.copy(from=file.path(getwd(), c("app.R", "Zoop synthesizer function.R")), to=file.path("..", "Zoop App"), overwrite = TRUE)
file.copy(from=file.path(getwd(), "Data", c("zoopforzooper.Rds", "zoopenvforzooper.Rds", "new_crosswalk.xlsx", "Undersampled taxa.xlsx")), to=file.path("..", "Zoop App", "Data"), overwrite = TRUE)


# Create application ------------------------------------------------------


# Must use ggiraph v. 0.6.1
# Before running `Create_app`, be sure to download windows binary (.zip) version of 
# ggiraph 0.6.1 (from https://mran.microsoft.com/timemachine, go to 05/01/2019)
# and place that binary zipped version within the folder ~/Zoop App/bin.

#While running, say "no" to all prompts about updating packages

create_app(
  app_name = "Zooper", 
  app_dir = file.path("..", "Zoop App"),
  pkgs = c("RColorBrewer", "shiny", "tidyverse", "readxl", "ggiraph", "dtplyr", "lubridate", #"ggiraph", 
           "leaflet", "webshot", "mapview", "shinyWidgets", "leaflet.minicharts", "randomcoloR"),
  include_R = TRUE,
  app_icon= "Zooper_icon.ico"
)



# add to the file: utils\launch_app.R, line 87 (before # start electron)
# Sys.sleep(3) (3 second for me to be sure, 1 could be enough)


# Create installer --------------------------------------------------------

#Where is Inno installed on your computer?
Inno_dir<-"C:/Users/sbashevkin/AppData/Local/Programs"

compile_iss(Inno_dir)