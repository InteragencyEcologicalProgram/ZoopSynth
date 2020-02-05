
# WARNING -----------------------------------------------------------------

# BE CAREFUL WHEN RUNNING THIS BECAUSE IT WILL OVERWRITE FILES


# Transfer files ----------------------------------------------------------

#devtools::install_github("sbashevkin/RInno") #Must use my modified version of RInno
require(RInno)

#First must create "Data" and "www" folders within the "Zoop App" folder.

file.copy(from=file.path(getwd(), "app.R"), to=file.path("..", "Zoop App"), overwrite = TRUE)

#file.copy(from=file.path(getwd(), c("app.R", "Zoop synthesizer function.R")), to=file.path("..", "Zoop App"), overwrite = TRUE)
#file.copy(from=file.path(getwd(), "Data", c("zoopforzooper.Rds", "zoopenvforzooper.Rds", "new_crosswalk.xlsx", "Undersampled taxa.xlsx")), to=file.path("..", "Zoop App", "Data"), overwrite = TRUE)
file.copy(from=file.path(getwd(), "www", c("Logo.jpg", "zooper.png")), to=file.path("..", "Zoop App", "www"), overwrite = TRUE)


# Create application ------------------------------------------------------


# Must use ggiraph v. 0.6.1
# Before running `Create_app`, be sure to download windows binary (.zip) version of 
# ggiraph 0.6.1 (from https://mran.microsoft.com/timemachine, go to 05/01/2019)
# and place that binary zipped version within the folder ~/Zoop App/bin.

#While running, say "no" to all prompts about updating packages

create_app(
  app_name = "Zooper_app", 
  app_dir = file.path("..", "Zoop App"),
  pkgs = c("RColorBrewer", "shiny", "tidyverse", "readxl", "ggiraph", "dtplyr", "lubridate",
           "leaflet", "webshot", "mapview", "shinyWidgets", "leaflet.minicharts"),
  include_R = TRUE,
  app_icon= "Zooper_icon.ico",
  user_browser = "chrome"
)


# Create installer --------------------------------------------------------

#Where is Inno installed on your computer?
Inno_dir<-"C:/Users/sbashevkin/AppData/Local/Programs"

compile_iss(Inno_dir)
