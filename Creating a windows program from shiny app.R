
# WARNING -----------------------------------------------------------------

# BE CAREFUL WHEN RUNNING THIS BECAUSE IT WILL OVERWRITE FILES


# Transfer files ----------------------------------------------------------

#devtools::install_github("sbashevkin/RInno") #Must use my modified version of RInno
require(RInno)

#First must create "Zoop App" folder and "www" subfolder.

file.copy(from=file.path("app", "app.R"), to=file.path("..", "Zoop App"), overwrite = TRUE)
file.copy(from=file.path("app", "www", c("Logo.jpg", "zooper.png")), to=file.path("..", "Zoop App", "www"), overwrite = TRUE)


# Create application ------------------------------------------------------


# First must build a binary version of the zooper package
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
#Inno_dir<-"C:/Users/sbashevkin/AppData/Local/Programs"
Inno_dir<-"C:\\Program Files (x86)"

compile_iss(Inno_dir)
