
# Setup -------------------------------------------------------------------

#options(shiny.reactlog = TRUE)
#options(shiny.jquery.version = 1)
#require(reactlog)

require(RColorBrewer)
require(shiny)
require(tidyverse)
require(readxl)
require(dtplyr)
require(lubridate)
require(ggiraph) # NEW VERSION MAKES APP SUPER SLOW, STICK WITH 0.6.1 devtools::install_version("ggiraph", version = "0.6.1", repos = "http://cran.us.r-project.org")
require(leaflet)
require(webshot)
require(mapview)
require(shinyWidgets) 
require(leaflet.minicharts)

#required for users to download map plots from shinyapps.io
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }

#Source zooplankton synthesizer function
source("Zoop synthesizer function.R")

#Settings for the "data crunching" message. 
info_loading <- "Data crunching in progress..."
progress_color <- "#CD2626"
progress_background <- "#D3D3D3"

#Function to create a custom legend (used for bubble size legend in taxa map)
addLegendCustom <- function(map, colors, labels, sizes, position, opacity = 0.5, title){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px", "; border-radius: 50%")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, 
                   opacity = opacity, position=position, title=title))
}


# Define user interface for app -------------------------------------------


ui <- fluidPage(
  
  # Application title and IEP logo
  titlePanel(title=div(h1("Zooplankton data synthesizer: TEST VERSION", style="display: inline-block"), img(src="Logo.jpg", height = 132, width = 100, align="right", style="display: inline-block")), windowTitle = "Zooplankton data synthesizer"),
  
  
  # Sidebar with user instructions, input, and downloading options ----------
  
  fluidRow(
    column(3,
           
           #Instructions
           actionBttn("Instructions", "Instructions", style="simple", color="primary", icon=icon("question-circle")),
           br(), br(),
           
           #Select data type and sources
           radioGroupButtons("Datatype", "Data Type:", choices = c("Taxa", "Community"), selected = "Community", individual = TRUE, checkIcon = list( yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
           awesomeCheckboxGroup("Sources",
                                "Sources:",
                                choices = c("Environmental Monitoring Program (EMP)" = "EMP", 
                                            "Fish Restoration Program (FRP)" = "FRP", "Fall Midwater Trawl (FMWT)" = "FMWT", "Summer Townet Survey (TNS)" = "TNS", "20mm Survey (twentymm)" = "twentymm")),
           awesomeCheckboxGroup("Size_class",
                                "Size classes:",
                                choices = c("Micro (43 micrometer mesh)"="Micro", "Meso (150-160 micrometer mesh)"="Meso", "Macro (500-505 micrometer mesh)"="Macro"), selected = "Meso"),
           
           #Allow users to select which filters they would like to use, then those filter options will appear.
           awesomeCheckboxGroup("Filters",
                                "Filters:",
                                choices = c("Dates", "Months", "Surface salinity", "Latitude", "Longitude")),
           conditionalPanel(condition = "input.Filters.includes('Dates')",
                            dateRangeInput("Date_range", label = "Date range", 
                                           start = "1972-01-01", end = "2018-12-31", startview = "year")),
           conditionalPanel(condition = "input.Filters.includes('Months')",
                            pickerInput("Months", "Months:", choices=c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12), selected = 1, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3"))),
           conditionalPanel(condition = "input.Filters.includes('Surface salinity')",
                            sliderInput("Sal_surf_range",
                                        "Surface salinity:",
                                        min = 0, max = 32, value = c(0,7), step=0.1)),
           conditionalPanel(condition = "input.Filters.includes('Latitude')",
                            sliderInput("Lat_range",
                                        "Latitude Range",
                                        min = 37.8, max = 38.6, value = c(37.8, 38.6), step=0.05)),
           conditionalPanel(condition = "input.Filters.includes('Longitude')",
                            sliderInput("Long_range",
                                        "Longitude Range:",
                                        min = -122.5, max = -121.3, value = c(-122.5, -121.3), step=0.05)),
           
           #Button to run synthesizer function
           actionBttn("Run", "Run", style="bordered", icon = icon("play"), color="danger", size="sm"),
           
           #Allows users to select taxa, but only in Taxa mode, and updates Taxa choices based on 
           #the data selections the user has made (after hitting Run). This is conditional on an 
           #output so that it only appears when the app has actually created Taxa data, rather
           #than when the user has checked the taxa box but not yet clicked run. It prevents the
           #user from accidentally filtering taxa in the "Community" mode.
           conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                            uiOutput("select_Taxlifestage")), 
           conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                            actionBttn("Update_taxa", "Update taxa", style="bordered", icon = icon("sync"), color="primary"), size="sm"),
           br(), br(),
           
           #Buttons to download data and plots
           actionBttn("Download", "Download data", style="bordered", color = "primary", size="sm", icon=icon("file-download")),
           br(), br(),
           actionBttn("Saveplot", "Download plot", icon = icon("chart-bar"), size="sm", style = "bordered", color="royal"),
           br(), br(),
           conditionalPanel(condition = "output.Datatype == 'Taxa' || output.Datatype == 'Community'", 
                            actionBttn('Disclaimer',"Data disclaimer", style="jelly", color = "danger", icon = icon("exclamation-triangle")))
    ),
    
    
    # Display plots and plot options ------------------------------------------
    
    column(9,
           tabsetPanel(type="tabs",
                       id = "Tab",
                       
                       #Sample plot
                       tabPanel("Samples", br(), 
                                
                                #Info
                                fluidRow(actionBttn("Sample_info", label = NULL, style="material-circle", 
                                                    color="primary", icon=icon("question"))), 
                                
                                #Plot
                                fluidRow(ggiraphOutput("Sampleplot"))),
                       
                       #CPUE plot
                       tabPanel("CPUE", br(),
                                
                                #Info
                                fluidRow(column(1, actionBttn("CPUE_info", label = NULL, style="material-circle", 
                                                              color="primary", icon=icon("question"))), 
                                         
                                         #Select size classes
                                         column(2, uiOutput("select_SizeClass_CPUE")),
                                         
                                         #Group by salinity zones
                                         column(1, materialSwitch(
                                           inputId = "Salzones",
                                           label = "Salinity zones?", 
                                           value = FALSE, inline=F,
                                           status = "primary"
                                         )), 
                                         column(8, conditionalPanel(condition = "input.Salzones", 
                                                                    sliderInput("Lowsal", "Low salinity zone", 
                                                                                min=0, max=30, value=c(0.5,6), 
                                                                                step=0.1, width="100%")))), 
                                #Plot
                                fluidRow(column(12, ggiraphOutput("CPUEplot")))),
                       
                       #Map plot
                       tabPanel("Map", fluidRow(column(1, offset = 0, style='padding:0px;', br() ,
                                                       
                                                       #Info
                                                       actionBttn("Map_info", label = NULL, style="material-circle", 
                                                                  color="primary", icon=icon("question"))), 
                                                
                                                column(2, offset = 0, style='padding:0px;', br(), 
                                                       
                                                       #Community map options
                                                       conditionalPanel(condition = "output.Datatype == 'Community'", dropdown(
                                                         tags$h3("Map settings"),
                                                         uiOutput("select_Lifestage"), 
                                                         pickerInput('Taxagroups', 'Select taxa:', choices =c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Mysida", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other"), multiple =T, selected=c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Mysida", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")),
                                                         uiOutput("select_SizeClass_mapcom"),
                                                         circle = TRUE, status = "danger",
                                                         icon = icon("gear"), width = "300px",
                                                         
                                                         tooltip = tooltipOptions(title = "Click to see inputs!")
                                                       )), 
                                                       
                                                       #Select size classess for taxa plot
                                                       conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                                                                        uiOutput("select_SizeClass_maptaxa"))),
                                                
                                                #Year slider
                                                column(9, uiOutput("select_Year"))),
                                
                                #Plot
                                fluidRow(leafletOutput("Mapplot", width = "100%", height = "100%")))
                       
           )
    )),
  
  
  
  # Misc UI settings --------------------------------------------------------
  
  
  # Display the "data crunching" message. 
  tags$head(tags$style(type="text/css",
                       paste0("
                                             #loadmessage {
                                             position: fixed;
                                             top: 0px;
                                             left: 0px;
                                             width: 100%;
                                             padding: 5px 0px 5px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 100%;
                                             color: ", progress_color,";
                                             background-color: ", progress_background,";
                                             z-index: 105;
                                             }
                                             "))),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(info_loading,id="loadmessage")),
  tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
  
  #Change disclaimer button text color to white
  tags$style(type="text/css", "#Disclaimer {color: white}"),
  
  #Specify plot sizes
  tags$head(tags$style("#Sampleplot{height:100vh !important;}")),
  tags$head(tags$style("#CPUEplot{height:80vh !important;}")),
  tags$head(tags$style("#Mapplot{height:80vh !important;}")),
  
  #Specify app icon 
  
    tags$head(
      tags$link(
        rel = "icon", 
        type = "image/x-icon", 
        href = "http://localhost:1984/Zooper_icon.ico")
    )
  
)

# Define server logic to process data, draw plots, and dowload data -------


server <- function(input, output, session) {
  
  # Data and plot download options ------------------------------------------
  
  #Modal dialog (popup window) to save plots
  Modalsave<-function(){
    modalDialog(
      conditionalPanel(condition = "input.Tab == 'Samples' || input.Tab == 'CPUE'" , 
                       awesomeRadio("Format1", "Plot format", choices=c("png", "jpeg", "tiff", "pdf", "eps", "ps"))),
      conditionalPanel(condition = "input.Tab == 'Map'", 
                       awesomeRadio("Format2", "Plot format", choices=c("png", "jpeg", "pdf"))),
      conditionalPanel(condition = "input.Tab == 'Samples' || input.Tab == 'CPUE'" , 
                       numericInput("Plotwidth", "Plot width (in)", value=8, min=1, max=15, step = 1),
                       numericInput("Plotheight", "Plot height (in)", value=4, min=1, max=15, step = 1)),
      footer = tagList(modalButton("Cancel"),
                       downloadBttn("Save", "Save", style="bordered", color = "primary", size="sm")),
      easyClose=TRUE
    )
  }
  
  #Modal dialog (popup window) to download data
  ModalDownloadData<-function(){
    modalDialog(
      h1("Data disclaimer"),
      h4("Data are subject to the following caveats:"),
      p(plotdata()$Caveats),
      footer = tagList(modalButton("Cancel"),
                       downloadBttn("Downloaddata", "Download data", style="bordered", color = "primary", size="sm")),
      easyClose=TRUE
    )
  }
  
  
  # Run zooper --------------------------------------------------------------
  
  #Using eventReactive so app only updates when "Run" button is clicked, letting you check all the boxes you want before running the app
  plotdata <- eventReactive(input$Run, {
    
    Zooper(Data = input$Datatype, 
           Sources = input$Sources, 
           Size_class=input$Size_class,
           Date_range = ifelse(rep("Dates"%in%input$Filters, 2), input$Date_range, c(NA, NA)),
           Months = ifelse(rep("Months"%in%input$Filters, length(input$Months)), as.integer(input$Months), rep(NA, length(input$Months))),  
           Sal_surf_range = ifelse(rep("Surface salinity"%in%input$Filters, 2), input$Sal_surf_range, c(NA, NA)),
           Lat_range = ifelse(rep("Latitude"%in%input$Filters, 2), input$Lat_range, c(NA, NA)), 
           Long_range = ifelse(rep("Longitude"%in%input$Filters, 2), input$Long_range, c(NA, NA)), 
           Shiny=T, All_env=F)
  })
  
  
  # Informational popups ----------------------------------------------------
  
  #Popup for data disclaimer
  observeEvent(input$Disclaimer, {
    sendSweetAlert(session, title = "Data disclaimer", text = paste0("Data are subject to the following caveats:\n\n", plotdata()$Caveats),
                   type = "info",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  #Popup for app instructions
  observeEvent(input$Instructions, {
    sendSweetAlert(session, title = "Instructions", 
                   text = tags$span(tags$p("This app combines any combination of the zoop datasets and calculates least common denominator taxa to facilitate comparisons across datasets with differing levels of taxonomic resolution."),
                                    tags$p("Option 'Data type' allows you to choose a final output dataset for either Community or Taxa-specific  analyses. If you want all available data on given Taxa, use 'Taxa' If you want to conduct a community analysis, use 'Community.'"),
                                    tags$p("Briefly, 'Community' optimizes for community-level analyses by taking all taxa by life stage combinations that are not measured in every input dataset, and summing them up taxonomic levels to the lowest taxonomic level they belong to that is covered by all datasets. Remaining Taxa x life stage combinations that are not covered in all datasets up to the phylum level (usually something like Annelida or Nematoda or Insect Pupae) are removed from the final dataset."), 
                                    tags$p("'Taxa' optimizes for the Taxa-level user by maintaining all data at the original taxonomic level. To facilitate comparions across datasets, this option also sums data into general categories that are comparable across all datasets (e.g., Calanoida_all"), 
                                    "------------------------------------------",
                                    tags$p(tags$b("App created and maintained by Sam Bashevkin with help from the IEP zooplankton synthesis team. Please email shiny@deltacouncil.ca.gov with any questions or comments"))),
                   type = "info",
                   btn_labels = "Ok", html = F, closeOnClickOutside = TRUE)
  })
  
  #Sample plot info
  observeEvent(input$Sample_info, {
    sendSweetAlert(session, title = "Plot info", text = tags$span(h2("Total number of zooplankton samples collected each month by each survey."), p("Hover over the plot with your mouse to view data values.")),
                   type = "info",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  #CPUE plot info
  observeEvent(input$CPUE_info, {
    if("Taxatype"%in%colnames(plotdata2())){
      sendSweetAlert(session, title = "Plot info", text = tags$span(h2("Average abundance by year of each selected taxa by life stage combination"), h3("It is highly recommended to only select a few taxa for this plot"), tags$p("If desired, data can be subdivided into 3 salinity zones with the 'Salinity zones' switch. The borders of the salinity zones can be adjusted by dragging the bar to encompass your favored definition of the low salinity zone. Hover over the plot with your mouse to view data values.")),
                     type = "info",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    } else{
      sendSweetAlert(session, title = "Plot info", text = tags$span(h2("Community composition by year"), tags$p("If desired, data can be subdivided into 3 salinity zones with the 'Salinity zones' switch. The borders of the salinity zones can be adjusted by dragging the bar to encompass your favored definition of the low salinity zone. Hover over the plot with your mouse to view data values.")),
                     type = "info",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  #Map plot info
  observeEvent(input$Map_info, {
    if("Taxatype"%in%colnames(plotdata2())){
      sendSweetAlert(session, title = "Plot info", text = tags$span(h2("Mapped average yearly abundance of each selected taxa by life stage combination"), h3("It is highly recommended to only select a few taxa for this plot"), tags$p("Bubble area is scaled to CPUE. The map can be animated to loop through years by clicking the 'play' button on the right side of the year selector. Hover over the plot with your mouse to view data values. Adjustments to the bounds and zoom of the plot will not be reflected in the downloaded plot, sorry!")),
                     type = "info",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    } else{
      sendSweetAlert(session, title = "Plot info", text = tags$span(h2("Mapped yearly community composition of major taxonomic groups"), tags$p("Click the red gear to adjust map options. Life stages of each taxa are summed within each sample, so it is not recommended to combine life stages. If the map is too crowded with simialr colors, taxa can be deselected and they will be summed into the 'other' group."), p("The map can be animated to loop through years by clicking the 'play' button on the right side of the year selector. Click pie charts to view data values. Adjustments to the bounds and zoom of the plot will not be reflected in the downloaded plot, sorry!")),
                     type = "info",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  
  # Filter and process data -------------------------------------------------
  
  #Filter data to selected taxa.
  plotdata2 <- eventReactive(c(input$Run, input$Update_taxa), {
    if (length(input$Taxlifestage)>0 & input$Datatype=="Taxa"){
      filter(plotdata()$Data, Taxlifestage%in%input$Taxlifestage)
    } else {
      plotdata()$Data
    }
  }, ignoreInit=T)
  
  #Create data for tax map plot
  mapdatataxa <- reactive( {
    req(length(input$Maptax_size_class)>0)
    plotdata2()%>%
      filter(!is.na(Latitude) & !is.na(Longitude) & SizeClass%in%input$Maptax_size_class)%>%
      group_by(Taxlifestage, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE, na.rm=T))%>%
      ungroup()
  })
  
  #Create data for community map plot
  mapdatacom <- reactive( {
    
    if(is.null(input$Lifestage)){
      Lifestages<-unique(plotdata()$Data$Lifestage)[1]
    } else{
      Lifestages<-input$Lifestage
    }
    
    if(is.null(input$Taxagroups)){
      Taxagroups<-c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Mysida", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")
    } else{
      Taxagroups<-input$Taxagroups
    }
    
    if(is.null(input$Mapcom_size_class)){
      Mapcom_size_class<-c("Micro", "Meso", "Macro")
    } else{
      Mapcom_size_class<-input$Mapcom_size_class
    }
    
    plotdata2()%>%
      filter(!is.na(Latitude) & !is.na(Longitude) & Lifestage%in%Lifestages & SizeClass%in%Mapcom_size_class & !Undersampled)%>%
      mutate(Taxa=case_when(
        Order=="Calanoida" ~ "Calanoida",
        Order=="Cyclopoida" ~ "Cyclopoida",
        Order=="Harpacticoida" ~ "Harpacticoida",
        Class=="Copepoda" ~ "UnID copepods",
        Order=="Mysida" ~ "Mysida",
        Order=="Cladocera" ~ "Cladocera",
        Class=="Insecta" ~ "Insecta",
        Class=="Cirripedia" ~ "Cirripedia",
        Phylum=="Rotifera" ~ "Rotifera",
        TRUE ~ "Other"
      ))%>%
      mutate(Taxa=ifelse(Taxa%in%Taxagroups, Taxa, "Other"))%>%
      mutate(Taxa=factor(Taxa, levels=c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Mysida", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")))%>%
      {if(length(unique(.$Taxa))<3){
        mutate(., Taxa=Taxname)
      } else{
        .
      }}%>%
      group_by(Taxa, Year, Latitude, Longitude, SampleID)%>%
      summarise(CPUE=sum(CPUE, na.rm=T))%>%
      ungroup()%>%
      group_by(Taxa, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE, na.rm=T))%>%
      ungroup()%>%
      arrange(Taxa)%>%
      mutate(CPUE=round(CPUE))
    
  })
  
  #Pull out maximum CPUE for taxa map plot scale
  mapmax<- reactive( {
    mapdatataxa()%>%
      pull(CPUE)%>%
      max()
  })
  
  #Create labels for taxa map legend
  maplabels<-reactive({
    a<-mapmax()*c(1/3, 2/3, 1)
    b<-signif(a,2)
    return(list(Radii=(sqrt(b)/sqrt(mapmax()))*22, Labels=format(b, big.mark = ",")))
  })
  
  #Filter taxa map data to year using year slider
  filteredmapdatataxa<-reactive({
    mapdatataxa()%>%
      filter(Year==input$Year)
  })
  
  #Color palette for community map
  Compal<-reactive({
    colorFactor(brewer.pal(length(unique(mapdatacom()$Taxa)), "RdYlBu"), mapdatacom()%>%pull(Taxa))
  })
  
  #Filtered community map data to year
  filteredmapdatacom<-reactive({
    mapdatacom()%>%
      mutate(Taxa=as.character(Taxa))%>%
      filter(Year==input$Year)
  })
  
  #Create palette for taxa map plot
  Taxapal <- reactive({
    colorFactor(brewer.pal(7, "Dark2"), mapdatataxa()%>%pull(Taxlifestage))
  })
  
  
  # Calculate available choices for various UI inputs -----------------------
  
  #Calculate available taxlifestages for choosing taxlifestages
  output$select_Taxlifestage <- renderUI({
    
    choice_Taxlifestage <- reactive({
      if (input$Datatype=="Taxa"){
        plotdata()$Data%>%
          mutate(Group=case_when(
            Class=="Insecta" ~ "Insecta",
            Phylum=="Arthropoda" & !is.na(Order) ~ Order,
            TRUE ~ Phylum
          ))%>%
          select(Taxatype, Taxname, Lifestage, Taxlifestage, Group)%>%
          distinct()%>%
          arrange(Group, Taxatype, Taxname, Lifestage)%>%
          select(Taxlifestage, Group)%>%
          distinct()%>%
          split(x=.$Taxlifestage, f=.$Group)%>%
          lapply(., as.list)
      } else {
        NULL
      }
      
    })
    
    pickerInput('Taxlifestage', 'Select Taxa:', choices =choice_Taxlifestage(), multiple =T, selected=choice_Taxlifestage(), options=list(`live-search`=TRUE, `actions-box`=TRUE, size=10, title = "Select Taxa", `selected-text-format` = "count > 3")) 
    
  })
  
  
  #Calculate available size classes
  choice_SizeClass <- reactive({
    plotdata2()%>%
      pull(SizeClass)%>%
      unique()
  })
  
  #Size class input for CPUE plot
  output$select_SizeClass_CPUE <- renderUI({
    
    checkboxGroupButtons(
      inputId = "CPUE_size_class",
      label = "Size classes",
      choices = choice_SizeClass(),
      direction="horizontal",
      size = "sm",
      justified = TRUE,
      selected=choice_SizeClass()
    )
    
  })
  
  #Size class input for community map
  output$select_SizeClass_mapcom <- renderUI({
    
    checkboxGroupButtons(
      inputId = "Mapcom_size_class",
      label = "Size classes",
      choices = choice_SizeClass(),
      direction="horizontal",
      size = "sm",
      justified = TRUE,
      selected=choice_SizeClass()
    )
    
  })
  
  #Size class input for taxa map
  output$select_SizeClass_maptaxa <- renderUI({
    
    checkboxGroupButtons(
      inputId = "Maptax_size_class",
      label = "Size classes",
      choices = choice_SizeClass(),
      direction="horizontal",
      size = "sm",
      justified = TRUE,
      selected=choice_SizeClass()
    )
    
  })
  
  #Year choices and input for slider on map plots
  output$select_Year <- renderUI({
    
    choice_Year <- reactive({
      plotdata()$Data%>%
        pull(Year)%>%
        range()
    })
    
    sliderInput("Year",
                "Select year:",
                min = min(choice_Year()),  max=max(choice_Year()), value =  min(choice_Year()), step=1, round=T, sep="", 
                animate=animationOptions(interval=1000), width="100%")
    
  })
  
  #Lifestage choices for community map
  output$select_Lifestage <- renderUI({
    
    choice_Lifestage <- reactive({
      plotdata()$Data%>%
        pull(Lifestage)%>%
        unique()
    })
    
    pickerInput("Lifestage",
                "Select life stage:", choices=choice_Lifestage(), selected = choice_Lifestage()[1], multiple = T, options=list(dropupAuto=F))
    
  })
  
  #Create variable for data type that can be passed back to UI
  output$Datatype<-reactive( {
    ifelse("Taxatype"%in%colnames(plotdata()$Data), "Taxa", "Community")
  })
  
  
  # Create plots ------------------------------------------------------------
  
  #Sample plot
  Sampleplot <- reactive({
    
    #Set color palette
    myColors <- RColorBrewer::brewer.pal(5,"Set2")
    names(myColors) <- c("EMP", "FMWT", "TNS", "twentymm", "FRP")
    fillScale <- scale_fill_manual(name = "Source", values = myColors)
    
    #Create tooltip text template for mouse hovers
    str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                        "<tr><td>Source &nbsp</td><td>%s</td></tr>", 
                        "<tr><td>N &nbsp</td><td>%s</td></tr>")
    
    #Filter and process data
    data<-plotdata2()%>%
      mutate(Month=recode_factor(month(Date), "1"="January","2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December"))%>%
      filter(CPUE>0)%>%
      select(Source, Year, Month, SampleID)%>%
      distinct()
    
    data<-data%>%
      group_by(Source, Year, Month)%>%
      summarise(N_samples=n())%>%
      ungroup()%>%
      mutate(tooltip=sprintf(str_model, Year, Source, N_samples),
             ID=as.character(1:n()))%>%
      mutate(tooltip=paste0( "<table>", tooltip, "</table>" ))
    
    #Plot
    p<-ggplot(data, aes(x=Year, y = N_samples, fill=Source)) +
      geom_bar_interactive(stat="identity", aes(tooltip=tooltip, data_id = ID))+
      facet_wrap(~Month)+
      coord_cartesian(expand=0)+
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
      ylab("Number of plankton samples")+
      theme_bw()+
      theme(panel.grid=element_blank(), strip.background=element_blank(), text=element_text(size=14), panel.spacing.x = unit(15, "points"))+
      fillScale
    return(p)
  })
  
  #CPUE plot
  CPUEplot <- reactive({
    
    #Only run after it has processed the plot size class selection
    req(length(input$CPUE_size_class)>0)
    
    if("Taxatype"%in%colnames(plotdata2())){
      
      #Taxa plot
      
      #Number of colors needed
      colorCount <- plotdata2()%>%pull(Taxlifestage)%>%unique()%>%length()
      
      #Create text template for mouse hover tooltip
      if(input$Salzones){
        str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                            "<tr><td>Taxa &nbsp</td><td>%s</td></tr>",
                            "<tr><td>Salinity &nbsp</td><td>%s</td></tr>", 
                            "<tr><td>CPUE &nbsp</td><td>%s</td></tr>")
      } else{
        str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                            "<tr><td>Taxa &nbsp</td><td>%s</td></tr>",
                            "<tr><td>CPUE &nbsp</td><td>%s</td></tr>")
      }
      
      #Process data
      p<-plotdata2()%>%
        filter(SizeClass%in%input$CPUE_size_class)%>%
        {if(input$Salzones){
          filter(., !is.na(SalSurf))%>% 
            mutate(Salinity_zone=case_when(
              SalSurf < min(input$Lowsal) ~ "Freshwater",
              SalSurf > min(input$Lowsal) & SalSurf < max(input$Lowsal) ~ "Low salinity zone",
              SalSurf > max(input$Lowsal) ~ "High salinity zone"
            ))%>% 
            mutate(Salinity_zone=factor(Salinity_zone, levels=c("Freshwater", "Low salinity zone", "High salinity zone")))%>%
            group_by(Taxlifestage, Year, Salinity_zone)
        } else{
          group_by(., Taxlifestage, Year)
        }}%>%
        summarise(CPUE=mean(CPUE, na.rm = T))%>%
        ungroup()%>%
        {if(input$Salzones){
          mutate(., tooltip=sprintf(str_model, Year, Taxlifestage, Salinity_zone, round(CPUE)),
                 ID=as.character(1:n()))
        } else{
          mutate(., tooltip=sprintf(str_model, Year, Taxlifestage, round(CPUE)),
                 ID=as.character(1:n()))
        }}%>%
        mutate(tooltip=paste0( "<table>", tooltip, "</table>" ))%>%
        
        #Plot
        {if(input$Salzones){
          ggplot(., aes(x=Year, y=CPUE))+
            geom_line(size=1, aes(color=Taxlifestage, linetype=Salinity_zone))+
            geom_point_interactive(size=2, aes(color=Taxlifestage, shape=Salinity_zone, tooltip=tooltip, data_id = ID))+
            scale_linetype_manual(values=c(3,2,1))+
            coord_cartesian(expand=0)+
            scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
            scale_color_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(colorCount), name="Taxa and life stage", guide = guide_legend(ncol=1))+
            ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
            theme_bw()+
            theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=10))
        } else{
          ggplot(., aes(x=Year, y=CPUE))+
            geom_line(size=1, aes(color=Taxlifestage))+
            geom_point_interactive(size=2, aes(color=Taxlifestage, tooltip=tooltip, data_id = ID))+
            coord_cartesian(expand=0)+
            scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
            scale_color_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(colorCount), name="Taxa and life stage", guide = guide_legend(ncol=1))+
            ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
            theme_bw()+
            theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=10))
        }}
    }else{
      
      #CPUE plot
      
      #Number of colors needed
      colorCount <- plotdata2()%>%filter(!Undersampled)%>%pull(Taxlifestage)%>%unique()%>%length()
      
      #Create text template for mouse hover tooltip
      str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                          "<tr><td>Taxa &nbsp</td><td>%s</td></tr>",
                          "<tr><td>CPUE &nbsp</td><td>%s</td></tr>")
      set.seed(500)
      #Process data
      p<-plotdata2()%>%
        filter(SizeClass%in%input$CPUE_size_class & !Undersampled)%>%
        {if(input$Salzones){
          filter(., !is.na(SalSurf))%>% 
            mutate(Salinity_zone=case_when(
              SalSurf < min(input$Lowsal) ~ "Freshwater",
              SalSurf > min(input$Lowsal) & SalSurf < max(input$Lowsal) ~ "Low salinity zone",
              SalSurf > max(input$Lowsal) ~ "High salinity zone"
            ))%>%
            mutate(Salinity_zone=factor(Salinity_zone, levels=c("Freshwater", "Low salinity zone", "High salinity zone")))%>%
            group_by(Year,Phylum, Class, Order, Family, Genus, Species, Lifestage, Taxlifestage, Salinity_zone)
        } else{
          group_by(., Year,Phylum, Class, Order, Family, Genus, Species, Lifestage, Taxlifestage)
        }}%>%
        summarise(CPUE=mean(CPUE, na.rm=T))%>%
        ungroup()%>%
        mutate(tooltip=sprintf(str_model, Year, Taxlifestage, round(CPUE)),
               ID=as.character(1:n()))%>%
        mutate(tooltip=paste0( "<table>", tooltip, "</table>" ))%>%
        arrange(Phylum, Class, Order, Family, Genus, Species, Lifestage)%>%
        mutate(Taxlifestage=factor(Taxlifestage, unique(Taxlifestage)))%>%
        
        #Plot
        ggplot(aes(x=Year, y=CPUE))+
        geom_bar_interactive(stat="identity", color="white", size=0.01, aes(fill=Taxlifestage, tooltip=tooltip, data_id = ID))+
        coord_cartesian(expand=0)+
        scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
        {if(input$Salzones){
          facet_wrap(~Salinity_zone, nrow=1)
        }}+
        scale_fill_manual(values=sample(colorRampPalette(brewer.pal(12, "Set3"))(colorCount)), name="Taxa and life stage", guide = guide_legend(ncol=2))+
        ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
        theme_bw()+
        theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=6), legend.key.size = unit(10, "points"), strip.background=element_blank())
      set.seed(seed=NULL)
    }
    p
  })
  
  #Map plot bases
  Mapplot<-reactive({
    if("Taxatype"%in%colnames(plotdata2())){
      
      #Taxa map plot
      leaflet(mapdatataxa())%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        fitBounds(~min(Longitude, na.rm=T), ~min(Latitude, na.rm=T), ~max(Longitude, na.rm=T), ~max(Latitude, na.rm=T))%>%
        addLegendCustom(colors = rep("black",3),
                        labels = maplabels()$Labels,
                        sizes = maplabels()$Radii*2, position="topleft",
                        title="CPUE")%>%
        addLegend("topleft", pal = Taxapal(), values = ~Taxlifestage)
    } else{
      
      #Community map plot
      leaflet(mapdatacom())%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        fitBounds(~min(Longitude, na.rm=T), ~min(Latitude, na.rm=T), ~max(Longitude, na.rm=T), ~max(Latitude, na.rm=T))%>%
        addLegend("topright", pal = Compal(), values = ~Taxa, opacity=1)
    }
  })
  
  #Output plots (girafe makes plot interactive/hoverable)
  output$Sampleplot <- renderggiraph({
    p1<-girafe(code=print(Sampleplot()), width_svg = 10)
    girafe_options(p1, opts_toolbar(saveaspng = FALSE), opts_selection(type="none"))
  })
  
  output$CPUEplot <- renderggiraph({
    p2<-girafe(code=print(CPUEplot()), width_svg = 10)
    girafe_options(p2, opts_toolbar(saveaspng = FALSE), opts_selection(type="none"), opts_tooltip(offx = -100, offy = 40))
  })
  
  output$Mapplot <- renderLeaflet({
    Mapplot()
  })
  
  #Add data to map plots
  #leafletProxy lets us change layers in the map plot without re-rendering the whole thing
  
  #Taxa map plot
  observeEvent(c(input$Update_taxa, input$Year, input$Maptax_size_class), {
    req(input$Tab=="Map" & "Taxatype"%in%colnames(plotdata2()))
    pal <- Taxapal()
    map<-leafletProxy("Mapplot", session, data = filteredmapdatataxa(), deferUntilFlush=T)%>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~(sqrt(CPUE)/sqrt(mapmax()))*22, weight = 1, lng = ~Longitude, lat = ~Latitude,
                       fillColor = ~pal(Taxlifestage), color="black", fillOpacity = 0.7, label = ~paste0(Taxlifestage, ": ", round(CPUE)))
  }, ignoreNULL = T)
  
  #Community map plot
  observeEvent(c(input$Lifestage, input$Year, input$Taxagroups, input$Mapcom_size_class), {
    req(input$Tab=="Map" & !("Taxatype"%in%colnames(plotdata2())) & nrow(filteredmapdatacom())>1)
    filteredspreadmapdatacom<-filteredmapdatacom()%>%
      spread(key=Taxa, value = CPUE)
    filteredspreadmapdatacommat<-filteredspreadmapdatacom%>%select_at(vars(unique(filteredmapdatacom()$Taxa)))%>%as.matrix()
    map<-leafletProxy("Mapplot", session, data = filteredspreadmapdatacom, deferUntilFlush=T)%>%
      clearMinicharts() %>%
      addMinicharts(lng = filteredspreadmapdatacom$Longitude, lat = filteredspreadmapdatacom$Latitude,
                    type = "pie",
                    chartdata = filteredspreadmapdatacommat, 
                    colorPalette = Compal()(colnames(filteredspreadmapdatacommat)), legend=FALSE, transitionTime = 0, opacity=0.8)
  }, ignoreNULL = T)
  
  # Create map plots for download
  mapdown <- reactive({
    if("Taxatype"%in%colnames(plotdata2())){
      pal <- Taxapal()
      leaflet(filteredmapdatataxa())%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        addLegendCustom(colors = rep("black",3),
                        labels = maplabels()$Labels,
                        sizes = maplabels()$Radii*2, position="topleft",
                        title="CPUE")%>%
        addLegend("topleft", pal = Taxapal(), values = ~Taxlifestage)%>%
        addCircleMarkers(radius = ~(sqrt(CPUE)/sqrt(mapmax()))*22, weight = 1, lng = ~Longitude, lat = ~Latitude,
                         fillColor = ~pal(Taxlifestage), color="black", fillOpacity = 0.7, label = ~paste0(Taxlifestage, ": ", round(CPUE)))
    } else{
      filteredspreadmapdatacom<-filteredmapdatacom()%>%
        spread(key=Taxa, value = CPUE)
      filteredspreadmapdatacommat<-filteredspreadmapdatacom%>%select_at(vars(unique(filteredmapdatacom()$Taxa)))%>%as.matrix()
      leaflet(data = filteredspreadmapdatacom)%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        addLegend("topright", pal = Compal(), values = mapdatacom()$Taxa, opacity=1)%>%
        addMinicharts(lng = filteredspreadmapdatacom$Longitude, lat = filteredspreadmapdatacom$Latitude,
                      type = "pie",
                      chartdata = filteredspreadmapdatacommat, 
                      colorPalette = Compal()(colnames(filteredspreadmapdatacommat)), legend=FALSE, transitionTime = 0, opacity=0.8)
    }
  })
  
  
  # Download data and plots -------------------------------------------------
  
  
  
  #Show modal dialog to save plot when Saveplot button is clicked
  observeEvent(input$Saveplot, {
    showModal(Modalsave())
  })
  
  #Show modal dialog to save data when Download button is clicked
  observeEvent(input$Download, {
    showModal(ModalDownloadData())
  })
  
  #Download handler for plot saving
  output$Save <- downloadHandler(
    filename =function() {
      if (input$Tab=="Map"){
        paste0(input$Datatype, input$Tab, Sys.Date(), ".", input$Format2)
      } else {
        paste0(input$Datatype, input$Tab, Sys.Date(), ".", input$Format1)
      }
    },
    content = function(file) {
      if(input$Tab=="Samples"){
        ggsave(filename=file, plot=Sampleplot(), device=input$Format, width=input$Plotwidth, height=input$Plotheight, units="in")
      } else {
        if(input$Tab=="CPUE"){
          ggsave(filename=file, plot=CPUEplot(), device=input$Format, width=input$Plotwidth, height=input$Plotheight, units="in")
        } else {
          mapshot(mapdown(), file=file, zoom=3)
        }
      }
      #removeModal()
    }
  )
  
  #Download handler for data downloading
  output$Downloaddata <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      #Load extra environmental data
      zoopEnv<-readRDS("Data/zoopenvforzooper.Rds")%>%
        select(-Year, -Date, -SalSurf, -Latitude, -Longitude, -Source)
      
      data <- plotdata2()%>%
        left_join(zoopEnv,
                  by="SampleID")
      write_csv(data, file)
    })
  
  #Enable the datatype output to be passsed to input
  outputOptions(output, "Datatype", suspendWhenHidden = FALSE)
  
  # Close the app when the session completes
  if(!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
