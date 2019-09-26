# This is a Shiny web application designed to allow users to select which zooplankton data they would
# like to combine, visualize the data, and download data.

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(RColorBrewer)
require(shiny)
require(tidyverse)
require(readxl)

#Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
require(dtplyr)
require(lubridate)
require(ggiraph)
require(leaflet)
require(webshot)
require(mapview)
require(shinyWidgets) 
require(leaflet.minicharts)

#required for users to download map plots from shinyapps.io
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }

#Source Sam's function that gets the data from online
source("Zoop synthesizer function.R")

#Settings for the "data crunching" message. 
info_loading <- "Data crunching in progress..."
progress_color <- "#CD2626"
progress_background <- "#D3D3D3"

addLegendCustom <- function(map, colors, labels, sizes, position, opacity = 0.5, title){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px", "; border-radius: 50%")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, 
                   opacity = opacity, position=position, title=title))
}

# Define UI for application that draws graphs and allows you to download data

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Zooplankton data synthesizer"),
  
  # check boxes where you choose data you want
  fluidRow(
    column(3,
           actionBttn("Instructions", "Instructions", style="simple", color="primary", icon=icon("question-circle")),
           br(), br(),
           radioGroupButtons("Datatype", "Data Type:", choices = c("Taxa", "Community"), selected = "Community", individual = TRUE, checkIcon = list( yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
           awesomeCheckboxGroup("Sources",
                                "Sources:",
                                choices = c("Environmental Monitoring Program (EMP)" = "EMP", 
                                  "Fish Restoration Program (FRP)" = "FRP", "Fall Midwater Trawl (FMWT)" = "FMWT", "Summer Townet Survey (TNS)" = "TNS", "20mm Survey (20mm)" = "20mm")),
           
           #Allow users to select which filters they would like to use, then those filter options will appear.
           
           awesomeCheckboxGroup("Filters",
                                "Filters:",
                                choices = c("Dates", "Months", "Surface salinity", "Latitude", "Longitude")),
           conditionalPanel(condition = "input.Filters.includes('Dates')",
                            dateRangeInput("Daterange", label = "Date range", 
                                           start = "1972-01-01", end = "2018-12-31", startview = "year")),
           conditionalPanel(condition = "input.Filters.includes('Months')",
                            pickerInput("Months", "Months:", choices=c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12), selected = 1, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3"))),
           conditionalPanel(condition = "input.Filters.includes('Surface salinity')",
                            sliderInput("SalSurfrange",
                                        "Surface salinity:",
                                        min = 0, max = 32, value = c(0,7), step=0.1)),
           conditionalPanel(condition = "input.Filters.includes('Latitude')",
                            sliderInput("Latrange",
                                        "Latitude Range",
                                        min = 37.8, max = 38.6, value = c(37.8, 38.6), step=0.05)),
           conditionalPanel(condition = "input.Filters.includes('Longitude')",
                            sliderInput("Longrange",
                                        "Longitude Range:",
                                        min = -122.5, max = -121.3, value = c(-122.5, -121.3), step=0.05)),
           #radioButtons("Plottype", "Plot Type", choices = c("Samples", "CPUE", "Map"), selected = "Samples",
           #             inline = TRUE),
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
           #Allow users to download data
           
           actionBttn("Download", "Download data", style="bordered", color = "primary", size="sm", icon=icon("file-download")),
           br(), br(),
           actionBttn("Saveplot", "Download plot", icon = icon("chart-bar"), size="sm", style = "bordered", color="royal"),
           br(), br(),
           conditionalPanel(condition = "output.Datatype == 'Taxa' || output.Datatype == 'Community'", 
                            actionBttn('Disclaimer',"Data disclaimer", style="jelly", color = "danger", icon = icon("exclamation-triangle")))
    ),
    
    # Display the plot
    column(9,
           tabsetPanel(type="tabs",
                       id = "Tab",
                       tabPanel("Samples", br(), 
                                fluidRow(actionBttn("Sample_info", label = NULL, style="material-circle", 
                                                          color="primary", icon=icon("question"))), 
                                fluidRow(ggiraphOutput("Sampleplot"))),
                       tabPanel("CPUE", br(),
                                fluidRow(column(2, fluidRow(actionBttn("CPUE_info", label = NULL, style="material-circle", 
                                                       color="primary", icon=icon("question"))), br(),
                                                fluidRow(materialSwitch(
                                              inputId = "Salzones",
                                              label = "Salinity zones?", 
                                              value = TRUE, inline=F,
                                              status = "primary"
                                            ))), 
                                            column(10, conditionalPanel(condition = "input.Salzones", 
                                                             sliderInput("Lowsal", "Low salinity zone", min=0, max=30, value=c(0.5,6), step=0.1, width="100%")))), 
                                fluidRow(column(12, ggiraphOutput("CPUEplot")))),
                       tabPanel("Map", fluidRow(column(1, offset = 0, style='padding:0px;', br() ,
                                                       actionBttn("Map_info", label = NULL, style="material-circle", 
                                                                                  color="primary", icon=icon("question"))), 
                                                       column(1, offset = 0, style='padding:0px;', br(), conditionalPanel(condition = "output.Datatype == 'Community'", dropdown(
                         
                         tags$h3("Map settings"),
                         
                         uiOutput("select_Lifestage"), 
                         pickerInput('Taxagroups', 'Select taxa:', choices =c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other"), multiple =T, selected=c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")),
                         
                         circle = TRUE, status = "danger",
                         icon = icon("gear"), width = "300px",
                         
                         tooltip = tooltipOptions(title = "Click to see inputs!")
                       ))),
                       column(10, uiOutput("select_Year"))),
                       fluidRow(leafletOutput("Mapplot", width = "100%", height = "100%")))
                       
           )
    )),
  
  # This is just to display the "data crunching" message. 
  
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
  
  #tags$style(type="text/css", "#Downloaddata {color: white}"),
  #tags$style(type="text/css", "#Save {color: white}"),
  tags$style(type="text/css", "#Disclaimer {color: white}"),
  
  #Make leaflet legend shapes circles
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}",
  #            ".leaflet .legend i{
  #                    border-radius: 50%;
  #                   width: 10px;
  #                  height: 10px;
  #                 margin-top: 4px;
  #                }
  #               "),
  
  #Specify plot sizes
  tags$head(tags$style("#Sampleplot{height:100vh !important;}")),
  tags$head(tags$style("#CPUEplot{height:80vh !important;}")),
  tags$head(tags$style("#Mapplot{height:80vh !important;}"))
)

# Define server logic required to process data, draw plots, and dowload data
server <- function(input, output, session) {
  
  #Add function for Modal dialog (popup window) to save plots
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
  
  #Using eventReactive so app only updates when "Run" button is clicked, letting you check all the boxes you want before running the app
  plotdata <- eventReactive(input$Run, {
    
    #select the data the user wants
    
    Zooper(Data = input$Datatype, 
           Sources = input$Sources, 
           Daterange = ifelse(rep("Dates"%in%input$Filters, 2), input$Daterange, c(NA, NA)),
           Months = ifelse(rep("Months"%in%input$Filters, length(input$Months)), as.integer(input$Months), rep(NA, length(input$Months))),  
           SalSurfrange = ifelse(rep("Surface salinity"%in%input$Filters, 2), input$SalSurfrange, c(NA, NA)),
           Latrange = ifelse(rep("Latitude"%in%input$Filters, 2), input$Latrange, c(NA, NA)), 
           Longrange = ifelse(rep("Longitude"%in%input$Filters, 2), input$Longrange, c(NA, NA)), 
           Shiny=T, AllEnv=F)
  })
  
  #Disclaimer
  observeEvent(input$Disclaimer, {
    sendSweetAlert(session, title = "Data disclaimer", text = paste0("Data are subject to the following caveats:\n\n", plotdata()$Caveats),
                   type = "info",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  #Instructions
  observeEvent(input$Instructions, {
    sendSweetAlert(session, title = "Instructions", text = tags$span(tags$p("This app combines any combination of the zoop datasets and calculates least common denominator taxa to facilitate
 comparisons across datasets with differing levels of taxonomic resolution."),  
                                                                     tags$p("Option 'Data type' allows you to choose a final output dataset for either Community or Taxa-specific  analyses. If you want all available data on given Taxa, use 'Taxa' If you want to conduct a community analysis, use 'Community.'"),
                                                                     tags$p("Briefly, 'Community' optimizes for community-level analyses by taking all taxa by life stage combinations that are not measured in every input dataset, and summing them up taxonomic levels to the lowest taxonomic level they belong to that is covered by all datasets. Remaining Taxa x life stage combinations that are not covered in all datasets up to the phylum level (usually something like Annelida or Nematoda or Insect Pupae) are removed from the final dataset."), 
                                                                     tags$p("'Taxa' optimizes for the Taxa-level user by maintaining all data at the original taxonomic level. To facilitate comparions across datasets, this option also sums data into general categories that are comparable across all datasets (e.g., Calanoida_all"), 
                                                                     "------------------------------------------",
                                                                     tags$p(tags$b("App created and maintained by Sam Bashevkin with help from the IEP zooplankton synthesis team. Please email sam.bashevkin@deltacouncil.ca.gov with any questions or comments"))),
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
      sendSweetAlert(session, title = "Plot info", text = tags$span(h2("Mapped yearly community composition of major taxonomic groups"), tags$p("Click the red gear to adjust map options. Life stages of each taxa are summed within each sample, so it is recommended to combine life stages. If the map is too crowded with simialr colors, taxa can be deselected and they will be summed into the 'other' group."), p("The map can be animated to loop through years by clicking the 'play' button on the right side of the year selector. Click pie charts to view data values. Adjustments to the bounds and zoom of the plot will not be reflected in the downloaded plot, sorry!")),
                     type = "info",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  #Filter data to selected taxa. Doing this plotdata in 2 steps makes it so 
  #Zooper function isn't re-run every time the user selects new taxa to plot
  plotdata2 <- eventReactive(c(input$Run, input$Update_taxa), {
    if (length(input$Taxlifestage)>0 & input$Datatype=="Taxa"){
      filter(plotdata()$Data, Taxlifestage%in%input$Taxlifestage)
    } else {
      plotdata()$Data
    }
  }, ignoreInit=T)
  
  #Create data for map plot
  mapdatataxa <- reactive( {
    plotdata2()%>%
      filter(!is.na(Latitude) & !is.na(Longitude))%>%
      lazy_dt()%>%
      group_by(Taxlifestage, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE, na.rm=T))%>%
      as_tibble()%>%
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
      Taxagroups<-c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")
    } else{
      Taxagroups<-input$Taxagroups
    }
    
    plotdata2()%>%
      filter(!is.na(Latitude) & !is.na(Longitude) & Lifestage%in%Lifestages)%>%
      mutate(Taxa=case_when(
        Order=="Calanoida" ~ "Calanoida",
        Order=="Cyclopoida" ~ "Cyclopoida",
        Order=="Harpacticoida" ~ "Harpacticoida",
        Class=="Copepoda" ~ "UnID copepods",
        Order=="Cladocera" ~ "Cladocera",
        Class=="Insecta" ~ "Insecta",
        Class=="Cirripedia" ~ "Cirripedia",
        Phylum=="Rotifera" ~ "Rotifera",
        TRUE ~ "Other"
      ))%>%
      mutate(Taxa=ifelse(Taxa%in%Taxagroups, Taxa, "Other"))%>%
      mutate(Taxa=factor(Taxa, levels=c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")))%>%
      lazy_dt()%>%
      group_by(Taxa, Year, Latitude, Longitude, SampleID)%>%
      summarise(CPUE=sum(CPUE, na.rm=T))%>%
      ungroup()%>%
      group_by(Taxa, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE, na.rm=T))%>%
      as_tibble()%>%
      ungroup()%>%
      arrange(Taxa)%>%
      mutate(Taxa=as.character(Taxa),
             CPUE=round(CPUE))
  })
  
  #Pull out maximum CPUE for map plot scale
  mapmax<- reactive( {
    mapdatataxa()%>%
      pull(CPUE)%>%
      max()
  })
  
  #Create labels for map legend
  maplabels<-reactive({
    a<-mapmax()*c(1/3, 2/3, 1)
    b<-signif(a,2)
    return(list(Radii=(sqrt(b)/sqrt(mapmax()))*22, Labels=format(b, big.mark = ",")))
  })
  
  #Create Taxa map data filtered to year
  filteredmapdatataxa<-reactive({
    mapdatataxa()%>%
      filter(Year==input$Year)
  })
  
  #Create Community map data filtered to year
  filteredmapdatacom<-reactive({
    mapdatacom()%>%
      filter(Year==input$Year)
  })
  
  #Create palette for taxa map plot
  Taxapal <- reactive({
    colorFactor(brewer.pal(7, "Dark2"), mapdatataxa()%>%pull(Taxlifestage))
  })
  
  #Update Taxlifestage choices based on data for the user input choices
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
  
  #Update year choices based on prior user input choices
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
  
  #Update Lifestage choices for community map based on prior user input choices
  output$select_Lifestage <- renderUI({
    
    choice_Lifestage <- reactive({
      plotdata()$Data%>%
        pull(Lifestage)%>%
        unique()
    })
    
    pickerInput("Lifestage",
                "Select life stage:", choices=choice_Lifestage(), selected = choice_Lifestage()[1], multiple = T, options=list(dropupAuto=F))
    
  })
  
  #Create output so app knows what type of data has actually been created. Used for the select taxa input
  output$Datatype<-reactive( {
    ifelse("Taxatype"%in%colnames(plotdata()$Data), "Taxa", "Community")
  })
  
  #Make reactive functions for each plot type
  
  Sampleplot <- reactive({
    myColors <- RColorBrewer::brewer.pal(5,"Set2")
    names(myColors) <- c("EMP", "FMWT", "TNS", "20mm", "FRP")
    fillScale <- scale_fill_manual(name = "Source", values = myColors)
    
    str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                        "<tr><td>Source &nbsp</td><td>%s</td></tr>", 
                        "<tr><td>N &nbsp</td><td>%s</td></tr>")
    
    plotdata2()%>%
      #mutate(Season=case_when(
      #  Month%in%c(1,2,3) ~ "Winter",
      #  Month%in%c(4,5,6) ~ "Spring",
      #  Month%in%c(7,8,9) ~ "Summer",
      #  Month%in%c(10,11,12) ~ "Fall"
      #))%>%
      #mutate(Season=factor(Season, levels=c("Fall", "Winter", "Spring", "Summer")))%>%
      mutate(Month=recode_factor(month(Date), "1"="January","2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December"))%>%
      select(Source, Year, Month, SampleID)%>%
      distinct()%>%
      group_by(Source, Year, Month)%>%
      summarise(N_samples=n())%>%
      ungroup()%>%
      mutate(tooltip=sprintf(str_model, Year, Source, N_samples),
             ID=as.character(1:n()))%>%
      mutate(tooltip=paste0( "<table>", tooltip, "</table>" ))%>%
      ggplot(aes(x=Year, y = N_samples, fill=Source)) +
      geom_bar_interactive(stat="identity", aes(tooltip=tooltip, data_id = ID))+
      facet_wrap(~Month)+
      coord_cartesian(expand=0)+
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
      ylab("Number of plankton samples")+
      theme_bw()+
      theme(panel.grid=element_blank(), strip.background=element_blank(), text=element_text(size=14), panel.spacing.x = unit(15, "points"))+
      fillScale
  })
  
  CPUEplot <- reactive({
    colorCount <- plotdata2()%>%pull(Taxlifestage)%>%unique()%>%length()
    if("Taxatype"%in%colnames(plotdata2())){
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
      
      plotdata2()%>%
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
        {if(input$Salzones){
          ggplot(., aes(x=Year, y=CPUE))+
            geom_line(size=1, aes(color=Taxlifestage, linetype=Salinity_zone))+
            geom_point_interactive(size=2, aes(color=Taxlifestage, shape=Salinity_zone, tooltip=tooltip, data_id = ID))+
            scale_linetype_manual(values=c(3,2,1))+
            coord_cartesian(expand=0)+
            scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
            scale_color_manual(values=rep(brewer.pal(11, "RdYlBu"), ceiling(colorCount/11)), name="Taxa and life stage", guide = guide_legend(ncol=1))+
            ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
            theme_bw()+
            theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=10))
        } else{
          ggplot(., aes(x=Year, y=CPUE))+
            geom_line(size=1, aes(color=Taxlifestage))+
            geom_point_interactive(size=2, aes(color=Taxlifestage, tooltip=tooltip, data_id = ID))+
            coord_cartesian(expand=0)+
            scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
            scale_color_manual(values=rep(brewer.pal(11, "RdYlBu"), ceiling(colorCount/11)), name="Taxa and life stage", guide = guide_legend(ncol=1))+
            ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
            theme_bw()+
            theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=10))
        }}
    }else{
      str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                          "<tr><td>Taxa &nbsp</td><td>%s</td></tr>",
                          "<tr><td>CPUE &nbsp</td><td>%s</td></tr>")
      plotdata2()%>%
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
        ggplot(aes(x=Year, y=CPUE))+
        geom_bar_interactive(stat="identity", color="white", size=0.01, aes(fill=Taxlifestage, tooltip=tooltip, data_id = ID))+
        coord_cartesian(expand=0)+
        scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), max(x)), n=4))), expand=c(0,0))+
        {if(input$Salzones){
          facet_wrap(~Salinity_zone, nrow=1)
        }}+
        scale_fill_manual(values=rep(brewer.pal(11, "RdYlBu"), ceiling(colorCount/11)), name="Taxa and life stage", guide = guide_legend(ncol=2))+
        ylab(bquote(Average~CPUE~"("*Catch*"/"*m^3*")"))+
        theme_bw()+
        theme(panel.grid=element_blank(), text=element_text(size=14), legend.text = element_text(size=6), legend.key.size = unit(10, "points"), strip.background=element_blank())
    }
  })
  
  Mapplot<-reactive({
    if("Taxatype"%in%colnames(plotdata2())){
      pal <- Taxapal()
      leaflet(mapdatataxa())%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        fitBounds(~min(Longitude, na.rm=T), ~min(Latitude, na.rm=T), ~max(Longitude, na.rm=T), ~max(Latitude, na.rm=T))%>%
        addLegendCustom(colors = rep("black",3),
                        labels = maplabels()$Labels,
                        sizes = maplabels()$Radii*2, position="topleft",
                        title="CPUE")%>%
        addLegend("topleft", pal = Taxapal(), values = ~Taxlifestage)
    } else{
      leaflet(plotdata2()%>%filter(!is.na(Latitude) & !is.na(Longitude)))%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        fitBounds(~min(Longitude, na.rm=T), ~min(Latitude, na.rm=T), ~max(Longitude, na.rm=T), ~max(Latitude, na.rm=T))
    }
  })
  
  #Create plot outputs (girafe makes plot interactive/hoverable)
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
  
  #leafletProxy lets us change layers in the map plot without re-rendering the whole thing
  observeEvent(c(input$Update_taxa, input$Year), {
    req(input$Tab=="Map" & "Taxatype"%in%colnames(plotdata2()))
    pal <- Taxapal()
    map<-leafletProxy("Mapplot", session, data = filteredmapdatataxa(), deferUntilFlush=T)%>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~(sqrt(CPUE)/sqrt(mapmax()))*22, weight = 1, lng = ~Longitude, lat = ~Latitude,
                       fillColor = ~pal(Taxlifestage), color="black", fillOpacity = 0.7, label = ~paste0(Taxlifestage, ": ", round(CPUE)))
  }, ignoreNULL = T)
  
  observeEvent(c(input$Lifestage, input$Year, input$Taxagroups), {
    req(input$Tab=="Map" & !("Taxatype"%in%colnames(plotdata2())) & nrow(filteredmapdatacom())>1)
    filteredspreadmapdatacom<-filteredmapdatacom()%>%
      spread(key=Taxa, value = CPUE)
    map<-leafletProxy("Mapplot", session, data = filteredspreadmapdatacom, deferUntilFlush=T)%>%
      clearMinicharts() %>%
      addMinicharts(lng = filteredspreadmapdatacom$Longitude, lat = filteredspreadmapdatacom$Latitude,
                    type = "pie",
                    chartdata = filteredspreadmapdatacom%>%select_at(vars(unique(filteredmapdatacom()$Taxa)))%>%as.matrix(), 
                    colorPalette = brewer.pal(length(unique(mapdatacom()$Taxa)), "RdYlBu"), transitionTime = 0, opacity=0.8)
  }, ignoreNULL = T)
  
  
  # map that will be downloaded
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
                         fillColor = ~pal(Taxlifestage), color="black", fillOpacity = 0.7, label = ~paste0(Taxlifestage, ": ", round(CPUE)))#%>% 
      #setView(lng = input$Mapplot_center$lng,  lat = input$Mapplot_center$lat, zoom = input$Mapplot_zoom) ##Not working, doesn't exactly match the map you see
    } else{
      filteredspreadmapdatacom<-filteredmapdatacom()%>%
        spread(key=Taxa, value = CPUE)
      
      leaflet(data = filteredspreadmapdatacom)%>%
        addProviderTiles("Esri.WorldGrayCanvas")%>%
        addMinicharts(lng = filteredspreadmapdatacom$Longitude, lat = filteredspreadmapdatacom$Latitude,
                      type = "pie",
                      chartdata = filteredspreadmapdatacom%>%select_at(vars(unique(filteredmapdatacom()$Taxa)))%>%as.matrix(), 
                      colorPalette = brewer.pal(length(unique(mapdatacom()$Taxa)), "RdYlBu"), transitionTime = 0, opacity=0.8)#%>% 
      #setView(lng = input$Mapplot_center$lng,  lat = input$Mapplot_center$lat, zoom = input$Mapplot_zoom) ##Not working, doesn't exactly match the map you see
    }
  })
  
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
          mapshot(mapdown(), file=file)
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
      data <- Zooper(Data = input$Datatype, 
                     Sources = input$Sources, 
                     Daterange = ifelse(rep("Dates"%in%input$Filters, 2), input$Daterange, c(NA, NA)),
                     Months = ifelse(rep("Months"%in%input$Filters, length(input$Months)), as.integer(input$Months), rep(NA, length(input$Months))),  
                     SalSurfrange = ifelse(rep("Surface salinity"%in%input$Filters, 2), input$SalSurfrange, c(NA, NA)),
                     Latrange = ifelse(rep("Latitude"%in%input$Filters, 2), input$Latrange, c(NA, NA)), 
                     Longrange = ifelse(rep("Longitude"%in%input$Filters, 2), input$Longrange, c(NA, NA)), 
                     Shiny=T, AllEnv=T)
      data <-if (length(input$Taxlifestage)>0 & input$Datatype=="Taxa"){
        filter(data$Data, Taxlifestage%in%input$Taxlifestage)
      } else {
        data$Data
      }
      write_csv(data, file)
    })
  
  #Enable the datatype output to be passsed to input
  outputOptions(output, "Datatype", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
