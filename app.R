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
require(mapview)
require(shinyWidgets) 
require(leaflet.minicharts)

#Source Sam's function that gets the data from online
source("Zoop synthesizer function.R")

#Settings for the "data crunching" message. 
info_loading <- "Data crunching in progress..."
progress_color <- "#CD2626"
progress_background <- "#D3D3D3"

# Define UI for application that draws graphs and allows you to download data

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Zooplankton data synthesizer"),
  
  # check boxes where you choose data you want
  fluidRow(
    column(3,
      radioGroupButtons("Datatype", "Data Type:", choices = c("Taxa", "Community"), selected = "Community", individual = TRUE, checkIcon = list( yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
      awesomeCheckboxGroup("Sources",
                         "Sources:",
                         choices = c("Environmental Monitoring Program (EMP)" = "EMP", "Fish Restoration Program (FRP)" = "FRP", "Fall Midwater Trawl (FMWT)" = "FMWT", "Summer Townet Survey (TNS)" = "TNS", "20mm Survey (20mm)" = "20mm")),
      
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
                                   min = 37, max = 39, value = c(37,39), step=0.1)),
      conditionalPanel(condition = "input.Filters.includes('Longitude')",
                       sliderInput("Longrange",
                                   "Longitude Range:",
                                   min = -125, max = -119, value = c(-125, -119), step=0.1)),
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
      
      downloadBttn("Downloaddata", "Download data", style="bordered", color = "primary", size="sm"),
      br(), br(),
      actionBttn("Saveplot", "Download plot", icon = icon("chart-bar"), size="sm", style = "bordered", color="royal")
    ),
    
    # Display the plot
    column(9,
      tabsetPanel(type="tabs",
                  id = "Tab",
                  tabPanel("Samples", ggiraphOutput("Sampleplot")),
                  tabPanel("CPUE", br(),
                           splitLayout(materialSwitch(
                             inputId = "Salzones",
                             label = "Salinity zones?", 
                             value = TRUE, inline=T,
                             status = "primary"
                           ), 
                           conditionalPanel(condition = "input.Salzones", 
                                            sliderInput("Lowsal", "Low salinity zone", min=0, max=30, value=c(0.5,6), step=0.1, width="100%")), cellWidths = c("25%", "75%"), cellArgs = list(style = "padding: 2px")), 
                           ggiraphOutput("CPUEplot")),
                  tabPanel("Map", fluidRow(column(1,br(),br(), style='padding-left:40px', conditionalPanel(condition = "output.Datatype == 'Community'", dropdown(
                    
                    tags$h3("Map settings"),
                    
                    uiOutput("select_Lifestage"), 
                    pickerInput('Taxagroups', 'Select pie chart groups:', choices =c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other"), multiple =T, selected=c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")),
                    
                    circle = TRUE, status = "danger",
                    icon = icon("gear"), width = "300px",
                    
                    tooltip = tooltipOptions(title = "Click to see inputs!")
                    ))),
                           column(11, uiOutput("select_Year"))),
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
  tags$style(type="text/css", "#Save {color: white}"),
  
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
                       downloadBttn("Save", "Save", style="simple", color = "primary", size="sm"))
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
           Shiny=T, zoop=zoop)
  })
  
  #Filter data to selected taxa. Doing this plotdata in 2 steps makes it so 
  #Zooper function isn't re-run every time the user selects new taxa to plot
  plotdata2 <- eventReactive(c(input$Run, input$Update_taxa), {
    if (length(input$Taxlifestage)>0 & input$Datatype=="Taxa"){
      filter(plotdata(), Taxlifestage%in%input$Taxlifestage)
    } else {
      plotdata()
    }
  }, ignoreInit=T)
  
  #Create data for map plot
  mapdatataxa <- reactive( {
    plotdata2()%>%
      filter(Volume>1 & !is.na(Latitude) & !is.na(Longitude))%>%
      lazy_dt()%>%
      group_by(Taxlifestage, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE))%>%
      as_tibble()%>%
      ungroup()
  })
  
  #Create data for community map plot
  mapdatacom <- reactive( {
    
    if(is.null(input$Lifestage)){
      Lifestages<-unique(plotdata()$Lifestage)
    } else{
      Lifestages<-input$Lifestage
    }
    
    if(is.null(input$Taxagroups)){
      Taxagroups<-c("Calanoida", "Cyclopoida", "Harpacticoida", "UnID copepods", "Cladocera", "Rotifera", "Cirripedia", "Insecta", "Other")
    } else{
      Taxagroups<-input$Taxagroups
    }
    
    plotdata2()%>%
      filter(Volume>1 & !is.na(Latitude) & !is.na(Longitude) & Lifestage%in%Lifestages)%>%
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
      group_by(Taxa, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE))%>%
      as_tibble()%>%
      ungroup()%>%
      arrange(Taxa)%>%
      mutate(Taxa=as.character(Taxa))
  })
  
  #Pull out maximum CPUE for map plot scale
  mapmax<- reactive( {
    mapdatataxa()%>%
      pull(CPUE)%>%
      max()
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
        plotdata()%>%
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
      plotdata()%>%
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
      plotdata()%>%
        pull(Lifestage)%>%
        unique()
    })
    
    pickerInput("Lifestage",
                "Select life stage:", choices=choice_Lifestage(), selected = choice_Lifestage(), multiple = T, options=list(dropupAuto=F))
    
  })
  
  #Create output so app knows what type of data has actually been created. Used for the select taxa input
  output$Datatype<-reactive( {
    ifelse("Taxatype"%in%colnames(plotdata()), "Taxa", "Community")
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
      mutate(Month=recode_factor(Month, "1"="January","2"="February", "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", "9"="September", "10"="October", "11"="November", "12"="December"))%>%
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
        filter(Volume>1)%>% # *****Currently removing data with very low sample volumes, should change this later*****
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
      str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                          "<tr><td>Taxa &nbsp</td><td>%s</td></tr>",
                          "<tr><td>CPUE &nbsp</td><td>%s</td></tr>")
      plotdata2()%>%
        filter(Volume>1)%>%# *****Currently removing data with very low sample volumes, should change this later*****
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
        summarise(CPUE=mean(CPUE))%>%
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
        scale_fill_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(colorCount), name="Taxa and life stage", guide = guide_legend(ncol=2))+
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
        addLegend("bottomright", pal = Taxapal(), values = ~Taxlifestage)%>%
        addLabelOnlyMarkers(lng = ~c(-122.222+0.006325176, -122.222+0.0253007, -122.222+0.05692658), lat = ~c(38.025, 38, 37.935),
                            label=~as.character(round(c(((((((sqrt(5000))/3))^2)/5000)*sqrt(mapmax()))^2, ((((((sqrt(5000))/3)*2)^2)/5000)*sqrt(mapmax()))^2, mapmax()))),
                            labelOptions = list(permanent=TRUE, textOnly=T, direction="right"), options = list(title="CPUE"))
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
      clearShapes() %>%
      addCircles(radius = ~(sqrt(CPUE)/sqrt(mapmax()))*5000, weight = 1, lng = ~Longitude, lat = ~Latitude,
                 fillColor = ~pal(Taxlifestage), color="black", fillOpacity = 0.7, label = ~paste0(Taxlifestage, ": ", round(CPUE)))%>%
      addCircles(radius = ~c(((sqrt(5000))/3)^2, (((sqrt(5000))/3)*2)^2, 5000), weight = 1, lng = ~c(-122.222, -122.222, -122.222), lat = ~c(38.025, 38, 37.935), color="black", fillColor = "white", fillOpacity = 100)
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
                    colorPalette = brewer.pal(9, "RdYlBu"), transitionTime = 0, opacity=0.8)
  }, ignoreNULL = T)
  
  
  #Show modal dialog to save plot when Saveplot button is clicked
  observeEvent(input$Saveplot, {
    showModal(Modalsave())
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
            pal <- Taxapal()
            map<-leaflet(mapdatataxa())%>%
              addProviderTiles("Esri.WorldGrayCanvas")%>%
              fitBounds(~min(Longitude, na.rm=T), ~min(Latitude, na.rm=T), ~max(Longitude, na.rm=T), ~max(Latitude, na.rm=T))%>%
              addLegend("bottomright", pal = pal, values = ~Taxlifestage)%>%
              addCircles(data=filteredmapdatataxa(), radius = ~((CPUE)/mapmax())*10000, weight = 1, lng = ~Longitude, lat = ~Latitude,
                         fillColor = ~pal(Taxlifestage), color="black", fillOpacity = 0.7, label = ~paste0(Taxlifestage, ": ", round(CPUE)))
            mapshot(map, file=file)
          }
        }
        removeModal()
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
                     Shiny=T)
      data <-if (length(input$Taxlifestage)>0 & input$Datatype=="Taxa"){
        filter(data, Taxlifestage%in%input$Taxlifestage)
      } else {
        data
      }
      write.csv(data, file)
    })
  
  #Enable the datatype output to be passsed to input
  outputOptions(output, "Datatype", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
