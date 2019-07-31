# This is a Shiny web application designed to allow users to select which zooplankton data they would
# like to combine, visualize the data, and download data.

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(sf)
require(RColorBrewer)
require(shiny)
require(tidyverse)
require(readxl)

#Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
require(dtplyr)
require(lubridate)
require(plotly)

#Source Sam's function that gets the data from online
source("Zoop synthesizer function.R")

mapDelta<-st_read("Data/DeltaShapefile")
pmap<-ggplot() + 
  geom_sf(data=mapDelta, color = "dodgerblue1", fill = "dodgerblue1") + 
  coord_sf()+
  theme_bw()+
  theme(panel.grid=element_blank())

#Settings for the "data crunching" message. 
info_loading <- "Data crunching in progress..."
progress_color <- "#CD2626"
progress_background <- "#D3D3D3"

# Define UI for application that draws graphs and allows you to download data

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Zooplankton"),
  
  # check boxes where you choose data you want
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("Datatype", "Data Type", choices = c("Taxa", "Community"), selected = "Community",
                   inline = TRUE),
      checkboxGroupInput("Sources",
                         "Sources:",
                         choices = c("Environmental monitoring program" = "EMP", "Fish restoration program" = "FRP", "Fall midwater trawl" = "FMWT", "Summer townet survey" = "TNS", "20mm survey" = "20mm")),
      
      #Allow users to select which filters they would like to use, then those filter options will appear.
      
      checkboxGroupInput("Filters",
                         "Filters:",
                         choices = c("Dates", "Months", "Surface_salinity", "Latitude", "Longitude")),
      conditionalPanel(condition = "input.Filters.includes('Dates')",
                       dateRangeInput("Daterange", label = "Date range", 
                                      start = "1972-01-01", end = "2018-12-31", startview = "year")),
      conditionalPanel(condition = "input.Filters.includes('Months')",
                       selectInput("Months", "Months:", choices=c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12), selected = 1, multiple = T)),
      conditionalPanel(condition = "input.Filters.includes('Surface_salinity')",
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
      actionButton("Run", "Run"),
      
      #Allows users to select taxa, but only in Taxa mode, and updates Taxa choices based on 
      #the data selections the user has made (after hitting Run). This is conditional on an 
      #output so that it only appears when the app has actually created Taxa data, rather
      #than when the user has checked the taxa box but not yet clicked run. It prevents the
      #user from accidentally filtering taxa in the "Community" mode.
      
      conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                       uiOutput("select_Taxlifestage")), 
      conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                       actionButton("Update_taxa", "Update taxa")),
      
      #Allow users to download data
      
      downloadButton("download", "Download")
    ),
    
    # Display the plot
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Samples", plotlyOutput("Sampleplot", height="800px")),
                  tabPanel("CPUE", plotlyOutput("CPUEplot", height="800px")),
                  tabPanel("Map", plotOutput("Mapplot", height="600px"), 
                                            uiOutput("select_Year"))
        
      )
    ),
    position = "left",
    fluid = T
  ),
  
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
  tags$style(type="text/css", ".recalculating {opacity: 1.0;}")
)

# Define server logic required to process data, draw plots, and dowload data
server <- function(input, output, session) {
  
  #observeEvent(c(input$Plottype, input$Taxlifestage), {
  #  if(input$Plottype=="CPUE" & length(input$Taxlifestage)>20){
  #    showNotification("We recommend you select fewer taxa for this plot (Make sure to click `Update taxa` after making your selection).", type="warning")
  #  }
  #})
  
  #Using eventReactive so app only updates when "Run" button is clicked, letting you check all the boxes you want before running the app
  plotdata <- eventReactive(input$Run, {
    
    #select the data the user wants
    
    Zooper(Data = input$Datatype, 
           Sources = input$Sources, 
           Daterange = ifelse(rep("Dates"%in%input$Filters, 2), input$Daterange, c(NA, NA)),
           Months = ifelse(rep("Months"%in%input$Filters, length(input$Months)), as.integer(input$Months), rep(NA, length(input$Months))),  
           SalSurfrange = ifelse(rep("Surface_salinity"%in%input$Filters, 2), input$SalSurfrange, c(NA, NA)),
           Latrange = ifelse(rep("Latitude"%in%input$Filters, 2), input$Latrange, c(NA, NA)), 
           Longrange = ifelse(rep("Longitude"%in%input$Filters, 2), input$Longrange, c(NA, NA)), 
           Shiny=T)
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
  
  mapdata <- reactive( {
    plotdata2()%>%
      filter(Volume>1)%>%
      lazy_dt()%>%
      group_by(Taxlifestage, Year, Latitude, Longitude)%>%
      summarise(CPUE=mean(CPUE))%>%
      as_tibble()%>%
      ungroup()
  })
  
  maprange<- reactive( {
    mapdata()%>%
      pull(CPUE)%>%
      range()
  })
  
  #Update Taxlifestage chocies based on data for the user input choices
  
  output$select_Taxlifestage <- renderUI({
    
    choice_Taxlifestage <- reactive({
      if (input$Datatype=="Taxa"){
        plotdata()%>%
          select(Taxatype, Taxname, Lifestage, Taxlifestage)%>%
          distinct()%>%
          arrange(Taxatype, Taxname, Lifestage)%>%
          pull(Taxlifestage)%>%
          unique()
      } else {
        NULL
      }
      
    })
    
    selectInput('Taxlifestage', 'Select Taxa (Use Ctr / Cmd / shift to select multiple)', choices =choice_Taxlifestage(), multiple =T, selected=choice_Taxlifestage(), selectize = F, size=10) 
    
  })
  
  output$select_Year <- renderUI({
    
    choice_Year <- reactive({
      plotdata()%>%
        pull(Year)%>%
        range()
    })
    
    sliderInput("Year",
                "Select year:",
                min = min(choice_Year()),  max=max(choice_Year()), value =  min(choice_Year()), step=1, round=T, sep="", 
                animate=animationOptions(interval=1500), width="100%")
    
  })
  
  #Create output so app knows what type of data has actually been created. Used for the select taxa input
  output$Datatype<-reactive( {
    ifelse("Taxatype"%in%colnames(plotdata2()), "Taxa", "Community")
  })
  
  Sampleplot <- reactive({
    myColors <- RColorBrewer::brewer.pal(5,"Set2")
    names(myColors) <- c("EMP", "FMWT", "TNS", "20mm", "FRP")
    fillScale <- scale_fill_manual(name = "Source", values = myColors)
    
    plotdata2()%>%
      mutate(Season=case_when(
        Month%in%c(1,2,3) ~ "Winter",
        Month%in%c(4,5,6) ~ "Spring",
        Month%in%c(7,8,9) ~ "Summer",
        Month%in%c(10,11,12) ~ "Fall"
      ))%>%
      mutate(Season=factor(Season, levels=c("Fall", "Winter", "Spring", "Summer")))%>%
      select(Source, Year, Season, SampleID)%>%
      distinct()%>%
      group_by(Source, Year, Season)%>%
      summarise(N_samples=n())%>%
      ggplot(aes(x=Year, y = N_samples, fill=Source)) +
      geom_bar(stat="identity")+
      facet_wrap(~Season)+
      theme_bw()+
      theme(panel.grid=element_blank(), strip.background=element_blank())+
      fillScale
  })
  
  CPUEplot <- reactive({
    colorCount <- plotdata2()%>%pull(Taxlifestage)%>%unique()%>%length()
    if("Taxatype"%in%colnames(plotdata2())){
      plotdata2()%>%
        filter(Volume>1)%>% # *****Currently removing data with very low sample volumes, should change this later*****
        group_by(Taxlifestage, Year)%>%
        summarise(CPUE=mean(CPUE, na.rm = T))%>%
        ggplot(aes(x=Year, y=CPUE, color=Taxlifestage))+
        geom_line(size=1)+
        geom_point(size=2)+
        coord_cartesian(expand=0)+
        scale_color_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(colorCount), name="Taxa and life stage")+
        ylab("Average CPUE")+
        theme_bw()+
        theme(panel.grid=element_blank(), text=element_text(size=16))
    }else{
      plotdata2()%>%
        filter(Volume>1)%>% # *****Currently removing data with very low sample volumes, should change this later*****
        group_by(Year,Phylum, Class, Order, Family, Genus, Species, Lifestage, Taxlifestage)%>%
        summarise(CPUE=mean(CPUE))%>%
        ungroup()%>%
        arrange(Phylum, Class, Order, Family, Genus, Species, Lifestage)%>%
        mutate(Taxlifestage=factor(Taxlifestage, unique(Taxlifestage)))%>%
        ggplot(aes(x=Year, y=CPUE, fill=Taxlifestage))+
        geom_bar(stat="identity", color="white", size=0.01)+
        coord_cartesian(expand=0)+
        scale_fill_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(colorCount), name="Taxa and life stage")+
        ylab("Average CPUE")+
        theme_bw()+
        theme(panel.grid=element_blank(), text=element_text(size=16))
    }
  })
  
  Mapplot <- reactive({
    if("Taxatype"%in%colnames(plotdata2())){
      pmap+
        geom_point(data=filter(mapdata(), Year==input$Year), aes(y=Latitude, x=Longitude, size=CPUE, color=Taxlifestage), alpha=0.8)+
        scale_size_area(limits=maprange())
    }
  })
  
  
  #Create plot (Plotly makes plot interactive/hoverable)
  
  output$Sampleplot <- renderPlotly({
    ggplotly(Sampleplot())%>%layout(margin=list(t=50))
  })
  
  output$CPUEplot <- renderPlotly({
    ggplotly(CPUEplot())%>%layout(margin=list(t=50))
  })
  
  output$Mapplot <- renderPlot({
    Mapplot()
  })
  
  #Enable downloads
  
  output$download = downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data <- Zooper(Data = input$Datatype, 
                     Sources = input$Sources, 
                     Daterange = ifelse(rep("Dates"%in%input$Filters, 2), input$Daterange, c(NA, NA)),
                     Months = ifelse(rep("Months"%in%input$Filters, length(input$Months)), as.integer(input$Months), rep(NA, length(input$Months))),  
                     SalSurfrange = ifelse(rep("Surface_salinity"%in%input$Filters, 2), input$SalSurfrange, c(NA, NA)),
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
