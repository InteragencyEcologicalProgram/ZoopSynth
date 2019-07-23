# This is a Shiny web application designed to allow users to select which zooplankton data they would
#like to combine and download the resulting data.

#It also currently makes a graph of the data, but it's not super userful.

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(tidyverse)
require(readxl)

#Requires Github developer version of dtplyr: devtools::install_github("tidyverse/dtplyr")
require(dtplyr)
require(lubridate)

#Source Sam's function that gets the data from online
source("Zoop synthesizer function.R")


info_loading <- "Data crunching in progress..."
your_color01 <- "#CD2626"
your_color02 <- "#D3D3D3"

# Define UI for application that draws a scaterplot and allows you to download data
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
                               choices = c("EMP", "FRP", "FMWT", "TNS", "20mm")),
            checkboxGroupInput("Filters",
                               "Filters:",
                               choices = c("Months", "Surface_salinity", "Latitude", "Longitude", "Dates")),
            conditionalPanel(condition = "input.Filters.includes('Months')",
                             selectInput("Months", "Months:", choices=1:12, selected = 1, multiple = T)),
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
            conditionalPanel(condition = "input.Filters.includes('Dates')",
                             dateRangeInput("Daterange", label = "Date range", 
                                            start = "1972-01-01", end = "2018-12-31", startview = "year")),
            actionButton("Run", "Run"),
            conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                             uiOutput("select_Taxlifestage")),
            conditionalPanel(condition = "output.Datatype == 'Taxa'", 
                             actionButton("Update_taxa", "Update taxa")),
            downloadButton("download", "Download")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        ),
        position = "left",
        fluid = F
    ),
        
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
                                             color: ", your_color01,";
                                             background-color: ", your_color02,";
                                             z-index: 105;
                                             }
                                             "))),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(info_loading,id="loadmessage"))
)

# Define server logic required to draw a plot
server <- function(input, output, session) {
    
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
    
    plotdata2 <- eventReactive(c(input$Run, input$Update_taxa), {
        if (length(input$Taxlifestage)>0 & input$Datatype=="Taxa"){
            filter(plotdata(), Taxlifestage%in%input$Taxlifestage)
        } else {
            plotdata()
        }
    }, ignoreInit=T)
    
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
        
        selectInput('Taxlifestage', 'Select Taxa (Use Ctr / Cmd / shift to select multiple)', choices =choice_Taxlifestage(), multiple =T, selected=choice_Taxlifestage(), selectize = F, size=10) # <- put the reactive element here
        
    })
    
    output$Datatype<-reactive( {
        ifelse("Taxatype"%in%colnames(plotdata2()), "Taxa", "Community")
    })
    
    zooplot <- reactive( {
        
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
            theme(panel.grid=element_blank(), strip.background=element_blank())
    })
    
    output$distPlot <- renderPlot({
        zooplot()
    })
    
    
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
            data <-if (length(input$Taxlifestage)>0){
                filter(data, Taxlifestage%in%input$Taxlifestage)
            } else {
                data
            }
            write.csv(data, file)
        })
    outputOptions(output, "Datatype", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
