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

require(data.table)
require(lubridate)

load("../zoopforzooper.Rdata")
#Source Sam's function that gets the data from online
source("../Zoop synthesizer function.R")

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
                                         min = 0, max = 32, value = c(0,7))),
            conditionalPanel(condition = "input.Filters.includes('Latitude')",
                             sliderInput("Latrange",
                                         "Latitude Range",
                                         min = 37, max = 39, value = c(37,39))),
            conditionalPanel(condition = "input.Filters.includes('Longitude')",
                             sliderInput("Longrange",
                                         "Longitude Range:",
                                         min = -125, max = -119, value = c(-125, -119))),
            conditionalPanel(condition = "input.Filters.includes('Dates')",
                             dateRangeInput("Daterange", label = "Date range", 
                                            start = "1972-01-01", end = "2018-12-31", startview = "year")),
            actionButton("Run", "Run"),
        downloadButton("download", "Download")
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        ),
        position = "left",
        fluid = F
    )
)

# Define server logic required to draw a plot
server <- function(input, output) {
    
    observeEvent(input$Run, {
        showNotification("Crunching data...", type = "message", duration = NULL)
    })
    
    #Using eventReactive so app only updates when "Run" button is clicked, letting you check all the boxes you want before running the app
    zooplot <- eventReactive(input$Run, {
        
        #select the data the user wants
        
        data <- Zooper(Data = input$Datatype, 
                       Sources = input$Sources, 
                       Daterange = ifelse(rep("Dates"%in%input$Filters, 2), input$Daterange, c(NA, NA)),
                       Months = ifelse(rep("Months"%in%input$Filters, length(input$Months)), as.integer(input$Months), rep(NA, length(input$Months))),  
                       SalSurfrange = ifelse(rep("Surface_salinity"%in%input$Filters, 2), input$SalSurfrange, c(NA, NA)),
                       Latrange = ifelse(rep("Latitude"%in%input$Filters, 2), input$Latrange, c(NA, NA)), 
                       Longrange = ifelse(rep("Longitude"%in%input$Filters, 2), input$Longrange, c(NA, NA)), 
                       Shiny=T)
        
        # draw the scatterplot of the the critters
        ggplot(data, aes(x=Date, y = CPUE)) +
            geom_point(aes(shape = Source))+
            theme_bw()+
            theme(panel.grid=element_blank())
    })
        
        output$distPlot <- renderPlot({
            zooplot()
        })
    
    output$download = downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            data    <- Zooper(Data = input$Datatype, 
                           Sources = input$Sources, 
                           Daterange= ifelse("Dates"%in%input$Filters, input$Daterange, c(NA, NA)),
                           Months = ifelse("Months"%in%input$Filters, as.integer(input$Months), NA),  
                           SalSurfrange = ifelse("Surface salinity"%in%input$Filters, input$SalSurfrange, NA),
                           Latrange = ifelse("Latitude"%in%input$Filters, input$Latrange, NA), 
                           Longrange =  ifelse("Longitudee"%in%input$Filters, input$Longrange, NA), 
                           Shiny=T)
            write.csv(data, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
