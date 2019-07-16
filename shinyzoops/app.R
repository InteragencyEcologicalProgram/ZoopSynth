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

library(shiny)
library(tidyverse)

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
                               choices = c("Months", "Surface salinity", "Latitude", "Longitude", "Dates")),
            conditionalPanel(condition = "input.Filters.includes('Months')",
                             sliderInput("Months",
                                         "Months:",
                                         min = 1, max = 12, value = c(1,12))),
            conditionalPanel(condition = "input.Filters.includes('Surface salinity')",
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
        #Source Sam's function that gets the data from online
        source("../Zoop synthesizer function.R")
        
        output$distPlot <- renderPlot({
            #select the data the user wants
            x    <- Zooper(Data = input$Datatype, 
                           Sources = input$Sources, 
                           Daterange= ifelse("Dates"%in%input$Filters, input$Daterange, c(NA, NA)),
                           Months = ifelse("Months"%in%input$Filters, input$Months[1]:input$Months[2], NA),  
                           SalSurfrange = ifelse("Surface salinity"%in%input$Filters, input$SalSurfrange, NA),
                           Latrange = ifelse("Latitude"%in%input$Filters, input$Latrange, NA), 
                           Longrange =  ifelse("Longitudee"%in%input$Filters, input$Longrange, NA), 
                           Shiny=T)
            
            # draw the scatterplot of the the critters
            ggplot(x, aes(x=Date, y = CPUE)) + geom_point(aes(pch = Source))+ggtitle(nrow(x))
        })
    })
    
    output$download = downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            x    <- Zooper(Data = input$Datatype, 
                           Sources = input$Sources, 
                           Daterange= ifelse("Dates"%in%input$Filters, input$Daterange, c(NA, NA)),
                           Months = ifelse("Months"%in%input$Filters, input$Months[1]:input$Months[2], NA),  
                           SalSurfrange = ifelse("Surface salinity"%in%input$Filters, input$SalSurfrange, NA),
                           Latrange = ifelse("Latitude"%in%input$Filters, input$Latrange, NA), 
                           Longrange =  ifelse("Longitudee"%in%input$Filters, input$Longrange, NA), 
                           Shiny=T)
            write.csv(x, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
