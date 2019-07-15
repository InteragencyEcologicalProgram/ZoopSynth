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
            #Note: we will want to split out townet eventually

          
            sliderInput("Months",
                               "Months:",
                               min = 1, max = 12, value = c(1,12)),
            sliderInput("SalSurfrange",
                        "Surface Slainity:",
                        min = 0, max = 32, value = c(0,7)),
            sliderInput("Latrange",
                        "Latitude Range",
                        min = 37, max = 39, value = c(37,39)),
            sliderInput("Longrange",
                        "Longitude Range:",
                        min = -125, max = -119, value = c(-125, -119)),
            dateRangeInput("Daterange", label = "Date range", 
                           start = "1972-01-01", end = "2018-12-31", startview = "year"),

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
    #Source Sam's function that gets the data from online
    source("Zoop synthesizer function.R")
            
    output$distPlot <- renderPlot({
        #select the data the user wants
x    <- Zooper(Data = input$Datatype, Sources = input$Sources, Daterange= input$Daterange,
                      Months = input$Months,  SalSurfrange= input$SalSurfrange,
                       Latrange= input$Latrange, Longrange= input$Longrange)

        # draw the scatterplot of the the critters
        ggplot(x, aes(x=Date, y = CPUE)) + geom_point(aes(pch = Source))
    })
    output$download = downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            x    <- Zooper(Data = input$Datatype, Sources = input$Sources, Daterange= input$Daterange,
                           Months = input$Months,  SalSurfrange= input$SalSurfrange,
                           Latrange= input$Latrange, Longrange= input$Longrange)
            write.csv(x, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

test = Zooper(Data = "Taxa", c("FMWT", "FRP"), Years = 2010)
