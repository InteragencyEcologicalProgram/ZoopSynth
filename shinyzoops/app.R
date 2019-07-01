#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Zooplankton"),

    # check boxes where you choose data you want
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("survey",
                        "Surveys:",
                        choices = c("EMP", "FRP", "FMWT", "Townet", "20mm")),

            checkboxGroupInput("taxon",
                               "Taxon:",
                               choices = c("ACARJUV", "ACARTELA","ACARTIA",
                                           "ASINEJUV",   "ASPLANCH",   "BARNNAUP",   "BOSMINA","CALJUV")),
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
    FMWT_EMP_20mm = read.csv("FMWT_EMP_20mm.csv")
    
    output$distPlot <- renderPlot({
        #select the data the user wants
        x    <- FMWT_EMP_20mm[which(FMWT_EMP_20mm$Project == input$survey &
                                    FMWT_EMP_20mm$LCDtax == input$taxon), ]

        # draw the histogram with the specified number of bins
        ggplot(x, aes(x=Date, y = CPUE, color = LCDtax)) + geom_point(aes(pch = Project))
    })
    output$download = downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            x    <- FMWT_EMP_20mm[which(FMWT_EMP_20mm$Project == input$survey &
                                            FMWT_EMP_20mm$LCDtax == input$taxon), ]
            write.csv(x, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
