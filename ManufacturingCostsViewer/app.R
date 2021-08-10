###############################################
## Manufacturing Costs Viewer
## Final Project for Week 4 for Developing Data Products course
##
## Author: Bradley Burquest
## Date: 8/10/2021
##
## Application to display data from the ManufactCosts dataset that is part of
## the Rdatasets package. The application provides options for selecting the years
## to plot and change the graph line width
##

library(shiny)
library(tidyverse)
library(ggplot2)


# Load the data from the web
mdata <- read.csv( 'https://vincentarelbundock.github.io/Rdatasets/csv/AER/ManufactCosts.csv')
colnames(mdata)[1] <- "year"   # rename column X to year
mdata$year <- mdata[,'year'] + 1946  # convert to proper year

cNames <- c("Capital","Labor","Energy","Materials")
cVals <- cNames
iNames <- c("Capital Price","Labor Price","Energy Price","Materials Price")
iVals <-  c("Capital","Labor","Energy","Materials")


ui <- shinyUI( fluidPage(

   h1(id="big-heading", "Manufacturing Costs Viewer"),
   tags$style(HTML("#big-heading{color: white; background-color: #3498DB;}")),
  
  sidebarLayout(
    sidebarPanel(style="background-color: #D6EAF8;", #light blue
        h4(HTML("<u>Introduction</u>")),
        p(HTML(paste("This application uses the <em>ManufactCosts</em> dataset included in the Rdatasets AER package.",
                     " The dataset consists of Manfacturing cost data from the year 1947-1971. More on this dataset can be found", 
                     " <a href='https://vincentarelbundock.github.io/Rdatasets/doc/AER/ManufactCosts.html'>here</a>."))),
        p(HTML(paste(" You can select which data graphs to show for the manfacturing data by checking the appropriate check boxes.", 
                     " The graphs will automatically update upon your selections."))),
        hr( style='border: 1px solid black;'),
        h4(HTML("<u>Graph Settings</u>")),
        p("Check appropriate data you would like to graph."),
        checkboxGroupInput("costs", "Select Costs to show", 
                           choiceNames=cNames, 
                           choiceValues = cVals,
                           selected=c("Capital", "Energy")),    

        checkboxGroupInput("prices", 
                           "Select Prices to show ", 
                           choiceNames=iNames, 
                           choiceValues = iVals,
                           selected=c("Capital","Energy","Materials")),
        hr( style='border: 1px solid black;'),
        h4(HTML("<u>Optional Settings</u>")),
        p( "Select the width of the graph lines for better viewing. Move the slider to the appropriate line width"),
        sliderInput( "lwidth", "Set Line Width", min=1, max=5, value=2),
        p( "Change the Year Range for graphing. Select the starting and ending years for display."),
        sliderInput( "startyr", "Start Year", min=1947, max=1971, value=1947),
        sliderInput( "endyr", "End Year", min=1947, max=1971, value=1971)
        
    ),
    mainPanel(style="background-color: #EBF5FB;", 
      h3("Manufacturing Costs"),
      plotOutput("costs"),
      h3("Manufacturing Prices"),
      plotOutput("prices"),
    )
  )
  
)
)

server <- shinyServer(function( input, output ) {
  
    # Create the Costs plot
    output$costs <- renderPlot({
      selected <- input$costs
      lwidth <- input$lwidth
      manudata <- mdata %>% 
                  filter( year >= input$startyr & year <= input$endyr)
      g <- ggplot( manudata, aes(x=year))
      g <- g + xlab(paste("Year (", input$startyr, "-", input$endyr, ")")) + ylab("Manufacturing Costs")
      for( costtype in selected ) {
        if (costtype == "Capital") {
          g <- g + geom_line(aes(y=capitalcost, col="Capital Cost"), size=lwidth)
        }
        if ( costtype == "Labor") {
          g <- g + geom_line(aes(y=laborcost, col="Labor Cost"), size=lwidth)
        }
        if ( costtype == "Energy") {
          g <- g + geom_line(aes(y=energycost, col="Energy Cost"), size=lwidth)
        }
        if ( costtype == "Materials") {
          g <- g + geom_line(aes(y=materialscost, col="Materials Cost"), size=lwidth)
        }
      }
      g
    })

    ## Create the Prices Plot
    output$prices <- renderPlot({
      lwidth <- input$lwidth
      manudata <- mdata %>% 
        filter( year >= input$startyr & year <= input$endyr )
      g <- ggplot( manudata, aes(x=year))
      g <- g + xlab(paste("Year (", input$startyr, "-", input$endyr, ")")) + ylab("Price Index")
      for(idx in input$prices) {
        if (idx == "Capital") {
          g <- g + geom_line(aes(y=capitalprice,  col="Capital Price"), size=lwidth)
        }
        if (idx == "Labor" ) {
          g <- g + geom_line(aes(y=laborprice, col="Labor Price"), size=lwidth)
        }
        if (idx == "Energy") {
          g <- g + geom_line(aes(y=energyprice, col="Energy Price"), size=lwidth)
        }
        if (idx == "Materials") {
          g <- g + geom_line(aes(y=materialsprice, col="Materials Price"), size=lwidth)
        }
      }
      g
    })
    
    
  }
)

shinyApp(ui, server)