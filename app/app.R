

############## app version


rm(list=ls())

library(shiny)
library(leaflet)
library(htmltools)
library(feather)

## Only run this example in interactive R sessions
  # table example

    ui = fluidPage(
      fluidRow(
        column(12,
               tableOutput('table')
        )
      ))
    
    
    
    
server = function(input, output) {
      output$table <- renderTable(iris)
    }
  
  


shinyApp(ui, server)




