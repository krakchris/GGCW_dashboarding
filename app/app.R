

############## app version


rm(list=ls())

library(shiny)
library(leaflet)
library(htmltools)
library(feather)



# read datafile
city_coords <- read_feather("city_coordinates.feather")

df1 <- read_feather("ams_rio_tokyo_hou_score.feather")









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




