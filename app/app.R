

############## app version


rm(list=ls())

library(shiny)
library(leaflet)
library(htmltools)
library(feather)



# read datafile
city_coords <- read_feather("city_coordinates.feather")

df1 <- read_feather("ams_rio_tokyo_hou_score.feather")

# calculat mean scoring over all values
df$mean_score  <- rowMeans(df[,c(6,7,8,10,11,12,13,14)])

# create secuence based on spread of scoring values
cut_values <- seq(min(df$mean_score)+0.1, max(df$mean_score)-0.1, length.out=5)
cut_values_econ <- seq(min(df$Monetary)+1, max(df$Monetary)-1, length.out=5)



df$name <- as.factor(df$name)

df <- df1

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




