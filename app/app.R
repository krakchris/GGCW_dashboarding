library(shiny)
library(leaflet)
library(shinydashboard)
library(htmltools)
library(feather)

city = 'amsterdam'

# read datafile
df <- read_feather("Amsterdam_score.feather")


# calculat mean scoring over all values
df$mean_score  <- rowMeans(df[,c(6,7,8,10,11,12,13,14)])


# create secuence based on spread of scoring values
cut_values <- seq(min(df$mean_score)+0.1, max(df$mean_score)-0.1, length.out=5)


df$name <- as.factor(df$name)

getColor <- function(input) {
  sapply(input, function(input) {
    if(input < cut_values[1]) {
      "#E50006"
    } else if(input <= cut_values[2]) {
      "#AB2F08"
    } else if(input <= cut_values[3]) {
      "#725F0A"
    } else if(input <= cut_values[4]) {
      "#398F0C"
    } else {
      "#00BF0F"
    } })
}


fileName <- 'www/index_v2.html'
html <- readChar(fileName, file.info(fileName)$size)


digit <- 'hallomensen'


header <- dashboardHeader(title = city,titleWidth = 300,disable = TRUE)

sidebar <- dashboardSidebar( actionButton("recalc", "New points"),disable = TRUE)


body <- dashboardBody(
  
  fluidRow(selectInput("City", "City:",
                       c("Amsterdam" = "ams",
                         "Houston" = "hou",
                         "Rio de Janeiro" = "rio"))))
  
  


body <- dashboardBody(fluidRow(

                      box(  leafletOutput("mymap",height = "500"), background = "black", width = 12)),
  
                        # Also add some custom CSS to make the title background area the same
                        # color as the rest of the header.
                       
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                        ),
                      tags$div(
                        HTML(sprintf(html , "100%", "50% 100%"))
                      )
                         
                        )

dashboardPage(header, sidebar, body)


ui <- dashboardPage( header,sidebar,body)






server <- function(input, output, session) {
  
  
 
  
  points <- eventReactive(input$recalc, {
    cbind(df$X_wgs,df$Y_wgs)
  }, ignoreNULL = FALSE)
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = FALSE)
      ) %>%
      addCircleMarkers(data = points(),color = getColor(df$mean_score),
                       stroke = FALSE, fillOpacity = 1, label = htmlEscape(df$name)) 
  })
  
 
  
  
  
}



shinyApp(ui, server)




