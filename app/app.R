library(shiny)
library(leaflet)
library(htmltools)
library(feather)




# read datafile
df <- read_feather("Amsterdam_score.feather")


# calculat mean scoring over all values
df$mean_score  <- rowMeans(df[,c(6,7,8,10,11,12,13,14)])

# create secuence based on spread of scoring values
cut_values <- seq(min(df$mean_score)+0.1, max(df$mean_score)-0.1, length.out=5)
cut_values_econ <- seq(min(df$Monetary)+1, max(df$Monetary)-1, length.out=5)



df$name <- as.factor(df$name)

######################################
# handy functions
#######################################




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

getScore_econ <- function(input) {
  sapply(input, function(input) {
    
    if(input < cut_values_econ[1]) {
      as.numeric(1)
    } else if(input <= cut_values_econ[2]) {
      as.numeric(2)
    } else if(input <= cut_values_econ[3]) {
      as.numeric(3)
    } else if(input <= cut_values_econ[4]) {
      as.numeric(4)
    } else {
      as.numeric(5)
    } })
}

#########################################

city = 'amsterdam'



mean_soc <- rowMeans(df[,c(10,11)])

mean_eco <- rowMeans(df[,c(6,7,8,12,13,14)])

mean_econ<- getScore_econ(df$Monetary)





ui <- shinyUI(bootstrapPage(theme = "bootstrap.css",
                            
                            fluidRow( 
                              
                              
                              box(selectInput("City", "city",
                                              c("Amsterdam" = "Amsterdam",
                                                "Houston" = "Houston",
                                                "Rio de Janeiro" = "Rio de Janeiro",
                                                "Tokyo" = "Tokyo")), align="center", width = "100%", height= "100px", 
                                  style='padding:10px; font-size: 150%; background = "black"; font-family: "Roboto";')
                            ),
                            
                            
                            
                            tags$head(HTML("<link href='http://fonts.googleapis.com/css?family=Roboto' rel='stylesheet' type = 'text/css'>")),
                            
                            
                            h1(title= "hallo", style = "font-family: 'Roboto'; color: white; font-size: 64px; text-align: center;"),
                            
                            
                            
                            
                            
                            
                            box(  leafletOutput("map",height = "500"), background = "black", width = 12),
                            
                            
                            
                            # Also add some custom CSS to make the title background area the same
                            # color as the rest of the header.
                            
                            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                            ),
                            
                            
                            
                            
                            
                            
                            
                            column(align="center", width = 4, style = "font-family: 'Roboto'; color: white; font-size: 20px;",
                                   sliderInput("Social", "Social:",
                                               min = 0, max = 100,
                                               value = 40))  ,
                            
                            column( align="center",width = 4, style = "font-family: 'Roboto'; color: white; font-size: 20px; ",
                                    sliderInput("Ecology", "Ecology:",
                                                min = 0, max = 100,
                                                value = 30)) ,
                            
                            column( width = 4, style = "font-family: 'Roboto'; color: white; font-size: 20px;  ",align="center",
                                    sliderInput("Economy", "Economy:",
                                                min = 0, max = 100,
                                                value = 30)) 
                            
                            
                            
                            ,
                            
                            
                            h1("Use sliders to change importance of different score groups", 
                               style = "font-family: 'Roboto'; color: white; font-size: 20px; text-align: center;"),
                            
                            
                            
                            fluidRow( 
                              column(1, align="center", tableOutput('values')
                              ),
                              column(2 ,offset = 5, align="center", tableOutput('table1')
                              )          
                              
                              
                            )))

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)