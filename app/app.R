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







ui <- bootstrapPage(
                    
                    
                    fluidRow(column(12, align="center" ,selectInput("City", "city",
                                      c("Amsterdam" = "Amsterdam",
                                        "Houston" = "Houston",
                                        "Rio de Janeiro" = "Rio de Janeiro",
                                        "Tokyo" = "Tokyo"))  ) ),
                          
                    
                    
                    
                     leafletOutput("map",height = "500"), 
                    
                    
                    
                    
                    # Also add some custom CSS to make the title background area the same
                    # color as the rest of the header.
                    
                    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")) ,
                    
                    
                    
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
                    
                    
                    
                    
                    
                    
                    )

server <- function(input, output, session) {
  
  
  
  acm_defaults <- function(map, x, y) 
    addCircleMarkers(map, x, y, radius=10, color="white", fillColor="orange", 
                     fillOpacity=0, opacity=1, weight=2, stroke=TRUE, layerId="OSM_id")
  
  
  
  points <- eventReactive(input$recalc, {
    cbind(df$X_wgs,df$Y_wgs)
  }, ignoreNULL = FALSE)
  
  
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = FALSE) ) %>%
      addCircleMarkers(data = points(),color = getColor(
        
        
        ((input$Social/100)*mean_soc)+ ((input$Economy/100)*mean_econ) + ((input$Ecology/100)*mean_eco)
        
        
      ), group="locations", layerId = df$OSM_id , stroke = FALSE, fillOpacity = 1, label = htmlEscape(df$name)) 
  })
  
  
  
  # distribute importance over 3 categories###
  observeEvent(input$Ecology,  {
    
    
    eco_val <- input$Ecology
    
    econ_val <- input$Economy
    
    soc_val <- input$Social
    
    current_rest = soc_val+econ_val
    
    change <- (((current_rest + eco_val)-100)*-1)/2
    
    updateSliderInput(session = session, inputId = "Social", value = soc_val+change)
    
    updateSliderInput(session = session, inputId = "Economy", value = econ_val+change)
  })
  
  observeEvent(input$Social,  {
    
    
    eco_val <- input$Ecology
    
    econ_val <- input$Economy
    
    soc_val <- input$Social
    
    
    current_rest = soc_val+econ_val
    
    change <- (((current_rest + eco_val)-100)*-1)/2
    
    updateSliderInput(session = session, inputId = "Ecology", value = eco_val+change)
    
    updateSliderInput(session = session, inputId = "Economy", value = econ_val+change)
  })
  
  observeEvent(input$Economy,  {
    
    
    eco_val <- input$Ecology
    
    econ_val <- input$Economy
    
    soc_val <- input$Social
    
    
    current_rest = soc_val+econ_val
    
    change <- (((current_rest + eco_val)-100)*-1)/2
    
    updateSliderInput(session = session, inputId = "Social", value = soc_val+change)
    
    updateSliderInput(session = session, inputId = "Ecology", value = eco_val+change)
    
  })
  
  ############################################
  
  
  
}

shinyApp(ui, server)