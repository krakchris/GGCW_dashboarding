

############## app version

library("htmltools")
library(shiny)
library(leaflet)
library(feather)
library(shinydashboard)


social <- data.frame()

# read datafile
city_coords <- read_feather("city_coordinates.feather")

df1 <- read_feather("ams_rio_tokyo_hou_score.feather")

df1 <- df1[!duplicated(df1), ]





df <- df1

# calculat mean scoring over all values
df$mean_score  <- rowMeans(df[,c(6,7,8,10,11,12,13,14)])

# create secuence based on spread of scoring values
cut_values <- seq(min(df$mean_score)+0.1, max(df$mean_score)-0.1, length.out=5)
cut_values_econ <- c(1000,5000,10000,100000,300000)



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




Park_name <- "Click on park for more info"

mean_soc <- rowMeans(df[,c(10,11)])

mean_eco <- rowMeans(df[,c(6,7,8,12,13,14)])

mean_econ<- getScore_econ(df$Monetary)




ui <- shinyUI(fluidPage(  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")) ,
                          
                          tags$head(HTML("<link href='http://fonts.googleapis.com/css?family=Roboto' rel='stylesheet' type = 'text/css'>")),
                          
                          
                          
                          fluidRow(
                            
                            column(12,
                                   
                                   selectInput("City", "City:",c(" " = " ", 
                                                                 "Amsterdam" = "Amsterdam",
                                                                 "Houston" = "Houston",
                                                                 "Rio de Janeiro" = "Rio de Janeiro",
                                                                 "Tokyo" = "Tokyo")), align = "center", height = "100px", 
                                   style = "height: 80px; font-family: 'Roboto'; color: white; font-size: 20px;"
                                   
                            )),
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          fluidRow(
                            
                            leafletOutput("map",height = "400px")
                            
                          ),
                          
                          
                          # Also add some custom CSS to make the title background area the same
                          # color as the rest of the header.

                          h1("Use sliders to change importance of different score groups", 
                             style = "font-family: 'Roboto'; color: white; font-size: 20px; text-align: center;"),
                          
                          
                          
                          
                          fluidRow(
                          
                          
                          
                          
                          column(align="center", width = 4, style = "font-family: 'Roboto'; color: white; font-size: 20px;",
                                 sliderInput("Social", "Social:",
                                             min = 0, max = 100,step= 10,
                                             value = 40))  ,
                          
                          column( align="center",width = 4, style = "font-family: 'Roboto'; color: white; font-size: 20px; ",
                                  sliderInput("Ecology", "Ecology:",
                                              min = 0, max = 100,step= 10,
                                              value = 30)) ,
                          
                          column( width = 4, style = "font-family: 'Roboto'; color: white; font-size: 20px;  ",align="center",
                                  sliderInput("Economy", "Economy:",
                                              min = 0, max = 100,step= 10,
                                              value = 30)) 
                          )
                          
                          
                          ,
                          
                          fluidRow(
                          
                          uiOutput("Park_name",align="center",style = "font-family: 'Roboto'; color: white; font-size: 11px; padding:10px; "),
                          
                          fluidRow(
                          
                          column(4,align="center", style = "font-family: 'Roboto'; color: white; font-size: 20px;  ",
                                 icon('users',  "fa-3x"),
                                 uiOutput("mean_soc",align="center",style = "font-family: 'Roboto'; color: white; font-size: 20px; padding:10px; ")
                                 
                          ),
                          
                          column(4,align="center", style = "font-family: 'Roboto'; color: white; font-size: 20px;  ",
                                 
                                 icon('envira',  "fa-3x"),
                                 uiOutput("mean_eco",align="center",style = "font-family: 'Roboto'; color: white; font-size: 20px;  padding:10px;")
                                 
                          ),
                          
                          column(4,align="center", style = "font-family: 'Roboto'; color: white; font-size: 20px;  ",
                                 icon('briefcase',  "fa-3x"),
                                 uiOutput("mean_econ",align="center",style = "font-family: 'Roboto'; color: white; font-size: 20px; padding:10px; ")
                                 
                          )
                          
                          
                          )
                          
                          ),fluidRow(),
                          
                          
                          
                          fluidRow(
                            
                          column(4, align="center", tableOutput('social'), style = "font-family: 'Roboto'; color: white; font-size: 12px;  "
                          ),
                          column(4 , align="center", tableOutput('eco'), style = "font-family: 'Roboto'; color: white; font-size: 12px;  "
                          ),
                          column(4 , align="center", tableOutput('econ'),style = "font-family: 'Roboto'; color: white; font-size: 12px;  "
                          )
)
                          
               
                          
                          

                          
                          
                          
)
)


###############################################################################
# server
###############################################################################

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
                       options = providerTileOptions(noWrap = FALSE)
      ) %>%
      addCircleMarkers(data = points(),color = getColor(
        
        
        ((input$Social/100)*mean_soc)+ ((input$Economy/100)*mean_econ) + ((input$Ecology/100)*mean_eco)
        
        
      ), group="locations", layerId = df$OSM_id , stroke = FALSE, fillOpacity = 1, label = htmlEscape(df$name)) 
  })
  
  
  observeEvent(input$map_marker_click, {
    ## Get the click info like had been doing
    
    p <- (input$map_marker_click)
    
    
    
    proxy <- leafletProxy("map")
    
  
    
    if(p$id=="OSM_id"){
      proxy %>% removeMarker(layerId="OSM_id")
    } else {
      
      s_sc <- df[isolate(p$id) == df$OSM_id,]
      
      output$Park_name <- renderUI({
        list(tags$h3(s_sc$name))
      }) 
      
      
      
      
      
      social = data.frame(c("Amenities and recreational facillities ","Gray vs Green ","Greenness in winter"), 
                          c(s_sc$Soc_Amen,s_sc$Soc_Grey,s_sc$Soc_Winter))
      
      colnames(social) = c("Social Indicators", "Score")
      
      
      
      eco = data.frame(c("Green within a riparian zone","Width of blue space in a park","Impermeable surfaces","Stormwater Capture","Leaf Area Index"), 
                       c(s_sc$Infil_Rip,s_sc$Temp_Water,s_sc$Infil_Inper,s_sc$Infil_Storm,s_sc$Temp_LAI))
      
      colnames(eco) = c("Ecological Indicators", "Score")
      
      
      
      econ = data.frame(c("Economic value of ecosystem services"), 
                        c(s_sc$Monetary))
      
      colnames(econ) = c("Economic indicator ", "$")

  
      output$mean_soc <- renderText(paste("Social mean score:",  as.character(round(mean(social[,2]),2))))
      output$mean_eco <- renderText(paste("Eco mean score:",  as.character(round(mean(eco[,2]),2))))
      output$mean_econ <- renderText(paste("Monetary value:",  as.character(mean(econ[,2])), " $"))
      
      
      
      output$social <- renderTable(social)
      output$eco <- renderTable(eco)
      output$econ <- renderTable(econ)
      
      
      
      proxy %>% setView(lng=p$lng, lat=p$lat, 12) %>% acm_defaults(p$lng, p$lat)
      
    }
    
    
    
    
  })
  
  
  
  # distribute importance over 3 categories###
  observeEvent(input$Ecology,  {
    
    
    p <- input$City
    
    
    proxy <- leafletProxy("map")
    
    if(p =="Rio de Janeiro"){
      
      x = city_coords$Rio[1]
      
      y = city_coords$Rio[2]
      
      proxy %>% setView(lat=y, lng=x,10)
      
      
    } else if (p =="Amsterdam"){
      
      x = city_coords$Amsterdam[1]
      
      y = city_coords$Amsterdam[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
    } else if (p =="Houston"){
      
      x = city_coords$Houston[1]
      
      y = city_coords$Houston[2]
      
      proxy %>% setView(lng=y, lat=x, 10) 
      
    } else if (p =="Tokyo"){
      
      x = city_coords$Tokyo[1]
      
      y = city_coords$Tokyo[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
      
      
    }
    
    
    proxy <- leafletProxy("map")
    
    
    eco_val <- input$Ecology
    
    econ_val <- input$Economy
    
    soc_val <- input$Social
    
    current_rest = soc_val+econ_val
    
    change <- (((current_rest + eco_val)-100)*-1)/2
    
    updateSliderInput(session = session, inputId = "Social", value = soc_val+change)
    
    updateSliderInput(session = session, inputId = "Economy", value = econ_val+change)
  })
  
  observeEvent(input$Social,  {
    
    
    proxy <- leafletProxy("map")
    
    p <- input$City
    
    if(p =="Rio de Janeiro"){
      
      x = city_coords$Rio[1]
      
      y = city_coords$Rio[2]
      
      proxy %>% setView(lat=y, lng=x, 10)
      
      
    } else if (p =="Amsterdam"){
      
      x = city_coords$Amsterdam[1]
      
      y = city_coords$Amsterdam[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
    } else if (p =="Houston"){
      
      x = city_coords$Houston[1]
      
      y = city_coords$Houston[2]
      
      proxy %>% setView(lng=y, lat=x, 10) 
      
    } else if (p =="Tokyo"){
      
      x = city_coords$Tokyo[1]
      
      y = city_coords$Tokyo[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
      
      
    }
    
    
    
    eco_val <- input$Ecology
    
    econ_val <- input$Economy
    
    soc_val <- input$Social
    
    
    current_rest = soc_val+econ_val
    
    change <- (((current_rest + eco_val)-100)*-1)/2
    
    updateSliderInput(session = session, inputId = "Ecology", value = eco_val+change)
    
    updateSliderInput(session = session, inputId = "Economy", value = econ_val+change)
  })
  
  observeEvent(input$Economy,  {
    
    
    proxy <- leafletProxy("map")
    
    p <- input$City
    
    
    
    
    if(p =="Rio de Janeiro"){
      
      x = city_coords$Rio[1]
      
      y = city_coords$Rio[2]
      
      proxy %>% setView(lat=y, lng=x, 10)
      
      
    } else if (p =="Amsterdam"){
      
      x = city_coords$Amsterdam[1]
      
      y = city_coords$Amsterdam[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
    } else if (p =="Houston"){
      
      x = city_coords$Houston[1]
      
      y = city_coords$Houston[2]
      
      proxy %>% setView(lng=y, lat=x, 10) 
      
    } else if (p =="Tokyo"){
      
      x = city_coords$Tokyo[1]
      
      y = city_coords$Tokyo[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
      
      
    }
    
    
    
    eco_val <- input$Ecology
    
    econ_val <- input$Economy
    
    soc_val <- input$Social
    
    
    current_rest = soc_val+econ_val
    
    change <- (((current_rest + eco_val)-100)*-1)/2
    
    updateSliderInput(session = session, inputId = "Social", value = soc_val+change)
    
    updateSliderInput(session = session, inputId = "Ecology", value = eco_val+change)
    
  })
  
  ############################################
  
  observeEvent(input$City, {
    ## Get the click info like had been doing
    
    p <- input$City
    
    
    
    
    
    proxy <- leafletProxy("map")
    
    if(p =="Rio de Janeiro"){
      
      x = city_coords$Rio[1]
      
      y = city_coords$Rio[2]
      
      proxy %>% setView(lat=y, lng=x, 10)
      
      
    } else if (p =="Amsterdam"){
      
      x = city_coords$Amsterdam[1]
      
      y = city_coords$Amsterdam[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
    } else if (p =="Houston"){
      
      x = city_coords$Houston[1]
      
      y = city_coords$Houston[2]
      
      proxy %>% setView(lng=y, lat=x, 10) 
      
    } else if (p =="Tokyo"){
      
      x = city_coords$Tokyo[1]
      
      y = city_coords$Tokyo[2]
      
      proxy %>% setView(lng=y, lat=x, 10)
      
      
      
    }
    
  })
  
  
  
  
  
  
  
  
  ### end of server ###
}



shinyApp(ui, server)




