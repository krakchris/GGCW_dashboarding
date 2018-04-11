library(shiny)
library(leaflet)
library(htmltools)
library(feather)


<<<<<<< HEAD
pdf(NULL)
dev.off() 

# read datafile
df <- read_feather("Amsterdam_score.feather")


# calculat mean scoring over all values
df$mean_score  <- rowMeans(df[,c(6,7,8,10,11,12,13,14)])

# create secuence based on spread of scoring values
cut_values <- seq(min(df$mean_score)+0.1, max(df$mean_score)-0.1, length.out=5)
cut_values_econ <- seq(min(df$Monetary)+1, max(df$Monetary)-1, length.out=5)



df$name <- as.factor(df$name)
=======
>>>>>>> parent of f23ca5f... fixed variable seq.

######################################
# handy functions
#######################################


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

getScore_econ <- function(input) {
  sapply(input, function(input) {
    
    if(input < cut_values_econ[1]) {
      1
    } else if(input <= cut_values_econ[2]) {
      2
    } else if(input <= cut_values_econ[3]) {
      3
    } else if(input <= cut_values_econ[4]) {
      4
    } else {
      5
    } })
}

#########################################


city = 'amsterdam'

# read datafile
df <- read_feather("Amsterdam_score.feather")


<<<<<<< HEAD
=======
# calculat mean scoring over all values
df$mean_score  <- rowMeans(df[,c(6,7,8,10,11,12,13,14)])


# create secuence based on spread of scoring values
cut_values <- seq(min(df$mean_score)+0.1, max(df$mean_score)-0.1, length.out=5)


>>>>>>> parent of f23ca5f... fixed variable seq.
mean_soc <- rowMeans(df[,c(10,11)])

mean_eco <- rowMeans(df[,c(6,7,8,12,13,14)])

mean_econ<- getScore_econ(df$Monetary)

<<<<<<< HEAD
<<<<<<< HEAD

=======
>>>>>>> parent of f23ca5f... fixed variable seq.


 ui <- shinyUI(bootstrapPage(theme = "bootstrap.css",
   
   
                             plot.new() ,
   
   fluidRow( 

     
     box(selectInput("City", "city",
                        c("Amsterdam" = "Amsterdam",
                          "Houston" = "Houston",
                          "Rio de Janeiro" = "Rio de Janeiro",
                          "Tokyo" = "Tokyo")), align="center", width = "100%", height= "100px", style='padding:10px; font-size: 150%; background = "black"; font-family: "Roboto";',
 
                  tags$style(type='text/css', "#button { vertical-align: middle; height: 50px; width: 100%; font-size: 40px;}")
     )
     
     
  ) ,
   
   
   tags$head(HTML("<link href='http://fonts.googleapis.com/css?family=Roboto' rel='stylesheet' type = 'text/css'>")),
  
  
   h1(title= "hallo", style = "font-family: 'Roboto'; color: white; font-size: 64px; text-align: center;"),
   
  

   
  
  
                    box(  leafletOutput("map",height = "500"), background = "black", width = 12),
=======
body <- dashboardBody(fluidRow(
                      box(  leafletOutput("mymap",height = "500"), background = "black", width = 12)),
>>>>>>> parent of 00967bd... Add dropdown, remove zoomcontrol
  
                        # Also add some custom CSS to make the title background area the same
                        # color as the rest of the header.
                       
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                        ),
<<<<<<< HEAD
  
  
  
  
  

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
  
 
  h1("Use sliders to change importance of different score groups", style = "font-family: 'Roboto'; color: white; font-size: 20px; text-align: center;"),
  
  
  
  fluidRow( 
    column(1, align="center", tableOutput('values')
    ),
    column(2 ,offset = 5, align="center", tableOutput('table1')
    )          
=======
                      tags$div(
                        HTML(sprintf(html, "50% 100%", "4","string", "80% 100%", "65% 100%"))
                      )
>>>>>>> parent of 00967bd... Add dropdown, remove zoomcontrol
                         
                        )))



###############################################################################
# server
###############################################################################

server <- function(input, output, session) {
   
  acm_defaults <- function(map, x, y) 
    addCircleMarkers(map, x, y, radius=10, color="white", fillColor="orange", 
                     fillOpacity=0, opacity=1, weight=2, stroke=TRUE, layerId="OSM_id")
  
  
  plot.new() 
  
  points <- eventReactive(input$recalc, {
    cbind(df$X_wgs,df$Y_wgs)
  }, ignoreNULL = FALSE)
<<<<<<< HEAD
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
=======
  output$mymap <- renderLeaflet({
    leaflet() %>%
>>>>>>> parent of 00967bd... Add dropdown, remove zoomcontrol
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = FALSE)
      ) %>%
      addCircleMarkers(data = points(),color = getColor(
        
        
        ((input$Social/100)*mean_soc)+ ((input$Economy/100)*mean_econ) + ((input$Ecology/100)*mean_eco)
        
        
      ), group="locations", layerId = df$OSM_id , stroke = FALSE, fillOpacity = 1, label = htmlEscape(df$name)) 
  })
  
  
  observeEvent(input$map_marker_click, {
    ## Get the click info like had been doing
    
    
    print(input$map_marker_click)
    
    p <- input$map_marker_click


    proxy <- leafletProxy("map")

    if(p$id=="OSM_id"){
      proxy %>% removeMarker(layerId="OSM_id")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, 12) %>% acm_defaults(p$lng, p$lat)
    }
    
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

  
  observeEvent(input$City,  {
    
    df <- data.frame(
      Name = c("Presence of amenities", "NDVI range"
               
               
               
      ),
      
      Value = as.integer(c(input$Social,
                           input$Ecology),
                         stringsAsFactors = FALSE))
    
    print(input$City)
    
    # output$table1 <- renderTable(df)
    
  })
    

 
  

  
  

  

  
  # output$table1 <- renderTable(df)
  
  
  table_soc_score <- reactive({
    
    data.frame(
      Name = c("Presence of amenities", "NDVI range"
               
 
               
      ),
      
      Value = as.integer(c(input$Social,
                           input$Ecology),
      stringsAsFactors = FALSE)
      
    )})
  
  
  
  
 #  sliderValues <- reactive({
 #    
 #    data.frame(
 #      Name = c("Social",
 #               "Ecology",
 #               "Economy"
 #               ),
 #      
 #      Value = as.integer(c(input$Social,
 #                             input$Ecology,
 #                             input$Economy
 #                             ),
 #      stringsAsFactors = FALSE)
 #    
 # )})
  
 

  
  

  
  
  
  

  

  
  ### end of server ###
}



shinyApp(ui, server)




