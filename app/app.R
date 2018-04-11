library(shiny)
library(ggmap)
library(leaflet)

ui <- shinyUI(bootstrapPage(
  leafletOutput("map")
))

server <- shinyServer(function(input, output, session) {
  ## One alternative: store circles data?
  ## I dont actually implement this, but you would do this in the observer as well
  dat <- reactiveValues(circs = data.frame(lng=numeric(), lat=numeric()))
  
  ## Make your initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -43.1729, lat = -22.9068, zoom = 11) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) 
  })
  
  ## Observe mouse clicks and add circles
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    address <- revgeocode(c(clng,clat))
    
    ## Add the circle to the map proxy
    ## so you dont need to re-render the whole thing
    ## I also give the circles a group, "circles", so you can
    ## then do something like hide all the circles with hideGroup('circles')
    leafletProxy('map') %>% # use the proxy to save computation
      addCircles(lng=clng, lat=clat, group='circles',
                 weight=1, radius=100, color='black', fillColor='orange',
                 popup=address, fillOpacity=0.5, opacity=1)
  })
  
})

shinyApp(ui=ui, server=server)