library(shiny)
library(leaflet)
library(shinydashboard)

city = 'Amsterdam'



mtcars <- mtcars[1:5,]

header <- dashboardHeader(title = city,titleWidth = 300,disable = TRUE)

sidebar <- dashboardSidebar( actionButton("recalc", "New points"),disable = TRUE)

body <- dashboardBody(fluidRow(
                      box(  leafletOutput("mymap",height = "500"), background = 'black', width = 12)),
  
                        fluidRow(
                          box(plotOutput("plot1"),
                          background = 'black', width = 6
                        )),
                        p(),
                       
                        # Also add some custom CSS to make the title background area the same
                        # color as the rest of the header.
                       
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                        )
                         
                        )

dashboardPage(header, sidebar, body)


ui <- dashboardPage( header,sidebar,body
                     
)




r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


server <- function(input, output, session) {
  
  output$plot1 <- renderPlot(
    dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7,
             main="Gas Milage for Car Models", 
             xlab="Miles Per Gallon",color = 'black')
  )
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(50) * 1 + 4, rnorm(50) + 52)
  }, ignoreNULL = FALSE)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = FALSE)
      ) %>%
      addCircleMarkers(data = points(),color = 'green')
  })
}

shinyApp(ui, server)