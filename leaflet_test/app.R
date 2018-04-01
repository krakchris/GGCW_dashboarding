library(shiny)
library(leaflet)
library(shinydashboard)


header <- dashboardHeader(title = "Global Green City Watch",titleWidth = 350)

sidebar <- dashboardSidebar( actionButton("recalc", "New points"),disable = TRUE)

body <- dashboardBody(   leafletOutput("mymap"),
                         p(),
                         
                         # Also add some custom CSS to make the title background area the same
                         # color as the rest of the header.
                         
                         tags$head(tags$style(HTML('
                                                   /* logo */
                                                   .skin-blue .main-header .logo {
                                                   background-color: #000000;
                                                   }
                                                   
                                                   /* logo when hovered */
                                                   .skin-blue .main-header .logo:hover {
                                                   background-color: #000000;
                                                   }
                                                   
                                                   /* navbar (rest of the header) */
                                                   .skin-blue .main-header .navbar {
                                                   background-color: #000000;
                                                   }        
                                                   
                                                   /* main sidebar */
                                                   .skin-blue .main-sidebar {
                                                   background-color: #ffffff;
                                                   }
                                                   
                                                   /* active selected tab in the sidebarmenu */
                                                   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                                   background-color: #000000;
                                                   }
                                                   
                                                   /* other links in the sidebarmenu */
                                                   .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                                   background-color: #000000;
                                                   color: #000000;
                                                   }
                                                   
                                                   /* other links in the sidebarmenu when hovered */
                                                   .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                                   background-color: #000000;
                                                   }
                                                   /* toggle button when hovered  */                    
                                                   .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                                   background-color: #ffffff;
                                                   }
                                                   ')))
                         
                        )

dashboardPage(header, sidebar, body)


ui <- dashboardPage( header,sidebar,body
                     
)




r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(50) * 2 + 4, rnorm(50) + 52)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(data = points(),color = 'green')
  })
}

shinyApp(ui, server)