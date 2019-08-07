#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Monastery Location by Province" = "Points",
  "Choropleth Map by Province " = "Chloro",
  "Seismic Zones by Communes" = "Seismic"
)

#UI Page

  navbarPage("Monasteries of Sicily", id="nav",
             
             tabPanel("Interactive map",
                      div(class="outer",
                         
                           tags$head(
                            # Include custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Monastery Explorer"),
                                        selectInput("mapstyle", "Map Style", vars),
                                        selectInput("colors", "Color Scheme",
                                                    rownames(subset(brewer.pal.info, category %in% c("seq", "div", "qual")))
                                        ),
                                        style = "opacity: 0.80; z-index: 1000;")
                          # # Shiny versions prior to 0.11 should use class = "modal" instead.
                          # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          #               width = 330, height = "auto", h2("Monasteries"))
                          
                      )
             ),
          
             
             tabPanel("Data explorer",
                      hr(),
                      DT::dataTableOutput("cleantable")
             ),
        

             conditionalPanel("false", icon("crosshair"))
  )
  


