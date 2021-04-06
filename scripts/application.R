library(shiny)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)

# get out of the scripts dir
setwd("..")

# load polygons
polys <- st_read("data/us_counties_hud_zip.geojson")

# color map to use
# NA is not shown because leaflet has a bug in continuous legends in shiny output
cmap <- colorNumeric("YlOrRd", domain=polys$br0_yr2017, na.color=rgb(0, 0, 0, 0))

ui <- navbarPage("33percent",
        tabPanel("Map",
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
            leafletOutput("map"),
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 50,
                          width = 330, height = "auto",
                          
                          h2("County Explorer"),
                          selectInput("aptChoice",
                                      "Apartment Type",
                                      c("Studio", "1 Bedroom", "2 Bedroom", "3 Bedroom")),
                          selectInput("yearChoice",
                                      "Year",
                                      2017:2020),
                          numericInput("monthlyIncome",
                                      "Monthly Income",
                                      4000, min=0),
                          submitButton(text="Apply"),
                          style="padding: 20px;"
            )
        ),
        tabPanel("About",
                 "It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like)")
)

server <- function(input, output, session) {
    output$map <- renderLeaflet({
        leaflet(polys, height="100%") %>%
            setView(-96, 37.8, 5) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addPolygons(
                weight=0.2,
                fillOpacity=0.7,
                fillColor = ~cmap(br0_yr2017),
                label = ~htmlEscape(cntyname)
            ) %>%
            addLegend(pal = cmap, values = ~br0_yr2017, opacity = 0.7, 
                      title = "Studio Rent in 2017",
                      position = "bottomleft", na.label="")
    })
}

shinyApp(ui, server)