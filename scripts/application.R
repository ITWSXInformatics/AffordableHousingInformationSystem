library(shiny)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)

# get out of the scripts dir
setwd("..")

# load polygons
polys <- st_read("data/us_counties_hud_zip.geojson")

# color maps
# NA is not shown because leaflet has a bug in continuous legends in shiny output
dollar_scale <- colorNumeric("YlOrRd", domain=c(0, 5200), na.color=rgb(0, 0, 0, 0))

rel_colors <- c(
    "#d7191c",
    "#fdae61",
    "#ffffbf",
    "#a6d96a",
    "#1a9641"
)

# binning shows the data in increments of 10%
rel_scale <- colorBin(rel_colors,
                      bins=length(rel_colors),
                      domain=c(0, 60), 
                      na.color=rgb(0, 0, 0, 0),
                      reverse=T)

# apartment type choices map onto # of bedrooms
br_choices <- c(
    "Studio"=0,
    "1 Bedroom"=1,
    "2 Bedroom"=2,
    "3 Bedroom"=3
)

# years of data available
yr_choices <- 2017:2020

# build the UI
uicontrols <- absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = FALSE, top = "auto", left = "auto", right = 20, bottom = 50,
                            width = 330, height = "auto",
                            
                            h2("County Explorer"),
                            selectInput("aptChoice",
                                        "Apartment Type",
                                        names(br_choices)),
                            selectInput("yearChoice",
                                        "Year",
                                        yr_choices),
                            numericInput("monthlyIncome",
                                         "Monthly Income",
                                         4000, min=0),
                            checkboxInput("vizChoice",
                                          "Show data relative to income",
                                          FALSE),
                            submitButton(text="Apply"),
                            style="padding: 20px;"
)

ui <- navbarPage("33percent",
        tabPanel("Map",
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
            leafletOutput("map"),
            uicontrols
            
        ),
        tabPanel("About",
                 "It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like)")
)

# build the server logic
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        # determine what data column is being visualized
        br_num <- br_choices[names(br_choices) == input$aptChoice]
        yr_num <- input$yearChoice
        column_name <- paste("br", br_num, "_yr", yr_num, sep="")
        data <- polys[[column_name]]
        # get the colormap to use
        if (input$vizChoice) {
            datatype <- "Percentage of Income"
            cmap <- rel_scale
            # recalc rents as a percentage of monthly income, cap those
            # above 60%
            data <- data / input$monthlyIncome * 100
            data[data > 60] <- 60
        } else{
            datatype <- "Rent"
            cmap <- dollar_scale
        }
        # Update the title
        title <- paste(input$aptChoice, datatype, "in", input$yearChoice)
        
        # bulid the map
        leaflet(polys, height="100%") %>%
            setView(-96, 37.8, 5) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addPolygons(
                weight=0.2,
                fillOpacity=0.7,
                fillColor = ~cmap(data),
                label = ~htmlEscape(cntyname)
            ) %>%
            addLegend(pal = cmap, values = ~data, opacity = 0.7, 
                      title = title,
                      position = "bottomleft", na.label="")
    })
}

shinyApp(ui, server)