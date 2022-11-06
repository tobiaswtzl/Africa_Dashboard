###############################################################################
##Global File
##############################################################################

#checks if pacman package exists, if not install
if (!require("pacman")) install.packages("pacman")

#install/load packages
pacman::p_load("shiny",
               "dplyr",
               "here",
               "leaflet",
               "shinyWidgets",
               "plotly",
               "shinydashboard"
)

#set wd
here::i_am("global.R")

#load data
load(here("data", "data_memphis.rdata"))
load(here("data", "data_boxes.rdata"))
load(here("data", "data_referate.rdata"))
load(here("data", "map_data.rdata"))
load(here("data", "ipc_glossar.rdata"))


################################################################################
###Functions
################################################################################

###################
#Plot aktuelle Lage
###################
plot_aktuelle_lage <- function(data, variable, custom_title, hover_suffix= "", text_caption) {
  
  #remove 0 values
  data[data==0] <- NA
  
  #plot
  plotly::plot_ly(data = data,
                  x = ~.data[[variable]],
                  y = ~reorder(country_de, .data[[variable]]),
                  text = ~.data[[variable]],
                  texttemplate= paste("%{x}", hover_suffix),
                  textposition = 'auto',
                  type = 'bar',
                  hoverinfo = 'y',          
                  orientation = 'h') %>% 
    
    #customise layout
    plotly::layout(
      title = list(
        text = custom_title,
        size = 20
      ),
      xaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = ""
      ),
      annotations = 
        list(x = 1,
             y = 0,
             text = paste0("Quelle: ", text_caption), 
             showarrow = FALSE,
             xref='paper',
             yref='paper', 
             xanchor='right',
             yanchor='auto',
             xshift=0, 
             yshift=0
        )
    ) %>% 
    plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "resetScale2d", "pan2d", "zoom2d", "autoScale2d",
                                              "select2d", "lasso2d", "hoverCompareCartesian", "hoverClosestCartesian"), displaylogo = FALSE)
  
}

###################
#Plot Africa Map
###################

map_africa <- function(data, bins, displayed_var, labels, title, lab_form = " %") {
  
  #prepare colours
  pal <- leaflet::colorBin(
    "YlOrRd",
    domain = displayed_var,
    bins = bins
  )
  
  #create leaflet object
  leaflet::leaflet(
    data = data,
    options = leafletOptions(
      minZoom = 3.4,
      maxZoom = 7,
      dragging = TRUE
    )
  ) %>%
    
    #define view
    leaflet::setView(
      lng = 13.1021,
      lat = 0.2812,
      zoom = 3.7
    ) %>%
    
    #polygon data
    leaflet::addProviderTiles(
      "MapBox",
      options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
      )
    ) %>% 
    
    #create coloured ploygons
    leaflet::addPolygons(
      fillColor = ~pal(displayed_var),
      layerId = ~ADMIN,
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      
      #define what happens when mouse hovers over polygons
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      
      #add labels
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")      
    ) %>% 
    
    #add legend
    leaflet::addLegend(
      pal = pal,
      values = ~displayed_var,
      opacity = 0.7,
      title = title,
      position = "bottomright",
      na.label = "Keine aktuellen Daten",
      labFormat = labelFormat(suffix = lab_form)
    )
  
}

###################
# prepare data for Pie Chart
###################

pie_chart_data_prep <- function(data, group_by_var) {
  
  data %>%
    dplyr::group_by({{group_by_var}}) %>%
    dplyr::summarise(financial_commitment_in_mio_per_variable = sum(financial_commitment_in_mio))
}


###################
#Area Chart data prep
###################
area_chart_data_prep <- function(data, group_by_var) {
  
  data %>%
    dplyr::group_by({{group_by_var}}, year) %>% 
    
    #sum
    dplyr::summarise(financial_commitment_in_mio_per_variable = sum(financial_commitment_in_mio)) %>% 
    
    # #pivot for plotting
    tidyr::pivot_wider(names_from = {{group_by_var}},
                       values_from = financial_commitment_in_mio_per_variable) 
}
