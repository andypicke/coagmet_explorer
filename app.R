#-------------------------------------------------------------------------
#
# coagmet_explorer
#
# A Shiny app to visualize data from the CoAgMet Weather station network in CO
#
# Andy Pickering
# andypicke@github.com
# 2024/06/06
#
#-------------------------------------------------------------------------

library(shiny)
library(leaflet)
library(rcoagmet)
library(dplyr)
library(DT)

meta_coag <- rcoagmet::get_coagmet_meta(network = "coagmet") |> filter(active == "active")

latest_data_coag <- rcoagmet::get_coagmet_data(station_id = "all", time_step = "latest") 


# check that data is within last 2 hours
latest_time <- max(latest_data_coag$date_and_time)
two_hours_ago <- latest_time - (2*3600)
latest_data_coag <- latest_data_coag |> filter(date_and_time > two_hours_ago)

data_merged <- meta_coag |> left_join(latest_data_coag, by = "station")


# --- define a function to make title for leaflet map
# From: https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map
leaf_title <- function(var_to_plot){
  tag.map.title <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 20%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 15px;
  }
"))
  
  title <- htmltools::tags$div(
    tag.map.title, 
    htmltools::HTML(paste("CoAgMet :  ",var_to_plot))
  )
  
}


#-------------- Define function to make leaflet map of data

map_data_leaflet <- function(var_to_plot){
  
  dat_to_plot <- data_merged |>
    dplyr::select(c(name, network, date_and_time, longitude_deg_e, latitude_deg_n))
  
  dat_to_plot$plot_var <- pull(data_merged[, which(names(data_merged) == var_to_plot)])
  
  if (var_to_plot == "rh") {
    dat_to_plot$plot_var <- dat_to_plot$plot_var * 100
    #  pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
    pal <- colorNumeric(palette = "YlOrRd", domain = dat_to_plot$plot_var)
  } else {
    pal <- colorNumeric(palette = "YlOrRd", domain = dat_to_plot$plot_var)
  }
  
  m <- dat_to_plot |>
    leaflet() |>
    addTiles() |>
    addCircleMarkers(lng = ~longitude_deg_e, lat = ~latitude_deg_n, 
                     label = paste(dat_to_plot$name, ": ",dat_to_plot$plot_var),
                     color = "grey",
                     weight = 1,
                     fillColor = ~pal(plot_var),
                     fillOpacity = 0.5,
                     popup = paste(dat_to_plot$name, "<br>",
                                   "Network: ", dat_to_plot$network, "<br>",
                                   dat_to_plot$date_and_time
                                   )
    ) |>
    addLegend(values = ~plot_var,
              pal = pal,
              title = var_to_plot)
  
  
  title <- leaf_title(var_to_plot)
  
  m <- addControl(map = m, title, position = "topleft", className = "map-title")
  
  
}


#-------------------------------------------------------------------------
# UI 
#-------------------------------------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("CoAgMet Weather Stations: Latest Data"),
  
  # Sidebar with a slider input for number of bins 
  
  
  tabsetPanel(
    tabPanel("Air Temperature",   leaflet::leafletOutput("temp_map", width = "100%")),
    tabPanel("Relative Humidity", leaflet::leafletOutput("rh_map",   width = "100%")),
    tabPanel("Wind Speed", leaflet::leafletOutput("windspeed_map", width = "100%")),
    tabPanel("Solar Radiation", leaflet::leafletOutput("solarrad_map", width = "100%")),
    tabPanel("Data Table", DTOutput("data_table")),
    tabPanel("About", h3("This Shiny App Displays CoAgMet Weather Data",),
             a(href = "https://coagmet.colostate.edu/", "CoAgMet"),
             h5(a(href = "https://github.com/andypicke/rcoagmet", "rcoagmet")),
             h5("Disclaimer...")
    )
  ) # tabsetPanel
  
  #  ) # sidebarLayout
) # fluidPage




#-------------------------------------------------------------------------
# SERVER
#-------------------------------------------------------------------------

server <- function(input, output) {
  
  
  output$temp_map <- leaflet::renderLeaflet({
    map_data_leaflet("air_temp")
  })
  
  output$rh_map <- leaflet::renderLeaflet({
    map_data_leaflet("rh")
  })
  
  output$windspeed_map <- leaflet::renderLeaflet({
    map_data_leaflet("wind")
  })
  
  output$solarrad_map <- leaflet::renderLeaflet({
    map_data_leaflet("solar_rad")
  })
  
  output$data_table <- renderDT(
    {
      data_merged |>
        datatable(
          rownames = FALSE,
          extensions = c("Responsive", "Buttons"),
          options = list(
            buttons = c("excel", "csv", "pdf"),
            dom = "Bftip"
          )
        )
    },
    server = FALSE
  )
  
}


#-------------------------------------------------------------------------
# Run the application 
#-------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
