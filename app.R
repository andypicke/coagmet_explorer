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
library(leaflet.extras)
#devtools::install_github("andypicke/rcoagmet")
library(rcoagmet)
library(dplyr)
library(DT)

# get station metadata for coag and nw networks
meta_coag <- rcoagmet::get_coagmet_meta(network = "coagmet") |> filter(active == "active")

meta_nw <- rcoagmet::get_coagmet_meta(network = "nw") |> filter(active == "active")

# combine all station metatdata into single data frame
meta_all <- rbind(meta_coag, meta_nw)

# get latest data for coag and nw networks
latest_data_coag <- rcoagmet::get_coagmet_data(station_id = "all", time_step = "latest") 

latest_data_nw <- rcoagmet::get_coagmet_data(station_id = "all", time_step = "latest", network = "nw") 

# select a subset of columns to keep
cols_to_keep <- c('station', 'date_and_time', 'air_temp', 'rh', 'dewpoint', 'wind', 'solar_rad')

latest_data_coag <- latest_data_coag |> 
  select(all_of(cols_to_keep))

latest_data_nw <- latest_data_nw |> 
  dplyr::rename(air_temp = avg_temp) |> # air temp is named differently in nw network
  select(all_of(cols_to_keep))

# combine latest data into single data frame
latest_data_all <- rbind(latest_data_coag, latest_data_nw) |>
  filter(air_temp < 130) # one station had crazy temp (400deg?)


# filter to data within last 2 hours
latest_time <- max(latest_data_all$date_and_time)
two_hours_ago <- latest_time - (2*3600)
latest_data_all <- latest_data_all |> filter(date_and_time > two_hours_ago)

# merge the metadata and latest data
data_merged <- meta_all |> left_join(latest_data_all, by = "station")


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




#-------------------------------------------------------------------------
# UI 
#-------------------------------------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("CoAgMet Weather Stations: Latest Data"),
  
  # Sidebar with a slider input for number of bins 
  
  
  tabsetPanel(
    tabPanel("Air Temperature",   leaflet::leafletOutput("temp_map")),
    tabPanel("Relative Humidity", leaflet::leafletOutput("rh_map")),
    tabPanel("Wind Speed",        leaflet::leafletOutput("windspeed_map")),
    tabPanel("Solar Radiation",   leaflet::leafletOutput("solarrad_map")),
    tabPanel("Data Table", DTOutput("data_table")),
    tabPanel("About", 
             h3("A Shiny App to Display CoAgMet Weather Data",),
             h5("Displays the latest data available during last 2 hours from the ",
                a(href = "https://coagmet.colostate.edu/", "CoAgMet"), 
                "weather station network"
             ),
             h5("Data is retrieved from the CoAgMet API using the ", 
                a(href = "https://github.com/andypicke/rcoagmet", "rcoagmet"),
                "package"
             ),
             h5("Source code for the app is availabe on ",
                a(href = "https://github.com/andypicke/coagmet_explorer", "github")
             )
    )
  ) # tabsetPanel
  
  #  ) # sidebarLayout
) # fluidPage




#-------------------------------------------------------------------------
# SERVER
#-------------------------------------------------------------------------

server <- function(input, output) {
  
  
  output$temp_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged, "air_temp", display_name = "Air Temperature <br> [&#176; F]") # &#176; = degree symbol in html
  })
  
  output$rh_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged, "rh", display_name = "Rel. Humidity <br> [%]")
  })
  
  output$windspeed_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged, "wind", display_name = "Wind Speed <br> [mph]")
  })
  
  output$solarrad_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged, "solar_rad", display_name = "Solar Radiation <br> [W/m<sup>2</sup>]")
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
