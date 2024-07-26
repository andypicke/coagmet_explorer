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


#---------------------------------
# Load libraries
#---------------------------------

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
#devtools::install_github("andypicke/rcoagmet")
library(rcoagmet)
library(dplyr)
library(DT)



#---------------------------------
# Load weather data
#---------------------------------

# get station metadata for CoAgMet and NW networks
meta_coag <- rcoagmet::get_coagmet_meta(network = "coagmet") |> filter(active == "active")
meta_nw <- rcoagmet::get_coagmet_meta(network = "nw") |> filter(active == "active")

# combine station metadata into single data frame
meta_all <- rbind(meta_coag, meta_nw)

# get latest data for CoAgMet and NW networks
latest_data_coag <- rcoagmet::get_coagmet_data(station_id = "all", time_step = "latest") 

latest_data_nw <- rcoagmet::get_coagmet_data(station_id = "all", time_step = "latest", network = "nw") 

# select a subset of columns to keep
cols_to_keep <- c('station', 'date_and_time', 'air_temp', 'rh', 'dewpoint', 'wind', 'solar_rad', 'rso')

latest_data_coag <- latest_data_coag |> 
  select(all_of(cols_to_keep))

latest_data_nw <- latest_data_nw |> 
  dplyr::rename(air_temp = avg_temp) |> # air temp is named differently in nw network
  select(all_of(cols_to_keep))

# combine latest data into single data frame, filter out some anomalous values
latest_data_all <- rbind(latest_data_coag, latest_data_nw) 

latest_data_all <- latest_data_all |> 
  mutate(air_temp = if_else(air_temp > -30, air_temp, NA)) |>
  mutate(air_temp = if_else(air_temp < 130, air_temp, NA)) |>
  mutate(rh = if_else(rh >= 0, rh, NA)) |>
  mutate(rh = if_else(rh <= 1, rh, NA)) |>
  dplyr::mutate(perc_sun = round(100*solar_rad/rso,2)) # add percent solar radiation

# filter to data within last 2 hours
#latest_time <- max(latest_data_all$date_and_time)
latest_time <- lubridate::now(tzone = "MST")
two_hours_ago <- latest_time - (2*3600)
latest_data_all <- latest_data_all |> filter(date_and_time > two_hours_ago)

# merge the metadata and latest data
data_merged <- meta_all |> left_join(latest_data_all, by = "station")

# add a column for datetime in local (Mountain) timezone (either MST or MDT depending on date..)
data_merged$date_and_time_local <- lubridate::with_tz(data_merged$date_and_time, tz = "US/Mountain")

#---------------------------------
#---------------------------------



#-------------------------------------------------------------------------
# UI 
#-------------------------------------------------------------------------

ui <- page_fillable(
  
  card(
    card_header(paste0("CoAgMet Weather Stations - Latest Data as of: ", 
                       format(lubridate::now(tzone = "US/Mountain"),"%Y-%m-%d %H:%M:%S %Z"))),
    
    # windowTitle = "Latest CoAgMet Data",
    
    navset_card_underline(
      # title = "Tabs",
      
      # TAB: Leaflet map
      nav_panel("Temperature", leaflet::leafletOutput("temp_map")),
      
      # TAB: Leaflet map
      nav_panel("Rel. Humidity", leaflet::leafletOutput("rh_map")),
      
      # TAB: Leaflet map
      nav_panel("Wind Speed", leaflet::leafletOutput("windspeed_map")),
      
      # TAB: Leaflet map
      nav_panel("Solar Radiation", leaflet::leafletOutput("solarrad_map")),
      
      # TAB: Leaflet map
      nav_panel("% Max Solar Radiation", leaflet::leafletOutput("perc_sun_map")),
      
      # TAB: Data Table
      nav_panel("Data Table", DTOutput("data_table")),
      
      # TAB: About
      nav_panel("About", 
                h3("A Shiny App to Display CoAgMet Weather Data",),
                h5("Displays the latest data available within the last 2 hours from the ",
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
      ) #nav_panel
    )#navset_card_underline
  )#card
)#page_fillable




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

  output$perc_sun_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged, "perc_sun", display_name = "% of Max Solar Radiation <br> [%]")
  })
  
  output$data_table <- renderDT(
    {
      data_merged |>
        select(station, name, location, date_and_time, air_temp, rh, solar_rad, perc_sun, wind) |>
        datatable(
          rownames = FALSE,
          extensions = c("Responsive", "Buttons"),
          options = list(
            buttons = c("excel", "csv", "pdf"),
            dom = "Bftip"
          )
        ) |>
        DT::formatDate(columns = 4, method = "toString")
    },
    server = FALSE
  )
  
}


#-------------------------------------------------------------------------
# Run the application 
#-------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
