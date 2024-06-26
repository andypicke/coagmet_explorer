
#-------------- Define function to make leaflet map of data

map_data_leaflet <- function(data_merged, var_to_plot, display_name = ""){
  
  dat_to_plot <- data_merged |>
    dplyr::select(c(name, network, date_and_time, longitude_deg_e, latitude_deg_n))
  
  dat_to_plot$plot_var <- pull(data_merged[, which(names(data_merged) == var_to_plot)]) 
  
  dat_to_plot <- dat_to_plot |>
    filter(!is.na(plot_var))
  
  if (var_to_plot == "rh") {
    dat_to_plot$plot_var <- dat_to_plot$plot_var * 100
    #  pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
    pal <- colorNumeric(palette = "YlOrRd", domain = dat_to_plot$plot_var)
  } else {
    pal <- colorNumeric(palette = "YlOrRd", domain = dat_to_plot$plot_var)
  }
  
  # make labels to show on hover
  # see https://stackoverflow.com/questions/30964020/popup-when-hover-with-leaflet-in-r
  labs <- as.list(paste(dat_to_plot$name, "<br>",
                        "Network: ", dat_to_plot$network, "<br>",
                        dat_to_plot$date_and_time, "<br>",
                        var_to_plot, " : ", dat_to_plot$plot_var)
  )
  
  m <- dat_to_plot |>
    leaflet() |>
    addTiles() |>
    fitBounds(lng1 = -109.06025, lat1 = 36.99243, lng2 = -102.04152, lat2 = 41.00344) |>
    addCircleMarkers(lng = ~longitude_deg_e, lat = ~latitude_deg_n, 
                     label = lapply(labs,HTML),
                     color = "grey",
                     weight = 1,
                     fillColor = ~pal(plot_var),
                     fillOpacity = 0.5
    )  |>
    addLegend(values = ~plot_var,
              pal = pal,
              title = display_name) |>
    addTerminator(group = "daylight") |> # add daylight shading curve to map
    addLayersControl(overlayGroups = c("daylight"), # add toggle for daylight
                     options = layersControlOptions(collapsed = FALSE)) |>
    leaflet.extras::addResetMapButton() # add button to reset map to original position
  
  
  
  #title <- leaf_title(var_to_plot)
  #m <- addControl(map = m, title, position = "topleft", className = "map-title")
  
}