
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
