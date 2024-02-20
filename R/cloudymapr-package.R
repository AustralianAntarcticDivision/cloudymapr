#' \pkg{cloudymapr}
#'
#' Cloud-focused Antarctic mapping functionality.
#'
#' @name cloudymapr
#' @aliases cloudymapr-package
#' @docType package
#' @importFrom abind abind
#' @importFrom dplyr across bind_cols everything mutate tribble
#' @importFrom future future
#' @importFrom graphics par points text
#' @importFrom grDevices col2rgb dev.off hcl.colors png
#' @importFrom leaflet addTiles addWMSTiles leaflet leafletCRS leafletOptions setView tileOptions WMSTileOptions
#' @importFrom magrittr %>%
#' @importFrom promises then
#' @importFrom sf st_as_sf st_coordinates `st_crs<-` st_geometry st_transform
#' @importFrom shiny actionButton addResourcePath checkboxInput column fluidPage fluidRow HTML icon isolate moduleServer NS observe observeEvent onSessionEnded reactive reactiveVal req selectInput shinyApp singleton tags tagList
#' @importFrom stats runif
#' @importFrom tibble tibble
#' @importFrom vapour gdal_raster_data
NULL
