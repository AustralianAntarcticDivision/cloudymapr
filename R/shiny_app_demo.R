#' Demonstration Shiny app
#'
#' @return A Shiny app object
#'
#' @examples
#' if (interactive()) vl_demo()
#'
#' @export
vl_demo <- function() {

    qcd <- system.file("extdata/demo/quantarctica_cache", package = "cloudymapr", mustWork = TRUE)
    quiet_reader <- function(...) sf::st_read(..., quiet = TRUE) ## shoosh
    cst <- st_geometry(quiet_reader(file.path(qcd, "ADD_Coastline_medium_res_polygon.shp")))
    ccamlr_areas <- st_geometry(quiet_reader(file.path(qcd, "CCAMLR_StatisticalAreas.shp")))

    ## plan(multisession(workers = 8))

    cache_obj <- cachem::cache_mem()
    ## pre-seed the cache for faster startup
    d <- dir(system.file("extdata/demo/cache_seed", package = "cloudymapr", mustWork = TRUE), full.names = TRUE)
    for (f in d) cache_obj$set(basename(f), readRDS(f))

    target_crs <- "EPSG:3031"
    lonlat_crs <- "EPSG:4326"

    bluemap <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF","grey99", "grey90", "grey92", "grey94", "grey96", "white"))(100)

    ## env/background layers
    ## 3 examples:
    ## - Basemap is the ArcGIS World Imagery, which is RGB imagery served through a web map tile server
    ## - GEBCO DEM is a global DEM in COG format, on lat-long projection
    ## - NSIDC Sea ice is also a COG, but in polar stereo projection. Values in this range from 0-2550 where 0-1000 are sea ice concentration x 10, and 2550 is land mask. So we clip the values to 0-1001 and render 1001 as transparent
    raster_cat <- tribble(~type, ~name, ~dsn, ~cmap, ~zlims,
                          "raster_image_rgb", "Basemap", "WMTS:http://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/WMTS/1.0.0/WMTSCapabilities.xml", NULL, NULL,
                          "raster_data", "GEBCO DEM", sds::gebco23(), bluemap, c(-8000, 5000),
                          "raster_data", "NSIDC Sea Ice", sds::nsidc_seaice(as.Date("2024-02-07")), c(hcl.colors(100, "Blue-Yellow 2"), c("#FFFFFF00")), c(0, 1001))

    ## some point data
    sx <- readRDS(system.file("extdata/demo/demo_data.rds", package = "cloudymapr"))
    ## duplicate lat and lon, just so they appear in the table when shown
    sx$longitude <- sx$lon
    sx$latitude <- sx$lat
    sx <- st_as_sf(sx, coords = c("lon", "lat"))
    st_crs(sx) <- lonlat_crs
    sx <- st_transform(sx, crs = target_crs)
    sx <- bind_cols(sx, st_coordinates(sx))

    ui <- fluidPage(
        tags$hr(),
        fluidRow(column(2, selectInput("bg", label = "Background", choices = raster_cat$name),
                        checkboxInput("cst", label = "Coastline", value = TRUE),
                        checkboxInput("ccamlr_areas", label = "CCAMLR Areas")
                        ),
                 column(10, fluidRow(column(8, style = "overflow-y:hidden; height:65vh;", vl_map_ui("mymap", view_wh = c(40, 60))),
                                     column(4, DT::dataTableOutput("site_tbl")))##,
                        )),
        vl_map_ui_postamble()
    )

    server <- function(input, output) {
        layerdef <- reactive({
            bg <- if (is.null(input$bg) || !input$bg %in% raster_cat$name) raster_cat[raster_cat$name == "Basemap", ] else raster_cat[raster_cat$name == input$bg, ]
            list(bg %>% mutate(z = 1L), ## raster layer using inbuilt renderer
                 list(fun = function(xlim, ylim) { ## custom layer using own plot function
                     if (isTRUE(input$cst)) {
                         plot(0, 0, type = "n", axes = FALSE, xlim = xlim, ylim = ylim)
                         plot(cst, border = "black", col = NA, add = TRUE, xlim = xlim, ylim = ylim)
                         points(sx$X, sx$Y, pch = 21, bg = "green")
                         TRUE
                     } else {
                         FALSE
                     }
                 }, z = 2),
                 list(fun = function(xlim, ylim) { ## custom layer using own plot function
                     if (isTRUE(input$ccamlr_areas)) {
                         plot(0, 0, type = "n", axes = FALSE, xlim = xlim, ylim = ylim)
                         plot(ccamlr_areas, border = "blue", col = NA, add = TRUE, xlim = xlim, ylim = ylim)
                         TRUE
                     } else {
                         FALSE
                     }
                 }, z = 3)
                 )
        })

        vl_obj <- vl_map_server("mymap", layerdef = layerdef, target_crs = target_crs, cache = cache_obj)

        ## handle click events
        observeEvent(vl_obj$click(), {
            req(vl_obj$click())
            idx <- which.min(abs(sx$X - vl_obj$click()[1]) + abs(sx$Y - vl_obj$click()[2]))
            output$site_tbl <- DT::renderDataTable({
                tbl_cols <- setdiff(names(sx), c("X", "Y", "geometry"))
                temp <- as.data.frame(sx[idx, ])[, tbl_cols] %>% mutate(across(everything(), function(z) as.character(if (is.numeric(z)) round(z, 3) else z)))
                temp <- tibble(Variable = names(temp), Value = unlist(temp))
                dt_opts <- list(sDom = '<"top">t<"bottom">r')##, ## filters and paging options, no i, p, or l
                DT::datatable(temp, rownames = FALSE, options = dt_opts, selection = "none", filter = "none", class = "display")
            })
        })

    }

    shinyApp(ui, server)
}
