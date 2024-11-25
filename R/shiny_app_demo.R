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

    cache_obj <- cachem::cache_mem()
    ## pre-seed the cache for faster startup
    d <- dir(system.file("extdata/demo/cache_seed", package = "cloudymapr", mustWork = TRUE), full.names = TRUE)
    for (f in d) cache_obj$set(basename(f), readRDS(f))

    target_crs <- "EPSG:3031"
    lonlat_crs <- "EPSG:4326"

    bluemap <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF","grey99", "grey90", "grey92", "grey94", "grey96", "white"))(100)
    sstmap <- pals::ocean.thermal(100)

    ## env/background layers
    raster_cat <- tribble(~type, ~name, ~dsn, ~cmap, ~zlims, ~extra_args,
                          "raster_image_rgb", "EO Basemap", "/vsicurl/https://data.raadsync.cloud.edu.au/raad/public/idea.public/basemap/vlrast_eo.tif", NULL, NULL, NULL,
                          "raster_image_rgb", "ESRI Basemap", "WMTS:http://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/WMTS/1.0.0/WMTSCapabilities.xml", NULL, NULL, NULL,
                          "raster_image_rgb", "BAS Basemap", "WMTS:https://tiles.arcgis.com/tiles/tPxy1hrFDhJfZ0Mf/arcgis/rest/services/Antarctica_and_the_Southern_Ocean/MapServer/WMTS/1.0.0/WMTSCapabilities.xml", NULL, NULL, NULL,
                          "raster_image_grey", "REMA Hillshade", "/vsicurl/https://data.raadsync.cloud.edu.au/raad/public/idea.public/basemap/rema_mosaic_100m_v2.0_filled_cop30_browse.tif", NULL, NULL, list(trns = 0), ## range 1-255, nodata = 0 gray.colors(101, start = 0, end = 1)
                          "raster_data", "GEBCO DEM", sds::gebco23(), bluemap, c(-8000, 5000), NULL,
                          "raster_data", "NSIDC Sea Ice", sds::nsidc_seaice(as.Date("2024-02-07")), c(hcl.colors(100, "Blue-Yellow 2"), c("#FFFFFF00")), c(0, 1001), NULL,
                          "raster_data", "Predator bioregions", "/vsicurl/https://data.source.coop/scar/distant/reisinger_et_al-2022/Re2022-bioregions_cog.tif", pals::glasbey(17), c(1, 17), NULL,
                          "raster_data", "GHRSST", "/vsicurl/https://data.raadsync.cloud.edu.au/raad/public/idea.public/ghrsst/2023/20230101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.tif", sstmap, c(-1.8, 25), NULL,
                          "raster_data", "Sea ice algae productivity",  "/vsicurl/https://data.source.coop/scar/distant/pinkerton_hayward-2021/Pi2021-annual_cog.tif", hcl.colors(palette = "Greens", n = 51, rev = TRUE), c(0, 10), NULL)

    ## metadata about the raster layers to show in the app
    raster_meta <- tribble(~name, ~citation, ~details,
                           "EO Basemap", tags$span("NASA Earth Observatory map by Joshua Stevens using data from NASA’s MODIS Land Cover, the Shuttle Radar Topography Mission (SRTM), the General Bathymetric Chart of the Oceans (GEBCO), and Natural Earth boundaries.", tags$a(href = "https://visibleearth.nasa.gov/images/147190/explorer-base-map", "https://visibleearth.nasa.gov/images/147190/explorer-base-map")), "An RGB COG (geotiff) in long-lat projection, served by a standard web server and reprojected on the fly",
                           "ESRI Basemap", tags$span("ESRI World Imagery basemap. https://www.arcgis.com/home/item.html?id=10df2279f9684e4a9f6a7f08febac2a9"), "ESRI's World Imagery basemap, which is RGB imagery served through a web map tile server",
                           "BAS Basemap", tags$span("Basemap of Antarctic and Southern Ocean combining hillshade and bathymetry by the British Antarctic Survey. https://www.arcgis.com/home/item.html?id=435e23642bf94b83b07d1d3fc0c5c9d5"), "RGB image tiles in polar stereographic projection, served by a web map server",
                           "REMA Hillshade", tags$span("Reference Elevation Model of Antarctica v2. https://doi.org/10.7910/DVN/EBW8UC"), "Greyscale COG (geotiff) in polar stereographic projection, served by a standard web server. Rendered to greyscale image with transparency",
                           ##"REMA Elevation", "A COG (geotiff) of data values in polar stereographic projection, served by a standard web server and rendered on the fly using a custom colour palette with multiple transparent entries", ## first 2 palette entries fully transparent
                           "GEBCO DEM", tags$span("GEBCO global bathymetry. https://doi.org/10.5285/f98b053b-0cbc-6c23-e053-6c86abc0af7b"), "A global DEM in COG format, on lat-long projection",
                           "NSIDC Sea Ice", tags$span("Daily sea ice concentration (7-Feb-2024) from passive microwave satellite. https://doi.org/10.5067/MPYG15WAA4WX"), "A COG (geotiff) of data values in polar stereographic projection. Served by the NOAA web server and rendered on the fly using a custom colour palette. Data values range from 0-2550 where 0-1000 are sea ice concentration x 10, and 2550 is land mask. We clip the values to 0-1001 and render 1001 as transparent",
                           "GHRSST", tags$span("Sea surface temperature (1-Jan-2023) from GHRSST. https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1"), "A COG (geotiff) of data values in long-lat projection, served by a standard web server and reprojected and rendered on the fly using a custom colour palette. NaN values over land are plotted as transparent",
                           "Predator bioregions", tags$span("Southern Ocean predator bioregions from Reisinger et al. (2022) https://doi.org/10.1016/j.biocon.2022.109630"), "A COG (geotiff) of discrete values (bioregion numbers) in long-lat projection, served from AWS S3 object storage",
                           "Sea ice algae productivity", tags$span("Annual mean sea ice algae productivity from Pinkerton & Hayward (2021) https://doi.org/10.1016/j.jmarsys.2021.103576"), "A COG (geotiff) of data values in polar stereographic projection, served from AWS S3 object storage and rendered on the fly using a custom colour palette") ## NB the direct S3 link to this is "/vsicurl/https://s3.us-west-2.amazonaws.com/us-west-2.opendata.source.coop/scar/distant/pinkerton_hayward-2021/Pi2021-annual_cog.tif"
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
                        checkboxInput("ccamlr_areas", label = "CCAMLR Areas"),
                        tags$hr(),
                        uiOutput("bg_dialog")),
                 column(10, style = "overflow-y:hidden; height:65vh;", vl_map_ui("mymap", view_wh = c("75vw", "60vh")))),
        fluidRow(column(6, offset = 6, DT::dataTableOutput("site_tbl"))),
        vl_map_ui_postamble()
    )

    server <- function(input, output) {
        layerdef <- reactive({
            bg <- if (is.null(input$bg) || !input$bg %in% raster_cat$name) raster_cat[raster_cat$name == "EO Basemap", ] else raster_cat[raster_cat$name == input$bg, ]
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

        vl_obj <- vl_map_server("mymap", layerdef = layerdef, target_crs = target_crs, cache = cache_obj, initial_view = list(tiles_per_side = 2L, extent = c(-1, 1, -1, 1) * 2048e4, res = 32e3))

        ## raster metadata
        output$bg_dialog <- renderUI({
            req(input$bg)
            idx <- which(raster_meta$name == input$bg)
            if (length(idx) == 1) tags$p("Background imagery:", raster_meta$citation[idx][[1]]) else NULL
        })

        ## handle click events
        observeEvent(vl_obj$click(), {
            req(vl_obj$click())
            idx <- which.min(abs(sx$X - vl_obj$click()[1]) + abs(sx$Y - vl_obj$click()[2]))
            ## TODO fix this, it's just snapping to the nearest data point, which might be a long way from the click
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
