#' Vapourlayer map shiny module
#'
#' @param id string: HTML element id
#' @param view_wh numeric: two-element vector giving the width and height of the map widget as percentages of the browser window width and height
#' @param image_wh integer: image size in pixels. Depending on the rendering mechanism, the native image size might not actually be this, but the browser will scale to this size
# @param min_scale scalar: the smallest cell size (in projected units), used when maximally zoomed in
## no, min_scale should be determined by the data. Or maybe allow min_scale to be specified but by default it's NA, in which case it falls back to the native data res
#' @param initial_view list: a named list with components:
#' * tiles_per_side integer: e.g. a value of 2 means that we will generate the image as 2x2 set of tiles
#' * extent numeric: the initial image extent c(xmin, xmax, ymin, ymax) in projected coordinates
#' * res numeric: the resolution (in m) to use for the image when shown at its intial extent
# @param initial_zoom integer: initial zoom level
# @param zoom_range integer: two-element integer vector giving the minimum and maximum zoom levels
#' @param layerdef reactive: TBD
#' @param target_crs string: target projection CRS string
#' @param cache logical or cachem: either TRUE/FALSE to use/not the default memory-based cache, or a `cachem` object (as returned by e.g. [cachem::cache_mem()])
#'
#' @return The UI and server components of a Shiny module
#'
# @examples
#'
#' @export
#' @rdname vl_map_module
vl_map_ui <- function(id, view_wh = c(40, 40)) {
    tagList(
        tags$head(singleton(htmltools::includeCSS(system.file("extdata/css/vapourlayer.css", package = "cloudymapr"))),
                  ## .viewport>* { transform-origin: 0 0; max-width:", view_wh[1], "vw; }
                  tags$style(paste0("#", id, "-plot", 1:9, " { z-index:-", 9:1, "; }", collapse = " ")), ## plot1 lowest, plot9 top-most
                  tags$style(paste0("#", id, " { width:", view_wh[1], "vw; height:", view_wh[2], "vh; }")),
                  singleton(tags$script("$(document).on('shiny:sessioninitialized', function() { Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) }); });")),
                  tags$script(HTML(paste0("var ", id, "_w_scaling=0; var ", id, "_h_scaling=0; var ", id, "_select_mode='pan';",
                                          "function ", id, "_m2px(mxy) { return [Math.round(mxy[0] * ", id, "_w_scaling), Math.round(mxy[1] * ", id, "_h_scaling)] }; ",
                                          "function ", id, "_px2m(pxy) { return [pxy[0] / ", id, "_w_scaling, pxy[1] / ", id, "_h_scaling] };"))),
                  tags$script(HTML(paste0("$(document).on('shiny:sessioninitialized', function() {
                                             Pannable(document.querySelector('#", id, "'));
                                             Shiny.setInputValue('", id, "-window_height', window.innerHeight); Shiny.setInputValue('", id, "-window_width', window.innerWidth);
                                             Shiny.setInputValue('", id, "-view_wh', '", paste0(view_wh, collapse = ","), "');
                                             $('#", id, "-zoom_in').on('pointerdown', function(ev) { ev.preventDefault(); Shiny.setInputValue('", id, "-do_zoom', 1, { priority: 'event' }); });
                                             $('#", id, "-zoom_out').on('pointerdown', function(ev) { ev.preventDefault(); Shiny.setInputValue('", id, "-do_zoom', -1, { priority: 'event' }); });
                                             var ", id, "_br_rsztmr;
                                             $(window).resize(function() {
                                               clearTimeout(", id, "_br_rsztmr);
                                               ", id, "_br_rsztmr = setTimeout(", id, "_br_doneResizing, 500);
                                             });
                                             function ", id, "_br_doneResizing() {
                                               Shiny.setInputValue('", id, "-window_height', window.innerHeight); Shiny.setInputValue('", id, "-window_width', window.innerWidth);
                                             }});"
                                          )))
                  ),
        tags$div(style = "position:relative; margin-top:1px;",
                 tags$div(id = NS(id, "plot_controls"), class = "vl-plot-controls",
                          tags$button(id = NS(id, "zoom_in"), class = "btn btn-default", icon("magnifying-glass-plus", id = NS(id, "zoom-in-icon")), title = "Zoom in"),
                          tags$button(id = NS(id, "zoom_out"), class = "btn btn-default", icon("magnifying-glass-minus", id = NS(id, "zoom-out-icon")), title = "Zoom out"),
                          actionButton(NS(id, "pan_button"), class = "btn btn-default", label = icon("hand", id = NS(id, "pan-icon")), title = "Pan map"),
                          actionButton(NS(id, "select_button"), class = "btn btn-default", label = icon("object-group", id = NS(id, "select-icon"), class = "icon-disabled"), title = "Select region")
                          ),
                 tags$div(id = id, class = "viewport", `data-wh` = paste0(view_wh, collapse = ","),
                          tags$canvas(id = NS(id, "canvas"), class = "viewport-canvas"),
                          do.call(tags$div, c(list(id = NS(id, "pannable"), class = "viewport-pannable"),
                                              lapply(9:1, function(z) tags$canvas(id = NS(id, paste0("plot", z)), class = "viewport-image"))))
                          )
                 )
    )
}

#' @export
#' @rdname vl_map_module
vl_map_ui_postamble <- function() {
    htmltools::includeScript(system.file("extdata/js/vapourlayer.js", package = "cloudymapr"))
}

#' @export
#' @rdname vl_map_module
vl_map_server <- function(id, image_wh = 3200, initial_view = list(tiles_per_side = 1L, extent = c(-1, 1, -1, 1) * 2e7, res = 32e3), layerdef, target_crs = "EPSG:3031", cache = TRUE) { ##Z initial_zoom = 1, zoom_range = c(0, 6),
    plotres <- 96 ## dpi, only used by png graphics device
    .warp_opts <- c("-wm", "999")
    ## .resampling_method <- "near"
    .resampling_method <- "bilinear"

    ## https://gdalcubes.github.io/source/concepts/config.html#recommended-settings-for-cloud-access
    vapour::vapour_set_config("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
    vapour::vapour_set_config("VSI_CACHE", "TRUE")
    vapour::vapour_set_config("GDAL_CACHEMAX","30%")
    vapour::vapour_set_config("VSI_CACHE_SIZE","10000000")
    vapour::vapour_set_config("GDAL_HTTP_MULTIPLEX","YES")
    vapour::vapour_set_config("GDAL_INGESTED_BYTES_AT_OPEN","32000")
    vapour::vapour_set_config("GDAL_HTTP_VERSION","2")
    vapour::vapour_set_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
    vapour::vapour_set_config("GDAL_NUM_THREADS", "ALL_CPUS")

    moduleServer(id, function(input, output, session) {
        tmpd <- tempfile()
        dir.create(tmpd)
        cat("plots temporary directory is:", tmpd, "\n")
        onSessionEnded(function() unlink(tmpd, recursive = TRUE))
        addResourcePath("plots", tmpd)

        if (is.logical(cache)) {
            cache <- if (isTRUE(cache)) cachem::cache_mem() else NULL
        } else if (!inherits(cache, "cachem")) {
            warning("`cache` should be a cachem object (e.g. cachem::cache_mem()), replacing with default memory-based cache")
            cache <- cachem::cache_mem()
        }

        use_fastpng <- TRUE ## otherwise use png()
        clear_on_zoom <- TRUE ## clear plots on zoom? Or leave them visible until refreshed (not quite seamless yet)

        ## generate our image_def from the user-supplied initial_view
        ## initial_view is list(tiles_per_side, extent(xmin, xmax, ymin, ymax), res)

        ## initial native image size
        ## width: diff(initial_view$extent[1:2]) / initial_view$res
        ## height: diff(initial_view$extent[3:4]) / initial_view$res

        temp_xygrid <- expand.grid(x = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side,
                              y = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side)
        tempxy <- expand.grid(x = seq(initial_view$extent[1], initial_view$extent[2], length.out = initial_view$tiles_per_side + 1)[-1] - diff(initial_view$extent[1:2]) / initial_view$tiles_per_side / 2,
                              y = seq(initial_view$extent[3], initial_view$extent[4], length.out = initial_view$tiles_per_side + 1)[-1] - diff(initial_view$extent[3:4]) / initial_view$tiles_per_side / 2)
        image_def <- reactiveVal(list(tiles_per_side = initial_view$tiles_per_side,
                                      n_tiles = initial_view$tiles_per_side ^ 2,
                                      xy_grid = temp_xygrid, ## xy centres of tiles in normalized [-1 1 -1 1] coords
                                      xy = as.data.frame(tempxy), ## xy centres of tiles
                                      w = diff(initial_view$extent[1:2]) / initial_view$tiles_per_side,
                                      h = diff(initial_view$extent[3:4]) / initial_view$tiles_per_side, ## tile width and height in map coords
                                      res = initial_view$res))
        tile_wh <- round(image_wh / initial_view$tiles_per_side) ## in pixels

        viewport_ctr <- reactiveVal(c(0, 0)) ## viewport centre in map units. The viewport is a fixed fraction of the browser window size, so its extent (in pixels and in map units) changes depending on the browser window size

        view_wh <- reactiveVal(NULL)
        get_viewport_size <- function() {
            ## size in pixels
            ##cat("window width: ", input$window_width, "\n")
            ##cat("window height: ", input$window_height, "\n")
            c(if (is.null(input$window_width) || is.null(view_wh()) || isTRUE(input$window_width <= 0)) 800 else round(input$window_width * view_wh()[1] / 100),
              if (is.null(input$window_height) || is.null(view_wh()) || isTRUE(input$window_height <= 0)) 800 else round(input$window_height * view_wh()[2] / 100))
        }

        calc_img_ext <- function(idef) c(min(idef$xy$x - idef$w / 2), max(idef$xy$x + idef$w / 2), min(idef$xy$y - idef$h / 2), max(idef$xy$y + idef$h / 2))
        xywh_to_ext <- function(x, y, w, h) unname(c(x + c(-1, 1) * w / 2, y + c(-1, 1) * h / 2))

## function releaseCanvas(canvas) {
##     canvas.width = 1;
##     canvas.height = 1;
##     const ctx = canvas.getContext('2d');
##     ctx && ctx.clearRect(0, 0, 1, 1);
## }

## iOS max canvas size is 4096 x 4096, other browsers like 10k x 10k

        ##get_native_tile_wh <- function() {
        ##    if (!use_fastpng) {
        ##        tile_wh
        ##    } else {
        ##        ## fastpng renders the images at the size of the data matrix
        ##        wh <- if (!is.null(tiles_data())) tryCatch(attr(tiles_data()$data[[1]], "dimension"), error = function(e) NULL) else NULL
        ##        if (is.null(wh)) c(NA_integer_, NA_integer_) else wh
        ##    }
        ##}
        pixels_to_mu <- function(px) {
            img <- isolate(image_def())
            tryCatch({
                c(px[1] / tile_wh * img$w, px[2] / tile_wh * img$h)
            }, error = function(e) {
                warning("pixels_to_mu failed: ", conditionMessage(e))
                c(NA_real_, NA_real_)
            })
        }
        mu_to_pixels <- function(mu) {
            img <- isolate(image_def())
            tryCatch({
                round(c(mu[1] / img$w * tile_wh, mu[2] / img$h * tile_wh)) ## integer pixels only
            }, error = function(e) {
                warning("mu_to_pixels failed: ", conditionMessage(e))
                c(NA_integer_, NA_integer_)
            })
        }

        ##Z zoom <- reactiveVal(initial_zoom) ## initial
        check_extent_trigger <- reactiveVal(0L)
        tiles_data <- reactiveVal(rep(list(list(img = NULL)), 9))

        set_pan <- function(pxy) {
            if (is.null(isolate(view_wh()))) return(NULL)
            if (missing(pxy)) pxy <- isolate(calc_img_offset(image_def()))
            cat("set_pan: ", pxy, "\n")
            evaljs(paste0("$('#", id, "-pannable').css({'left':'", pxy[1], "px', 'top':'", pxy[2], "px'});", collapse = ""))
        }

        send_plot <- function(plot_contents, plotnum, x = 0, y = 0, w = image_wh, h = image_wh, clear = TRUE, as = "file") {
            cat("plot", plotnum, "updated, sending to js\n")
            pxy <- isolate(calc_img_offset(image_def()))
            plotid <- paste0(id, "-plot", plotnum)
            js <- paste0("$('#", id, "-pannable').css({'left':'", pxy[1], "px', 'top':'", pxy[2], "px'}); $('#", plotid, "')")
            ## chain additional operations on that element to the end of that js
            if (as == "svg") {
                ## plot contents is an svg string
                ##evaljs(paste0(js, ".attr('src', 'data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)), "');"))
                js <- paste0("var image_", id, "_", plotnum, " = new Image(); image_", id, "_", plotnum, ".onload = function() { this_ctx = document.getElementById('", plotid, "').getContext('2d');",
                  "this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";",
                  if (clear) paste0("this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, ");"),
                  "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", x, ", ", y, ", ", w, ", ", h, "); }; image_", id, "_", plotnum, ".src = 'data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)), "';")
                evaljs(js)
            } else {
                ## plot_contents is a file or raw vector
                if (is.raw(plot_contents)) {
                    plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                } else {
                    plot_contents <- paste0("plots/", basename(plot_contents))
                }
                js <- paste0("var image_", id, "_", plotnum, " = new Image(); image_", id, "_", plotnum, ".onload = function() { this_ctx = document.getElementById('", plotid, "').getContext('2d');",
                             "this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";",
                             if (clear) paste0("this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, ");"),
                             "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", x, ", ", y, ", ", w, ", ", h, "); }; image_", id, "_", plotnum, ".src = '", plot_contents, "';")
                evaljs(js)
            }
            set_pan()
        }
        clear_plot <- function(plotnum) {
            ## can be multiple plotnums
            cat("clearing plot", plotnum, "\n")
            evaljs("var this_ctx;", paste0("this_ctx = document.getElementById('", id, "-plot", plotnum, "'); if (this_ctx) { this_ctx.getContext('2d').clearRect(0, 0, 3200, 3200); }", collapse = ""))
        }

        extend_tiles <- function(x = 0, y = 0, z) {
            if (clear_on_zoom) clear_tiles_data() ## there is some lagging/misplotting going on here, so use clear_tiles_data as a temporary workaround
            if (missing(z)) z <- which_are_raster_layers()
            i <- image_def()
            iext <- calc_img_ext(i)
            if (x < 0) {
                i$xy$x <- i$xy$x - diff(iext[1:2]) / 2
            } else if (x > 0) {
                i$xy$x <- i$xy$x + diff(iext[1:2]) / 2
            }
            if (y < 0) {
                i$xy$y <- i$xy$y - diff(iext[3:4]) / 2
            } else if (y > 0) {
                i$xy$y <- i$xy$y + diff(iext[3:4]) / 2
            }
            image_def(i) ## update the image reactive
            for (zz in z) {
                cat("updating tiles data for layer:", zz, "\n")
                update_tiles_data(i, zz) ## request the corresponding data
            }
        }

        clear_tiles_data <- function(z = 1:9) {
            td <- isolate(tiles_data())
            for (i in z) td[[i]] <- list(img = NULL)
            tiles_data(td)
            clear_plot(z) ## TODO only clear raster tile layers?
        }

        is_raster_layer <- function(i) {
            !is.null(layerdef()[[i]]) && "dsn" %in% names(layerdef()[[i]])
        }
        which_are_raster_layers <- function() {
            isolate(which(vapply(seq_along(layerdef()), is_raster_layer, FUN.VALUE = TRUE)))
        }

        fetch_a_tile <- function(ext, dsn, res, type, target_crs, warp_opts, ...) {
            tryCatch({
                if (type == "raster_data") {
                    dt <- vapour::gdal_raster_data(dsn, target_res = res, target_crs = target_crs, target_ext = ext, resample = .resampling_method, options = warp_opts)
                } else if (type == "raster_image_rgb") {
                    dt <- vapour::gdal_raster_data(dsn, bands = 1:3, target_res = res, target_crs = target_crs, target_ext = ext, band_output_type = "Byte", resample = .resampling_method, options = warp_opts)
                } else {
                    ## raster_image_grey
                    dt <- vapour::gdal_raster_data(dsn, bands = 1, target_res = res, target_crs = target_crs, target_ext = ext, band_output_type = "Byte", resample = .resampling_method, options = warp_opts)
                }
                list(data = dt, type = type, ...)
            }, error = function(e) list(data = NULL, type = type, err = conditionMessage(e), ...))
        }

        handle_tile_data <- function(result) {
            td <- isolate(tiles_data())
            td2 <- td[[result$z]]
            if (result$id %in% td2$img$ids) {
                td2$img$data[[which(result$id == td2$img$ids)]] <- result$data
                td2$img$data_hash <- digest::digest(td2$img$data)
                td[[result$z]] <- td2
                tiles_data(td)
                raster_plot_trigger(list(isolate(raster_plot_trigger())[[1]] + 1L, result$z))
            }
        }

        mirai_queue <- list()
        mirai_fetch_tile <- function(t, z, ids, i) {
            this_ext <- xywh_to_ext(x = t$xy$x[i], y = t$xy$y[i], w = t$w, h = t$h)
            cat("fetching data: ext ", this_ext, ", res ", t$res, "\n")
            ld <- layerdef()[[z]]
            rgs <- list(ext = this_ext, z = z, dsn = ld$dsn, res = t$res, type = ld$type, target_crs = target_crs, warp_opts = .warp_opts)
            key <- rlang::hash(rgs)
            if (!is.null(cache) && cache$exists(key)) {
                result <- cache$get(key)
                result$i <- i
                result$id <- ids[i]
                cat("got cached data for layer", result$z, "tile", result$i, "(id", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)), "\n")
                handle_tile_data(result)
                return(invisible(NULL))
            }
            rgs <- c(rgs, list(i = i, id = ids[i], key = key))
            mid <- mirai::mirai(do.call(fetch_a_tile, rgs), rgs = rgs, fetch_a_tile = fetch_a_tile)
            if (!is.null(mid)) mirai_queue[[length(mirai_queue) + 1]] <<- mid
        }

        ## poll the mirai results. Can this be replaced with a shiny::ExtendedTask approach?
        observe({
            done <- c()
            for (ji in seq_along(mirai_queue)) {
                job <- mirai_queue[[ji]]
                if (!inherits(job, "mirai")) {
                    cat("job on queue is not a mirai job:"); cat(utils::str(job))
                    done <- c(done, ji)
                } else {
                    if (!mirai::unresolved(job)) {
                        result <- job$data
                        done <- c(done, ji)
                        cat("got async data for layer", result$z, "tile", result$i, "(id", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)), "\n")
                        print(names(result))
                        if (is.null(result$err)) {
                            if (!is.null(cache)) cache$set(result$key, result[setdiff(names(result), c("i", "id", "key"))]) ## cache it
                            handle_tile_data(result)
                        } else {
                            cat("async data failed: ", conditionMessage(result$err), "\n")
                        }
                    }
                }
            }
            if (length(done) > 0) mirai_queue <<- mirai_queue[-done]
            shiny::invalidateLater(100)
        })

        raster_plot_trigger <- reactiveVal(list(0L, NULL))
        observeEvent(raster_plot_trigger(), {
            if (!is.null(raster_plot_trigger()[[2]])) {
                cat("--> raster_plot_trigger changed for plot:", raster_plot_trigger()[[2]], "\n")
                do_raster_plot(raster_plot_trigger()[[2]])
            }
        })

        have_zoomed <- FALSE
        update_tiles_data <- function(t, z = 1) {
cat("--> in update_tiles_data()\n")            
##**            if (have_zoomed) return(NULL)
            ## cat("updating tiles data for z =", utils::capture.output(utils::str(z)), "\n")
            ## t here is image_def()
            xy_hash <- digest::digest(list(t, layerdef()[[z]]))
            saved_hash <- isolate(tiles_data())[[z]]$xy_hash
            cat("xy_hash is:", xy_hash, ", saved hash is:", saved_hash, "\n")
            if (identical(xy_hash, saved_hash)) {
                cat("not updating tiles_data", z, "- xy_hash is unchanged\n")
            } else {
                cat("updating tiles_data", z, "\n")
                ## generate new IDs, one per tile
                ids <- uuid::UUIDgenerate(n = nrow(t$xy))
                td <- isolate(tiles_data())
                td[[z]]$img <- list(ids = ids, data = rep(list(NULL), nrow(t$xy)), xy_hash = xy_hash, data_hash = "") ##IMSRC src = rep(NA_character_, nrow(t$xy)),
                tiles_data(td)
                ld <- layerdef()[[z]]
                for (i in seq_len(nrow(t$xy))) mirai_fetch_tile(t, z, ids, i)
            }
        }

        observeEvent(input$pan_mxy, {
            if (!is.null(input$pan_mxy)) {
                ##cat("input$pan_mxy is:", utils::capture.output(utils::str(input$pan_mxy)), "\n")
                ## update the viewport centre point
                viewport_ctr(viewport_ctr() - input$pan_mxy * c(1, -1))
                ##cat("viewport_ctr: ", viewport_ctr(), "\n")
                check_extent_trigger(check_extent_trigger() + 1L)
            }
        })

        ## do the initial data load
        ##init_loaded <- FALSE
        observe({
            req(layerdef(), view_wh())
            for (z in which_are_raster_layers()) update_tiles_data(image_def(), z)
        })

        ## given the viewport width and height in pixels, and the tiles definition, work out the offset in pixels required for the image
        calc_img_offset <- function(img) {
            vps_mu <- pixels_to_mu(get_viewport_size()) ## viewport size in map units
            ## cat("calc_img_offset: vps_mu:", vps_mu, "\n")
            ## cat("calc_img_offset: viewport_ctr():", viewport_ctr(), "\n")
            vp_lt <- c(viewport_ctr()[1] - vps_mu[1] / 2, viewport_ctr()[2] + vps_mu[2] / 2)
            ## cat("calc_img_offset: vp_lt:", vp_lt, "\n")
            iext <- calc_img_ext(img)
            ## cat("calc_img_offset: iext:", iext, "\n")
            ## cat("calc_img_offset: image offset in mu:", c(-1, 1) * (vp_lt - iext[c(1, 4)]), "\n")
            io <- mu_to_pixels(c(-1, 1) * (vp_lt - iext[c(1, 4)]))
            ## cat("calc_img_offset: im_offset:", io, "\n")
            io
        }

        ## initial setup, calculate the initial offset to apply to the images
        init0 <- FALSE
        observe({
            if (!init0) {
                init0 <<- TRUE
                ## INIT evaljs("$('#", id, "').width('", view_wh[1], "vw').height('", view_wh[2], "vh');")
                evaljs("$('#", id, "-pannable').width('", image_wh, "px').height('", image_wh, "px');")
                ## TODO store array of contexts - but discard the pair plots idea first if possible
##?!?                evaljs("var this_ctx;")
##?!?                for (plotnum in 1:9) evaljs(paste0("this_ctx = document.getElementById('", id, "-plot", plotnum, "').getContext('2d'); this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";", collapse = ""))
            }
        })
        observe({
            ## get the viewport width and height via js (this means that we only need to specify view_wh in the ui call, not the server as well)
            cat("checking view_wh\n")
            cat("input$view_wh is:", utils::capture.output(utils::str(input$view_wh)), "\n")
            if (is.null(input$view_wh)) {
                evaljs("Shiny.setInputValue('",id, "-view_wh', $('#", id, "').attr('data-wh'));")
                shiny::invalidateLater(250)
            } else {
                view_wh(as.numeric(strsplit(input$view_wh, ",")[[1]]))
            }
        })
#        observe({
#        })
        init_offset <- FALSE
        observe({
            ##cat("INIT entered with", utils::capture.output(utils::str(input$window_width)), utils::capture.output(utils::str(input$window_height)), utils::capture.output(utils::str(image_def())), "\n")
            req(input$window_width, input$window_height, view_wh(), image_def())
            if (!init_offset) {
                init_offset <<- TRUE
                ## set the UI sizing
                ## TODO make this less fragile, e.g. if the window changes size during app startup
                ## TODO also  by id, not class
                ## initial image offset required
                set_pan()
                cat("INIT done\n")
            }
        }, priority = 99)

        observe({
            ## keep the client-side scaling factors up to date
            map2px <- c(tile_wh / image_def()$w, tile_wh / image_def()$h)
            evaljs(id, "_w_scaling=", map2px[1], "; ", id, "_h_scaling=", map2px[2], ";")
        })

        observeEvent(input$pan_button, {
            js_add_class(paste0(id, "-select-icon"), "icon-disabled")
            js_remove_class(paste0(id, "-pan-icon"), "icon-disabled")
            evaljs(id, "_select_mode = 'pan';")
        })
        observeEvent(input$select_button, {
            js_remove_class(paste0(id, "-select-icon"), "icon-disabled")
            js_add_class(paste0(id, "-pan-icon"), "icon-disabled")
            evaljs(id, "_select_mode = 'select';")
        })

        last_render_hash <- rep("", 9)
##        observe({
        do_raster_plot <- function(ii = 1:9) {
cat("--> in raster layer plotter for", ii, "\n")            
##**            if (have_zoomed) return(NULL)
            ## plot raster-based layers
            for (i in ii) {##seq_along(layerdef())) {
                if (is_raster_layer(i)) {
                    ## this is a dsn-based raster layer
                    td0 <- isolate(tiles_data())
                    td <- td0[[i]]$img
                    if (!is.null(td)) {
                        if (identical(last_render_hash[i], td$data_hash)) {
                            cat("layer i data hash unchanged, not re-rendering\n")
                        } else {
                            last_render_hash[i] <- td$data_hash
                            z <- layerdef()[[i]]$z
                            pltf <- if (use_fastpng) NULL else tempfile(tmpdir = tmpd, fileext = ".png")
                            ##IMSRC td$src needs to be per-tile, not per-full-image
                            ##IMSRC update tiles_data with src
                            iext <- calc_img_ext(image_def())
                            message("rendering raster plot:", pltf)
                            ##                tictoc::tic()
                            if (use_fastpng) {
                                tdim <- attr(td$data[[1]], "dimension")
                                if (!is.null(tdim)) {
                                    ord <- order(image_def()$xy$x, -image_def()$xy$y)
                                    n <- image_def()$tiles_per_side ## image is composed of a square arrangement of tiles (which may be only one tile)
                                    if (layerdef()[[i]]$type == "raster_data") {
                                        ## assemble tiles into single matrix
                                        m1 <- function(z) matrix(if (is.null(z)) NA_real_ else z, nrow = tdim[2], ncol = tdim[1], byrow = TRUE) ## matrix constructor
                                        ## in what order to we need to assemble the tiles?
                                        big <- do.call(cbind, lapply(seq_len(n), function(ci) {
                                            do.call(rbind, lapply((ci - 1) * n + seq_len(n), function(ri) {
                                                m1(td$data[[ord[ri]]][[1]])
                                            }))
                                        }))
                                        zl <- if (is.null(layerdef()[[i]]$zlims[[1]])) range(big, na.rm = TRUE) else layerdef()[[i]]$zlims[[1]]
                                        cmap <- layerdef()[[i]]$cmap[[1]]
                                        big <- pmax(pmin(round((big - zl[1]) / abs(diff(zl)) * (length(cmap) - 1L)) + 1L, length(cmap)), 1L) - 1L ## scale by given zlim and then mapped to colour range, using zero-based indexing
                                        pltf <- fastpng::write_png(big, palette = cmap, file = pltf, compression_level = 0L)
                                    } else {
                                        ## image, rgb or greyscale
                                        ## if it's greyscale we get one band back, but we assemble here into 3 bands (rgb)
                                        ## inefficient but I think fastpng requires it? (can handle 2d matrix but will scale the limits to 0-1)
                                        ## if we used gdal_raster_image fastpng::write_png(255 - aperm(array(col2rgb(td$data[[1]][[1]]), c(3, attr(td$data[[1]], "dimension"))), c(3, 2, 1)), filename = pltf, convert_to_row_major = TRUE)
                                        a1 <- function(z) array(if (is.null(z)) NA_real_ else 256 - as.integer(unlist(z)), dim = c(tdim, 3)) ## array constructor
                                        ## in what order to we need to assemble the array chunks?
                                        big <- do.call(abind, c(lapply(seq_len(n), function(ci) {
                                            do.call(abind, c(lapply((ci - 1) * n + seq_len(n), function(ri) {
                                                aperm(a1(td$data[[ord[ri]]][1:3]), c(2, 1, 3))
                                            }), list(along = 1)))
                                        }), list(along = 2)))
                                        pltf <- fastpng::write_png(big, file = pltf, compression_level = 0L)
                                    }
                                    ## fastpng writes the image to be sized at exactly the matrix dimensions (i.e. an 800 x 300 matrix will be 800 x 300 pixels)
                                    ## but we are displaying the image tiles in the browser at tile_wh in width and height
                                    ## could resize: this is too slow though, just use CSS to make the browser scale it visually
                                    ##magick::image_write(magick::image_scale(magick::image_read(pltf), paste0(tile_wh * image_def()$tiles_per_side, "x", tile_wh * image_def()$tiles_per_side)), pltf)
                                    message("done.")
                                } else {
                                    message("tdim is NULL")
                                }
                            } else {
                                ## TODO use unigd?
                                png(pltf, height = image_wh, width = image_wh, res = plotres, bg = NA)
                                opar <- par(no.readonly = TRUE)
                                par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") ## zero margins
                                on.exit(par(opar))
                                plot(0, 0, type = "n", axes = FALSE, xlim = iext[1:2], ylim = iext[3:4])
                                for (tl in seq_len(min(4, length(td$data)))) {
                                    ext <- xywh_to_ext(x = image_def()$xy$x[tl], y = image_def()$xy$y[tl], w = image_def()$w, h = image_def()$h)
                                    if (!is.null(td$data[[tl]])) {
                                        ximage::ximage(td$data[[tl]], extent = ext, asp = 1, zlim = layerdef()[[i]]$zlims[[1]], col = layerdef()[[i]]$cmap[[1]], add = TRUE)
                                    } else {
                                        text(x = runif(50) * (ext[2] - ext[1]) + ext[1], y = runif(50) * (ext[4] - ext[3]) + ext[3], labels = rep("?", 50))
                                    }
                                }
                                dev.off()
                            }
                            ##                tictoc::toc()
                            send_plot(pltf, z)
                        }
                    }
                }
            }
        }##)

##!! need offset per layer, because in transitional phase of zoom, the raster images will be at different scale/extent to R plots

        observe({
cat("--> in vector layer plotter\n")            
##**            if (have_zoomed) return(NULL)
            ## deal with non-raster layers for which the user has provided a plot function
            iext <- calc_img_ext(image_def())
            use_ugd <- TRUE
            for (i in seq_along(layerdef())) {
                if (!is.null(layerdef()[[i]]) && "fun" %in% names(layerdef()[[i]])) {
                    z <- layerdef()[[i]]$z
                    cat("rendering plot", i, "layer", z, " as", if (use_ugd) "svg\n" else "png\n")
                    ##tictoc::tic()
                    pltf <- tempfile(tmpdir = tmpd, fileext = ".png")
                    ##IMSRC need to keep track of pltf for each non-raster source as well
                    if (!use_ugd) {
                        png(pltf, height = image_wh, width = image_wh, res = plotres, bg = NA)
                    } else {
                        unigd::ugd(width = image_wh, height = image_wh, bg = "transparent")
                    }
                    opar <- par(no.readonly = TRUE)
                    par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") ## zero margins
                    ok <- layerdef()[[i]]$fun(xlim = iext[1:2], ylim = iext[3:4])
                    if (ok && use_ugd) {
                        pltf <- unigd::ugd_render(as = "svg")
                        pltf <- sub("<rect width=\"100%\" height=\"100%\" style=\"stroke: none;fill: #FFFFFF;\"/>", "", pltf, fixed = TRUE)
                    }
                    par(opar)
                    dev.off()
                    ##tictoc::toc()
                    if (use_ugd) {
                        if (ok) send_plot(pltf, z, as = "svg") else clear_plot(z)
                    } else {
                        if (ok) send_plot(pltf, z) else clear_plot(z)
                    }
                }
            }
        }, priority = 9)

        mapclick <- reactiveVal(NULL)
        observeEvent(input$mapclick, {
            req(input$mapclick)
            pxy <- input$mapclick[1:2] ## click coords in pixels relative to viewport; (0, 0) is bottom-left
            vps <- get_viewport_size()
            ## cat("viewport size:", vps, "\n")
            ## cat("click xy: ", pxy, "px\n")
            pxy <- c(pxy[1] - vps[1] / 2, pxy[2] - vps[2] / 2) ## centred, so now (0, 0) is centre of viewport
            ## cat("click xy: ", pxy, "px relative to viewport centre\n")
            ## in map units
            mxy <- pixels_to_mu(pxy)
            ## cat("click xy: ", mxy, "map units relative to viewport centre\n")
            mxy <- mxy + viewport_ctr() ## click location (whole map) in map units
            cat("click xy: ", round(mxy, 2), "map units\n")
            mapclick(mxy)
        })

        observeEvent(input$dragselect, {
            req(input$dragselect)
            if (length(input$dragselect) == 4) {
                vps <- get_viewport_size()
                mxy <- c(input$dragselect[1:2] - vps[1] / 2, input$dragselect[3:4] - vps[2] / 2) ## centred, so now (0, 0) is centre of viewport
                ## convert to map units
                mxy <- c(pixels_to_mu(mxy[c(1, 3)]), pixels_to_mu(mxy[c(2, 4)]))[c(1, 3, 2, 4)] ## TODO fix this nonsense
                mxy <- mxy + viewport_ctr()[c(1, 1, 2, 2)]
                ##cat("select mu:", mxy, "\n") ## [xmin xmax ymin ymax] in pixels relative to viewport; (0, 0) is bottom-left
                vps_mu <- pixels_to_mu(get_viewport_size()) ## viewport size in map units
                zoomf <- min(vps_mu[1] / abs(diff(mxy[c(1, 2)])), vps_mu[2] / abs(diff(mxy[c(3, 4)])))
                zoomc <- c((mxy[1] + mxy[2]) / 2, (mxy[3] + mxy[4]) / 2)
                ##cat("zoom factor:", zoomf, ", with centre:", zoomc, "\n")
                do_zoom(zoomc, zoom_by = zoomf)
            }
        })

        observeEvent(input$do_zoom, {
            req(input$do_zoom)
            cat("zoom:", input$do_zoom, "\n")
            do_zoom(in_out = input$do_zoom)
        })

        do_zoom <- function(mxy, in_out, zoom_by) {
            if (missing(mxy)) mxy <- isolate(viewport_ctr())
            cat("do_zoom, ctr: ", mxy, "\n")
            ## mxy is map units of centre point of zoom
            ## in_out is 1 for in, -1 for out, or specify zoom_by (zoom factor)
            if (missing(in_out)) in_out <- (zoom_by > 1) * 2L - 1L
            if (missing(zoom_by)) zoom_by <- NA
            ## TODO zoom() and zoom_range don't make sense if we are allowing zoom by arbitrary rectangle
            ## but we do need a zoom-out limit, probably
            if (TRUE) {##Z ((in_out > 0 && zoom() < zoom_range[2]) || (in_out < 0 && zoom() > zoom_range[1])) {
                if (in_out > 0) {
                    if (is.na(zoom_by)) zoom_by <- 2
                    ##Z zoom(zoom() + 1L)
                } else {
                    if (is.na(zoom_by)) zoom_by <- 1/2
                    ##Z zoom(zoom() - 1L)
                }
                ## some things before zooming
                vps_mu <- pixels_to_mu(get_viewport_size()) ## current viewport size in map units
                ## update the tile centres, width, height
                t0 <- t <- image_def()
                t$res <- t$res / zoom_by
                t$w <- t$w / zoom_by
                t$h <- t$h / zoom_by
                t$xy$x <- mxy[1] + t$w * image_def()$xy_grid[, 1]
                t$xy$y <- mxy[2] + t$h * image_def()$xy_grid[, 2]
                image_def(t)
                viewport_ctr(mxy)
                if (clear_on_zoom) {
                    clear_tiles_data()
                } else {
                    ## TODO
                    ## initially just rescale the images so we are showing the lower-res (when zooming in) detail while the higher res loads
                    ##IMSRC need current source for each layer so can re-send at different offset and wh
                    ## all layers
                    io <- {
                        vps_mu <- vps_mu / zoom_by ## viewport size in map units after zooming
                        vp_lt <- c(mxy[1] - vps_mu[1] / 2, mxy[2] + vps_mu[2] / 2) ## top-left of viewport after zooming
                        iext <- calc_img_ext(t0)
                        io_mu <- c(-1, 1) * (vp_lt - iext[c(1, 4)]) ## image offset in map units
                        c(io_mu[1] / t0$w * tile_wh * zoom_by, io_mu[2] / t0$h * tile_wh * zoom_by)
                    }
                    ## TODO these in one step per layer
                    set_pan(io) ## set pan at the temporarily rescaled size
                    cat("--> zoom-resizing the visible image of all layers\n")
                    for (z in 1:9) evaljs("$('#", id, "-plot", z, "').width('", image_wh * zoom_by, "px').height('", image_wh * zoom_by, "px');")
                }
                have_zoomed <<- TRUE
                cat("out zoom\n")
            }
            ##Z if (zoom() >= zoom_range[2]) js_add_class(paste0(id, "-zoom-in-icon"), "icon-disabled") else js_remove_class(paste0(id, "-zoom-in-icon"), "icon-disabled")
            ##Z if (zoom() <= zoom_range[1]) js_add_class(paste0(id, "-zoom-out-icon"), "icon-disabled") else js_remove_class(paste0(id, "-zoom-out-icon"), "icon-disabled")
        }

        observe({
            ##        ## detect when the image goes beyond the available data extent
            blah <- check_extent_trigger()
            ##cat("viewport ctr:", viewport_ctr(), "\n")
            ##cat("viewport size:", get_viewport_size(), "px\n")
            vps_mu <- pixels_to_mu(get_viewport_size()) ## viewport size in map units
            ##        vext <- isolate(xywh_to_ext(x = viewport()$xy[1], y = viewport()$xy[2], w = viewport()$w, h = viewport()$h))
            ##cat("viewport size:", vps_mu, "map units\n")
            vext <- isolate(xywh_to_ext(x = viewport_ctr()[1], y = viewport_ctr()[2], w = vps_mu[1], h = vps_mu[2]))
            ##cat("viewport ext: ", round(vext / 1e6, 2), "\n")
            iext <- calc_img_ext(image_def())
            ##cat("data ext: ", round(iext / 1e6, 2), "\n")
            ##cat("img offset for checking:", isolate(calc_img_offset(image_def())), "\n")
            ## don't do this if we are in the process of reloading data
            if (!any(is.na(vext)) && !any(is.na(iext))) {
####        vext <- c(.vw_w / 2 * c(-1, 1), .vw_h / 2 * c(-1, 1))
####        if (!is.null(input$pan_xy)) vext <- vext + rep(input$pan_xy, each = 2)
                ##
                ##        ## vext is visible extent (pixels) of the current image
                ##        vext <- c(.vw_w / 2 * c(1, 3), .vw_h / 2 * c(1, 3)) - rep(plot_offset(), each = 2)
                ##
                ##        map_er <- isolate(map_extres())
                fetch <- FALSE
                stx <- sty <- 0L
                ##        cat("vext now:", vext, "\n")
                ##        ##cat("pan_xy now:", input$pan_xy, "\n")
                ##        cat("plot_offset now:", plot_offset(), "\n")
                ##        cat("ext now:", map_er$ext, "\n")
                if (vext[1] < iext[1]) {
                    cat("out of bounds left, need to fetch more data\n")
                    fetch <- TRUE
                    stx <- -1L
                } else if (vext[2] > iext[2]) {
                    cat("out of bounds right, need to fetch more data\n")
                    fetch <- TRUE
                    stx <- 1L
                }
                if (vext[4] > iext[4]) {
                    cat("out of bounds up, need to fetch more data\n")
                    fetch <- TRUE
                    sty <- 1L
                } else if (vext[3] < iext[3]) {
                    cat("out of bounds down, need to fetch more data\n")
                    fetch <- TRUE
                    sty <- -1L
                }
                if (fetch) extend_tiles(x = stx, y = sty)
            }
        })

        list(click = mapclick, get_viewport_size = get_viewport_size, viewport_ctr = viewport_ctr)
    })
}
