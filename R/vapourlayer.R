fetch_a_tile <- function(ext, dsn, res, type, target_crs, warp_opts, resampling, ...) {
    tryCatch({
        if (type == "raster_data") {
            dt <- vapour::gdal_raster_data(dsn, target_res = res, target_crs = target_crs, target_ext = ext, resample = resampling, options = warp_opts)
        } else if (type == "raster_image_rgb") {
            dt <- vapour::gdal_raster_nara(dsn, bands = 1:3, target_res = res, target_crs = target_crs, target_ext = ext, band_output_type = "Byte", resample = resampling, options = warp_opts)
        } else {
            ## raster_image_grey
            dt <- vapour::gdal_raster_data(dsn, bands = 1, target_res = res, target_crs = target_crs, target_ext = ext, band_output_type = "Byte", resample = resampling, options = warp_opts)
        }
        list(data = dt, type = type, ...)
    }, error = function(e) list(data = NULL, type = type, err = conditionMessage(e), ...))
}

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
                                             $('#", id, "-zoom_in').on('pointerdown', function(ev) { ev.preventDefault(); Shiny.setInputValue('", id, "-do_zoom', [1, -parseInt($('#", id, "-pannable').css('left')), -parseInt($('#", id, "-pannable').css('top'))], { priority: 'event' }); });
                                             $('#", id, "-zoom_out').on('pointerdown', function(ev) { ev.preventDefault(); Shiny.setInputValue('", id, "-do_zoom', [-1, -parseInt($('#", id, "-pannable').css('left')), -parseInt($('#", id, "-pannable').css('top'))], { priority: 'event' }); });
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
vl_map_server <- function(id, image_wh = 3200, initial_view = list(tiles_per_side = 1L, extent = c(-1, 1, -1, 1) * 2e7, res = 32e3), layerdef, target_crs = "EPSG:3031", cache = TRUE) {
    .plotres <- 96 ## dpi, only used by png graphics device
    .warp_opts <- c("-wm", "999")
    .resampling_method <- "near"
    .debug <- 2L
    ## .resampling_method <- "bilinear" ## TODO allow this to be specified on a source-by-source basis. Be aware that bilinear sampling can cause issues when the source is raster_data and it has special values like a land mask (255) but valid values are (say) 0-100: interpolation near land will give values > 100 and < 255

    ## https://gdalcubes.github.io/source/concepts/config.html#recommended-settings-for-cloud-access
    vapour::vapour_set_config("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
    vapour::vapour_set_config("VSI_CACHE", "TRUE")
    vapour::vapour_set_config("GDAL_CACHEMAX","30%")
    vapour::vapour_set_config("VSI_CACHE_SIZE","10000000")
    vapour::vapour_set_config("GDAL_HTTP_MULTIPLEX","YES")
    vapour::vapour_set_config("GDAL_INGESTED_BYTES_AT_OPEN","128000") ## was 32k
    vapour::vapour_set_config("GDAL_HTTP_VERSION","2")
    vapour::vapour_set_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
    vapour::vapour_set_config("GDAL_NUM_THREADS", "ALL_CPUS")
    if (.debug > 1) vapour::vapour_set_config("CPL_DEBUG", "ON") else vapour::vapour_set_config("CPL_DEBUG", "OFF")

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

        .use_fastpng <- TRUE ## otherwise use png()
        .png_in_memory <- FALSE
        .png_compression_level <- 2L ## 0L is no compression, ~0.1s to write a 2500x2500 image (25MB file); 2L ~1.4s for the same image (9.5MB) or ~0.9s with .use_png_filter = FALSE (11MB file)
        .use_png_filter <- .png_compression_level < 1 ## see fastpng::write_png
        .clear_on_zoom <- FALSE ## clear plots on zoom? Or leave them visible while refreshing?
        .use_ugd <- TRUE ## for vector layers, use unigd:ugd to generate svg
        .clear_canvas_before_drawing <- FALSE

        ## generate our image_def from the user-supplied initial_view
        ## initial_view is list(tiles_per_side, extent(xmin, xmax, ymin, ymax), res)

        ## initial native image size
        ## width: diff(initial_view$extent[1:2]) / initial_view$res
        ## height: diff(initial_view$extent[3:4]) / initial_view$res

        ## tile width and height in map coords
        tile_w <- diff(initial_view$extent[1:2]) / initial_view$tiles_per_side
        tile_h <- diff(initial_view$extent[3:4]) / initial_view$tiles_per_side

        ## we need the view to be defined such that the xy extents are multiples of the number of tiles per side
        ## TODO or even multiples? or powers of 2?
        if (initial_view$tiles_per_side <= 0 || (initial_view$tiles_per_side / floor(initial_view$tiles_per_side) != 1)) stop("initial_view$tiles_per_side must be an even positive integer")
        if (initial_view$tiles_per_side > 1) {
            if (initial_view$tiles_per_side %% 2 != 0) stop("initial_view$tiles_per_side must be 1 or an even positive integer")
            chk <- c(tile_w, tile_h)
            if (any(abs(chk - round(chk)) > 0)) stop("initial_view must define the xy extents to be even multiples of the number of tiles per side")
        }

        ## xy centres of tiles in normalized [-1 1 -1 1] coords
        temp_xygrid <- expand.grid(x = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side,
                                   y = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side)
        ## xy centres of tiles in map coords
        tempxy <- expand.grid(x = seq(initial_view$extent[1], initial_view$extent[2], length.out = initial_view$tiles_per_side + 1)[-1] - diff(initial_view$extent[1:2]) / initial_view$tiles_per_side / 2,
                              y = seq(initial_view$extent[3], initial_view$extent[4], length.out = initial_view$tiles_per_side + 1)[-1] - diff(initial_view$extent[3:4]) / initial_view$tiles_per_side / 2)
        image_def <- reactiveVal(list(tiles_per_side = initial_view$tiles_per_side,
                                      n_tiles = initial_view$tiles_per_side ^ 2,
                                      xy_grid = temp_xygrid, ## xy centres of tiles in normalized [-1 1 -1 1] coords
                                      xy = as.data.frame(tempxy), ## xy centres of tiles in map coords
                                      w = tile_w, h = tile_h, ## tile width and height in map coords
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
        ##    if (!.use_fastpng) {
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
        mu_to_pixels <- function(mu, idef) {
            if (missing(idef)) idef <- isolate(image_def())
            tryCatch({
                round(c(mu[1] / idef$w * tile_wh, mu[2] / idef$h * tile_wh)) ## integer pixels only
            }, error = function(e) {
                warning("mu_to_pixels failed: ", conditionMessage(e))
                c(NA_integer_, NA_integer_)
            })
        }

        check_extent_trigger <- reactiveVal(0L)
        tiles_data <- reactiveVal(rep(list(list(img = NULL)), 9))

        get_pan_js <- function(pxy) {
            if (missing(pxy)) pxy <- isolate(calc_img_offset(image_def()))
            paste0("$('#", id, "-pannable').css({'left':'", pxy[1], "px', 'top':'", pxy[2], "px'});", collapse = "")
        }
        set_pan <- function(pxy) {
            if (is.null(isolate(view_wh()))) return(NULL)
            if (missing(pxy)) pxy <- isolate(calc_img_offset(image_def()))
            cat("set_pan: ", pxy, "\n")
            evaljs(get_pan_js(pxy))
        }

        send_plot <- function(plot_contents, plotnum, x = 0, y = 0, w = image_wh, h = image_wh, clear = .clear_canvas_before_drawing, as = "file") {
            cat("plot", plotnum, "updated, sending to js ")
            plotid <- paste0(id, "-plot", plotnum)
            panjs <- ""
            if (set_pan_on_plot) {
                panjs <- get_pan_js()
                set_pan_on_plot <<- FALSE
            }
            if (as == "svg") {
                cat("as svg\n")
                ## plot contents is an svg string
                js <- paste0("var image_", id, "_", plotnum, " = new Image(); image_", id, "_", plotnum, ".onload = function() { this_ctx = document.getElementById('", plotid, "').getContext('2d');",
                  "this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";",
                  if (clear) paste0("this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, ");"),
                  "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", x, ", ", y, ", ", w, ", ", h, "); ", panjs, "}; image_", id, "_", plotnum, ".src = 'data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)), "';")
                evaljs(js)
#                readline("enter to continue")
            } else {
                ## plot_contents is a file or raw vector
                if (is.raw(plot_contents)) {
                    cat("as b64 png from raw\n")
                    plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                } else {
                    cat("as image from file\n")
                    plot_contents <- paste0("plots/", basename(plot_contents))
                }
                js <- paste0("var image_", id, "_", plotnum, " = new Image(); image_", id, "_", plotnum, ".onload = function() { this_ctx = document.getElementById('", plotid, "').getContext('2d');",
                             "this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";",
                             if (clear) paste0("this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, ");"),
                             "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", x, ", ", y, ", ", w, ", ", h, "); ", panjs, "}; image_", id, "_", plotnum, ".src = '", plot_contents, "';")
                evaljs(js)
            }
        }
        clear_plot <- function(plotnum) {
            ## can be multiple plotnums
            cat("clearing plot", plotnum, "\n")
            evaljs("var this_ctx;", paste0("this_ctx = document.getElementById('", id, "-plot", plotnum, "'); if (this_ctx) { this_ctx.getContext('2d').clearRect(0, 0, 3200, 3200); }", collapse = ""))
        }

        set_pan_on_plot <- FALSE
        extend_tiles <- function(x = 0, y = 0, z) {
            if (.debug > 1) cat("--> in extend_tiles()\n")
            if (.clear_on_zoom) clear_tiles_data() ## there is some lagging/misplotting going on here, so use clear_tiles_data as a temporary workaround TODO check and fix
            if (missing(z)) z <- which_are_raster_layers()
            i0 <- i <- image_def()
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
            ## ask for pan to be reset when the next plot occurs (don't do it directly here, otherwise we're panning before the re-plot)
            set_pan_on_plot <<- TRUE
            ## don't think this is needed, because the "generic" update_tiles_data observer should catch the change to image_def
            ## for (zz in z) {
            ##     cat("updating tiles data for layer:", zz, "\n")
            ##     update_tiles_data(i, zz) ## request the corresponding data
            ## }
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
        which_are_vector_layers <- function() {
            isolate(which(!vapply(seq_along(layerdef()), is_raster_layer, FUN.VALUE = TRUE)))
        }

        handle_tile_data <- function(result) {
            td <- isolate(tiles_data())
            td2 <- td[[result$z]]
            if (result$id %in% td2$img$ids) {
                i <- which(result$id == td2$img$ids)
                td2$img$data[[i]] <- result$data
                td2$img$data_hash <- digest::digest(td2$img$data)
                td[[result$z]] <- td2
                tiles_data(td)
                isolate(draw_tile(z = result$z, td = td2, i = i, image_def = image_def(), layerdef = layerdef()))
            }
        }

        mirai_queue <- list()
        mirai_fetch_tile <- function(t, z, ids, i) {
            this_ext <- xywh_to_ext(x = t$xy$x[i], y = t$xy$y[i], w = t$w, h = t$h)
            cat("fetching data: ext ", this_ext, ", res ", t$res, "\n")
            ld <- layerdef()[[z]]
            rgs <- list(ext = this_ext, z = z, dsn = ld$dsn, res = t$res, type = ld$type, target_crs = target_crs, warp_opts = .warp_opts, resampling = .resampling_method)
            key <- rlang::hash(rgs)
            if (!is.null(cache) && cache$exists(key)) {
                result <- cache$get(key)
                result$i <- i
                result$id <- ids[i]
                cat("got cached data for layer", result$z, "tile", result$i, "(id", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)), "\n")
                handle_tile_data(result)
                return(invisible(NULL))
            }
            ## if a fetch request is already happening with this key, don't re-issue a new fetch request
            if (key %in% sapply(mirai_queue, function(job) job$key)) {
                ## cat("request with key", key, "is already in the queue, not re-queueing\n")
            } else {
                rgs <- c(rgs, list(i = i, id = ids[i], key = key))
                ## cat("fetching:\n"); cat(utils::str(rgs))
                ##if (.debug > 1)
                if (.debug > 1) temp <- proc.time()["elapsed"]
                mid <- mirai::mirai(do.call(cloudymapr:::fetch_a_tile, rgs), rgs = rgs)## don't pass the function here, it's super slow, fetch_a_tile = fetch_a_tile)
                if (.debug > 1) {
                    temp <- proc.time()["elapsed"] - temp
                    message("mirai dispatch time time: ", round(temp, 3), "s")
                }
                if (!is.null(mid)) mirai_queue[[length(mirai_queue) + 1]] <<- list(mid = mid, key = key)
            }
        }

        ## poll the mirai results. Can this be replaced with a shiny::ExtendedTask approach?
        observe({
            done <- c()
            for (ji in seq_along(mirai_queue)) {
                job <- mirai_queue[[ji]]$mid
                if (!inherits(job, "mirai")) {
                    cat("job on queue is not a mirai job:"); cat(utils::str(job))
                    done <- c(done, ji)
                } else {
                    if (!mirai::unresolved(job)) {
                        result <- job$data
                        done <- c(done, ji)
                        cat("got async data type", result$type, "for layer", result$z, "tile", result$i, "(id", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)), "\n")
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

        ## raster_plot_trigger <- reactiveVal(list(0L, NULL))
        ## observeEvent(raster_plot_trigger(), {
        ##     if (!is.null(raster_plot_trigger()[[2]])) {
        ##         cat("--> raster_plot_trigger changed for plot:", raster_plot_trigger()[[2]], "\n")
        ##         do_raster_plot(raster_plot_trigger()[[2]])
        ##     }
        ## })

        update_tiles_data <- function(t, z = 1) {
cat("--> in update_tiles_data()\n")            
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
            req(layerdef(), view_wh(), image_def())
            cat("triggering generic update_tiles_data() because image_def(), layerdef(), or view_wh() has changed\n")
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
                ## set the canvas sizes
                evaljs(paste(sapply(1:9, function(plotnum) paste0("this_ctx = document.getElementById('", id, "-plot", plotnum, "').getContext('2d'); this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";")), collapse = ""))
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
                ## needed? should be fired by the browser js when the sessioninitialized code runs
                ## cat("triggering input$view_wh update\n")
                ## evaljs("Shiny.setInputValue('",id, "-view_wh', $('#", id, "').attr('data-wh'));")
                ## ## shiny::invalidateLater(250)
            } else {
                view_wh(as.numeric(strsplit(input$view_wh, ",")[[1]]))
            }
        })

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
        do_raster_plot <- function(ii = 1:9) {
            ## cat("--> in raster layer plotter for", ii, "\n")
            ## plot raster-based layers
            for (i in ii) {##seq_along(layerdef())) {
                if (is_raster_layer(i)) {
                    ## this is a dsn-based raster layer
                    td0 <- isolate(tiles_data())
                    td <- td0[[i]]$img
                    if (!is.null(td)) {
                        if (!is.na(last_render_hash[i]) && identical(last_render_hash[i], td$data_hash)) {
                            cat("layer i data hash unchanged, not re-rendering\n")
                        } else {
                            last_render_hash[i] <- td$data_hash
                            z <- layerdef()[[i]]$z
                            pltf <- render_to_png(td = td, image_def = image_def(), layerdef = layerdef()[[i]], image_wh = image_wh)
                            send_plot(pltf, z)
                        }
                    }
                }
            }
        }

        draw_tile <- function(z, td, i, image_def, layerdef, clear = TRUE, as = "file") {
            ## z is the layer index (e.g. into tiles_data()
            ## td is that tiles_data() entry
            ## i is the index within the layer (> 1 if we have multiple tiles per side)
            ## send tile to canvas
            plot_contents <- tile_to_png(td = td, i = i, layerdef = layerdef[[z]])
            if (!is.null(plot_contents)) {
                iext_mu <- calc_img_ext(image_def) ## full canvas extent in map units, e.g. [-2 2 -2 2]e7
                ## the normalized x and y extents of this tile in that canvas extent
                xn <- (attr(td$img$data[[i]], "extent")[1:2] - iext_mu[1]) / diff(iext_mu[1:2])
                yn <- 1 - (attr(td$img$data[[i]], "extent")[3:4] - iext_mu[3]) / diff(iext_mu[3:4]) ## 1 - because we draw from top
                ## and thus the region in pixels in the canvas for this tile
                tlp <- c(xn[1], yn[2]) * image_wh
                brp <- c(xn[2], yn[1]) * image_wh

                ## top-left/bottom-right (in pixels) of this tile in the image, remembering that the canvas origin (0, 0) is top-left
                                        #            tlp <- (image_def$xy_grid[i, ] + 1)/2 + 1/image_def$tiles_per_side/2 * c(-1, 1); tlp[2] <- 1 - tlp[2]; tlp <- tlp * image_wh
                                        #            brp <- (image_def$xy_grid[i, ] + 1)/2 + 1/image_def$tiles_per_side/2 * c(1, -1); brp[2] <- 1 - brp[2]; brp <- brp * image_wh

                cat("sending tile to js ")
                plotid <- paste0(id, "-plot", z)
                panjs <- ""
                if (set_pan_on_plot) {
                    panjs <- get_pan_js()
                    set_pan_on_plot <<- FALSE
                }
                if (as == "svg") {
                    cat("as svg\n")
                    ## plot contents is an svg string
                    js <- paste0("var image_", id, "_", z, " = new Image(); image_", id, "_", z, ".onload = function() { this_ctx = document.getElementById('", plotid, "').getContext('2d');",
                                 ##"this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";",
                                 if (clear) paste0("this_ctx.clearRect(", tlp[1], ", ", tlp[2], ", ", brp[1] - tlp[1], ", ", brp[2] - tlp[2], ");"),
                                 "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", tlp[1], ", ", tlp[2], ", ", brp[1] - tlp[1], ", ", brp[2] - tlp[2], "); ", panjs, "}; image_", id, "_", z, ".src = 'data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)), "';")
                    evaljs(js)
                                        #                readline("enter to continue")
                } else {
                    ## plot_contents is a file or raw vector
                    if (is.raw(plot_contents)) {
                        cat("as b64 png from raw\n")
                        plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                    } else {
                        cat("as image from file\n")
                        plot_contents <- paste0("plots/", basename(plot_contents))
                    }
                    js <- paste0("var image_", id, "_", z, " = new Image(); image_", id, "_", z, ".onload = function() { this_ctx = document.getElementById('", plotid, "').getContext('2d');",
                                 ##"this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, ";",
                                 if (clear) paste0("this_ctx.clearRect(", tlp[1], ", ", tlp[2], ", ", abs(brp[1] - tlp[1]), ", ", abs(brp[2] - tlp[2]), ");"),
                                 "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", tlp[1], ", ", tlp[2], ", ", abs(brp[1] - tlp[1]), ", ", abs(brp[2] - tlp[2]), "); ", panjs, "}; image_", id, "_", z, ".src = '", plot_contents, "';")
                    evaljs(js)
                }
            }
        }

        ## render data to png, but with caching
        tile_to_png <- function(...) {
            keydata <- list(...)
            if (!nzchar(names(keydata)[1])) names(keydata)[1] <- "td"
            keydata$td <- keydata$td$img$data[[keydata$i]] ## don't use other bits of td for cache key calculation
            keydata$i <- NULL ## so that if we ask for a tile in slot N that was previously in slot M, the cached copy can be used
            key <- rlang::hash(keydata)
            ## cat("key:", key, "\n")
            ## cat(str(keydata))
            if (!is.null(cache) && cache$exists(key)) {
                message("got cached png")
                return(cache$get(key))
            } else {
                pltf <- tile_to_png_inner(...)
                if (!is.null(cache) && !is.null(pltf)) cache$set(key, pltf) ## cache it
                pltf
            }
        }

        tile_to_png_inner <- function(td, i, layerdef, res = .plotres, use_fastpng = .use_fastpng, png_compression_level = .png_compression_level, use_png_filter = .use_png_filter, png_in_memory = .png_in_memory) {
            if (is.null(td$img$data[[i]][[1]])) return(NULL)
            pltf <- if (use_fastpng && png_in_memory) NULL else tempfile(tmpdir = tmpd, fileext = ".png")
            message("rendering raster tile to png:", pltf)
            if (use_fastpng) {
                if (layerdef$type == "raster_data") {
                    zl <- if (is.null(layerdef$zlims[[1]])) stop("need z limits") else layerdef$zlims[[1]]
                    cmap <- layerdef$cmap[[1]]
                    pltf <- fastpng::write_png(td$img$data[[i]][[1]], palette = cmap, file = pltf, compression_level = png_compression_level, use_filter = use_png_filter)
                } else {
                    ## image, rgb or greyscale
                    nara <- inherits(td$img$data[[i]][[1]], "nativeRaster")
                    dat <- if (nara) td$img$data[[i]][[1]] else as.vector(matrix(unlist(td$img$data[[i]][[1]]), byrow = TRUE, nrow = 3))
                    if (.debug > 1) temp <- proc.time()["elapsed"]
                    tdim <- attr(td$img$data[[i]], "dimension")
                    pltf <- fastpng::write_png(dat, file = pltf, compression_level = png_compression_level, raw_spec = fastpng::raw_spec(width = tdim[1], height = tdim[2], depth = 3, bits = 8), use_filter = use_png_filter)
                    if (.debug > 1) message("png generation time: ", round(proc.time()["elapsed"] - temp, 3), "s", if (!.png_in_memory) paste0(", png file size: ", round(file.size(pltf) / 1e6, 1), "MB"))
                }
            } else {
                stop("not coded")
            }
            pltf
        }

        ## render data to png, but with caching
        render_to_png <- function(...) {
            keydata <- list(...)
            keydata$td$ids <- keydata$td$xy_hash <- NULL ## don't use these for cache key calculation, those IDs will change from request to request even for the same target
            attr(keydata$image_def$xy, "row.names") <- NULL ## the ordering of attributes "row.names" and "class" of image_def$xy changes as it gets updated, which causes the hash of otherwise-identical copies to change. Just drop the row.names attribute as a workaround
            key <- rlang::hash(keydata)
            if (!is.null(cache) && cache$exists(key)) {
                message("got cached png")
                return(cache$get(key))
            } else {
                pltf <- render_to_png_inner(...)
                if (!is.null(cache) && !is.null(pltf)) cache$set(key, pltf) ## cache it
                pltf
            }
        }

        render_to_png_inner <- function(td, image_def, layerdef, image_wh, res = .plotres, use_fastpng = .use_fastpng, png_compression_level = .png_compression_level, use_png_filter = .use_png_filter, png_in_memory = .png_in_memory) {
            ## not clear that generating png in memory rather than disk is actually better, because the in-memory version has to be serialized to base64 and sent to the browser
            pltf <- if (use_fastpng && png_in_memory) NULL else tempfile(tmpdir = tmpd, fileext = ".png")
            ##IMSRC td$src needs to be per-tile, not per-full-image
            ##IMSRC update tiles_data with src
            message("rendering raster plot:", pltf)
            ##tictoc::tic()
            if (use_fastpng) {
                tdim <- Filter(Negate(is.null), lapply(td$data, attr, "dimension"))
                tdim <- if (length(tdim) > 0) tdim[[1]] else NULL
                if (!is.null(tdim)) {
                    ord <- order(image_def$xy$x, -image_def$xy$y) ## tile ordering
                    n <- image_def$tiles_per_side ## image is composed of a square arrangement of tiles (which may be only one tile)
                    if (layerdef$type == "raster_data") {
                        ## assemble tiles into single matrix. TODO Probably room to make this more efficient
                        m1 <- function(z) matrix(if (is.null(z)) NA_real_ else z, nrow = tdim[2], ncol = tdim[1], byrow = TRUE) ## matrix constructor
                        message("assembling data matrix"); tictoc::tic()
                        big <- do.call(cbind, lapply(seq_len(n), function(ci) {
                            do.call(rbind, lapply((ci - 1) * n + seq_len(n), function(ri) {
                                m1(td$data[[ord[ri]]][[1]])
                            }))
                        }))
                        zl <- if (is.null(layerdef$zlims[[1]])) range(big, na.rm = TRUE) else layerdef$zlims[[1]]
                        cmap <- layerdef$cmap[[1]]
                        big <- pmax(pmin(round((big - zl[1]) / abs(diff(zl)) * (length(cmap) - 1L)) + 1L, length(cmap)), 1L) - 1L ## scale by given zlim and then mapped to colour range, using zero-based indexing
                        tictoc::toc()
                        pltf <- fastpng::write_png(big, palette = cmap, file = pltf, compression_level = png_compression_level, use_filter = use_png_filter)
                        ## saveRDS(td, "/tmp/td.rds")

                        ## something odd with this, does not align properly with other layers
                        ## message("assembling data matrix via nara"); tictoc::tic()
                        ## big <- nara::nr_new(tdim[1] * n, tdim[2] * n, fill = "#00000080")
                        ## xr <- range(image_def$xy$x) + image_def$w/2 * c(-1, 1)
                        ## yr <- range(image_def$xy$y) + image_def$h/2 * c(-1, 1)
                        ## #cat("xr:", str(xr))
                        ## zl <- if (is.null(layerdef$zlims[[1]])) range(unlist(lapply(td$data, function(v) if (is.null(v[[1]])) c(NA, NA) else range(v[[1]], na.rm = TRUE))), na.rm = TRUE) else layerdef$zlims[[1]]
                        ## #cat("zlim:", str(zl))
                        ## cmap <- layerdef$cmap[[1]]
                        ## for (i in seq_along(td$data)) {
                        ##     if (!is.null(td$data[[i]])) {
                        ##         xpos <- (attr(td$data[[i]], "extent")[1] - xr[1]) / diff(xr) * tdim[1] * n
                        ##         ypos <- ((1 - (attr(td$data[[i]], "extent")[3] - yr[1]) / diff(yr)) - 1/n) * tdim[2] * n
                        ##         #cat("xpos:", xpos, "ypos:", ypos, "\n")
                        ##         clamp01 <- function(z) { z[z < 0 | z > 1] <- NA; z }
                        ##         big <- nara::nr_blit(big, x = xpos, y = ypos, src = nara::matrix_to_nr(matrix(clamp01((td$data[[i]][[1]] - zl[1]) / diff(zl)), nrow = tdim[2], ncol = tdim[1], byrow = TRUE), palette = cmap))
                        ##     }
                        ## }
                        ## tictoc::toc()
                        ## pltf <- fastpng::write_png(big, file = pltf, compression_level = png_compression_level, use_filter = use_png_filter)
                    } else {
                        ## image, rgb or greyscale
                        ## if it's greyscale we get one band back, but we assemble here into 3 bands (rgb)
                        ## inefficient but I think fastpng requires it? (can handle 2d matrix but will scale the limits to 0-1)
                        ## if we used gdal_raster_image fastpng::write_png(255 - aperm(array(col2rgb(td$data[[1]][[1]]), c(3, attr(td$data[[1]], "dimension"))), c(3, 2, 1)), filename = pltf, convert_to_row_major = TRUE)

                        ## function for constructing an array from one tile's worth of gdal data
                        a1 <- function(z) {
                            if (is.null(z)) array(NA_integer_, dim = c(tdim, 3)) else array(unlist(lapply(z, function(v) matrix(as.integer(v), byrow = 3, nrow = tdim[1]))), dim = c(tdim, 3))
                        }

                        ##message("assembling matrix"); tictoc::tic()
                        nara <- any(vapply(td$data, function(z) inherits(z[[1]], "nativeRaster"), FUN.VALUE = TRUE))
                        if (n == 1) {
                            big <- if (nara) td$data[[1]][[1]] else as.vector(matrix(unlist(td$data[[1]]), byrow = TRUE, nrow = 3))
                        } else {
                            if (nara) {
                                big <- nara::nr_new(tdim[1] * n, tdim[2] * n, fill = "#00000000")
                                xr <- range(image_def$xy$x) + image_def$w/2 * c(-1, 1)
                                yr <- range(image_def$xy$y) + image_def$h/2 * c(-1, 1)
                                toplot <- !vapply(td$data, is.null, FUN.VALUE = TRUE)
                                if (any(toplot)) { ## surely this has to be TRUE, but just in case
                                    if (TRUE) {
                                        ## these two implementations are similarly fast
                                        xpos <- sapply(td$data[toplot], function(v) (attr(v, "extent")[1] - xr[1]) / diff(xr) * tdim[1] * n)
                                        ypos <- sapply(td$data[toplot], function(v) ((1 - (attr(v, "extent")[3] - yr[1]) / diff(yr)) - 1/n) * tdim[2] * n)
                                        nara::nr_blit_list(big, x = xpos, y = ypos, src_list = lapply(td$data[toplot], function(v) v[[1]]), src_idx = seq_len(sum(toplot)))
                                    } else {
                                        for (i in seq_along(td$data)) {
                                            if (!is.null(td$data[[i]])) {
                                                xpos <- (attr(td$data[[i]], "extent")[1] - xr[1]) / diff(xr) * tdim[1] * n
                                                ypos <- ((1 - (attr(td$data[[i]], "extent")[3] - yr[1]) / diff(yr)) - 1/n) * tdim[2] * n
                                                nara::nr_blit(big, x = xpos, y = ypos, src = td$data[[i]][[1]])
                                            }
                                        }
                                    }
                                }
                            } else {
                                big <- do.call(abind, c(lapply(seq_len(n), function(ci) { ## columns 1..n
                                    do.call(abind, c(lapply((ci - 1) * n + seq_len(n), ## rows
                                                            function(ri) a1(td$data[[ord[ri]]][1:3])), list(along = 1)))
                                }), list(along = 2)))
                            }
                        }
                        if (.debug > 1) temp <- proc.time()["elapsed"]
                        pltf <- fastpng::write_png(big, file = pltf, compression_level = png_compression_level, raw_spec = fastpng::raw_spec(width = tdim[1], height = tdim[2], depth = 3, bits = 8), use_filter = use_png_filter)
                        if (.debug > 1) message("png generation time: ", round(proc.time()["elapsed"] - temp, 3), "s", if (!.png_in_memory) paste0(", png file size: ", round(file.size(pltf) / 1e6, 1), "MB"))
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
                ## not using fastpng. TODO use unigd?
                png(pltf, height = image_wh, width = image_wh, res = res, bg = NA)
                opar <- par(no.readonly = TRUE)
                par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") ## zero margins
                on.exit(par(opar))
                iext <- calc_img_ext(image_def)
                plot(0, 0, type = "n", axes = FALSE, xlim = iext[1:2], ylim = iext[3:4])
                for (tl in seq_len(min(4, length(td$data)))) {
                    ext <- xywh_to_ext(x = image_def$xy$x[tl], y = image_def$xy$y[tl], w = image_def$w, h = image_def$h)
                    if (!is.null(td$data[[tl]])) {
                        ximage::ximage(td$data[[tl]], extent = ext, asp = 1, zlim = layerdef$zlims[[1]], col = layerdef$cmap[[1]], add = TRUE)
                    } else {
                        text(x = runif(50) * (ext[2] - ext[1]) + ext[1], y = runif(50) * (ext[4] - ext[3]) + ext[3], labels = rep("?", 50))
                    }
                }
                dev.off()
            }
            ## tictoc::toc()
            pltf
        }

        observe({
            req(image_def())
cat("--> in vector layer plotter\n")
            ## deal with non-raster layers for which the user has provided a plot function
            iext <- calc_img_ext(image_def())
            for (i in seq_along(layerdef())) do_vector_plot(i, iext)
        }, priority = 9)

        do_vector_plot <- function(i, iext, and_stop = FALSE) {
            ld <- isolate(layerdef())
            if (missing(iext)) iext <- calc_img_ext(image_def())
            if (and_stop) browser()
            if (!is.null(ld[[i]]) && "fun" %in% names(ld[[i]])) {
                z <- ld[[i]]$z
                cat("rendering plot", i, "layer", z, " as", if (.use_ugd) "svg\n" else "png\n")
                ##tictoc::tic()
                pltf <- tempfile(tmpdir = tmpd, fileext = ".png")
                ##IMSRC need to keep track of pltf for each non-raster source as well
                if (!.use_ugd) {
                    png(pltf, height = image_wh, width = image_wh, res = .plotres, bg = NA)
                } else {
                    unigd::ugd(width = image_wh, height = image_wh, bg = "transparent")
                }
                opar <- par(no.readonly = TRUE)
                par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") ## zero margins
                ok <- ld[[i]]$fun(xlim = iext[1:2], ylim = iext[3:4])
                if (ok && .use_ugd) {
                    pltf <- unigd::ugd_render(as = "svg")
                    pltf <- sub("<rect width=\"100%\" height=\"100%\" style=\"stroke: none;fill: #FFFFFF;\"/>", "", pltf, fixed = TRUE)
                }
                par(opar)
                dev.off()
                ##tictoc::toc()
                if (.use_ugd) {
                    if (ok) send_plot(pltf, z, as = "svg") else clear_plot(z)
                } else {
                    if (ok) send_plot(pltf, z) else clear_plot(z)
                }
            }
        }


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
                cat("select mu:", mxy, "\n") ## [xmin xmax ymin ymax] of selected area in (absolute) map units
                vps_mu <- pixels_to_mu(get_viewport_size()) ## viewport size in map units
                zoomf <- min(vps_mu[1] / abs(diff(mxy[c(1, 2)])), vps_mu[2] / abs(diff(mxy[c(3, 4)])))
                zoomc <- c((mxy[1] + mxy[2]) / 2, (mxy[3] + mxy[4]) / 2)
                ##cat("zoom factor:", zoomf, ", with centre:", zoomc, "\n")
                do_zoom(zoomc, zoom_by = zoomf)
            }
        })

        observeEvent(input$do_zoom, {
            req(input$do_zoom)
            cat("zoom:", utils::capture.output(utils::str(input$do_zoom)), "\n")
            ## pressing the zoom in/out buttons will give a vector with [zoom_direction left_offset top_offset] where the offsets are the current css offsets of the viewport in pixels
            ## and so (left_offset + viewport_width/2, top_offset + viewport_height/2) is the viewport-centre-pixel of the unzoomed image (also of the canvas), from which we can calculate the map coords on which to centre the zoom mxy with e.g.
            ## mxy <- calc_img_ext(image_def())[c(1, 4)] + c(1, -1) * pixels_to_mu(input$do_zoom[2:3] + get_viewport_size() / 2)
            ## BUT mxy should be the same as viewport_ctr(), and mxy seems to suffer from pixel-rounding errors, so use viewport_ctr() at least for now
            do_zoom(in_out = input$do_zoom[1])
        })

        do_zoom <- function(mxy, in_out, zoom_by) {
            ## mxy is the point on which to centre the zoom, in map units
            if (missing(mxy)) mxy <- isolate(viewport_ctr())
            cat("do_zoom, ctr: ", mxy, "\n")
            ## mxy is map units of centre point of zoom
            ## in_out is 1 for in, -1 for out, or specify zoom_by (zoom factor)
            if (missing(in_out)) in_out <- (zoom_by > 1) * 2L - 1L
            if (missing(zoom_by)) zoom_by <- NA
            ## TODO we might need a zoom-out limit?
            if (in_out > 0) {
                if (is.na(zoom_by)) zoom_by <- 2
            } else {
                if (is.na(zoom_by)) zoom_by <- 1/2
            }
            ## some things before zooming
            vps_mu <- pixels_to_mu(get_viewport_size()) ## current viewport size in map units
            ## update the tile centres, width, height
            idef0 <- idef <- image_def()
            tiles_data_before_zoom <- isolate(tiles_data())
            idef$res <- idef$res / zoom_by
            idef$w <- idef$w / zoom_by
            idef$h <- idef$h / zoom_by
##                 ## ^^^ TODO align to common grid boundaries to help with caching
##                 ## we will always align our tiles so that they align with (x) initial_view$extent[1] + N * CURRENT_tile_w and (y) initial_view$extent[3] + M * CURRENT_tile_h
##                 ## so N = (newx0 - initial_view$extent[1])/CURRENT_tile_w
##                 newx0 <- mxy[1] - idef$w / 2
##                 N <- round((newx0 - initial_view$extent[1])/idef$w)
##                 initial_view$extent[1] + N * idef$w
##                 newx0 <- min(idef0$xy$x) - idef0$w/2
## #N * current_tile_w + initial_view$extent[1]
## browser()
                
        ##view_ref <- list(x0 = initial_view$extent[1], y0 = initial_view$extent[3], 
#        image_def <- reactiveVal(list(tiles_per_side = initial_view$tiles_per_side,
#                                      n_tiles = initial_view$tiles_per_side ^ 2,
#                                      xy_grid = temp_xygrid, ## xy centres of tiles in normalized [-1 1 -1 1] coords
#                                      xy = as.data.frame(tempxy), ## xy centres of tiles in map coords
#                                      w = tile_w, h = tile_h, ## tile width and height in map coords
#                                      res = initial_view$res))

                ## mxy is where the zoomed view should be centred, but not necessarily the centre of the zoomed image extents
                
            idef$xy$x <- mxy[1] + idef$w * image_def()$xy_grid[, 1]
            idef$xy$y <- mxy[2] + idef$h * image_def()$xy_grid[, 2]
            image_def(idef)
            viewport_ctr(mxy)
            resend_tiles <- function(plotnum, tiles_data, zoomf, clear = .clear_canvas_before_drawing) {
                if (length(layerdef()[[plotnum]]) > 0) {
                    for (i in seq_along(tiles_data[[plotnum]]$img$data)) {
                        draw_tile(plotnum, i = i, td = tiles_data[[plotnum]], image_def = idef, layerdef = layerdef())
                    }
                }
            }
            ## TODO if .clear_on_zoom, clear the canvas elements here BUT if we are copying canvas to canvas, we need to have saved them first
            set_pan() ## re-centre the viewport on the canvas
            for (z in which_are_raster_layers()) {
                if (.clear_on_zoom) clear_canvas(z)
                ## TODO is it faster to redraw from the canvas? If the user connection is slow, resend_tiles requires re-sending the full image data for each tile to the canvas from the server (although browser caching might handle it)
                resend_tiles(plotnum = z, tiles_data = tiles_data_before_zoom, zoomf = zoom_by)
            }
            for (z in which_are_vector_layers()) {
                ## also need to replot the vector layers at their scaled sizes
                iext_mu <- calc_img_ext(idef) ## full canvas extent in map units, e.g. [-2 2 -2 2]e7
                ## the normalized x and y extents of the existing (unzoomed) vector layer in that canvas extent
                iext0_mu <- calc_img_ext(idef0)
                xn <- (iext0_mu[1:2] - iext_mu[1]) / diff(iext_mu[1:2])
                yn <- 1 - (iext0_mu[3:4] - iext_mu[3]) / diff(iext_mu[3:4]) ## 1 - because we draw from top
                ## and thus the region in pixels in the canvas for this tile
                tlp <- c(xn[1], yn[2]) * image_wh
                brp <- c(xn[2], yn[1]) * image_wh
                ## redraw the canvas
                plotid <- paste0(id, "-plot", z)
                ## draw to off-screen canvas then replace the on-screen one
                js <- paste0("var this_cvs = document.getElementById('", plotid, "'); var offs_cvs = document.createElement('canvas'); offs_cvs.width = '", image_wh, "'; offs_cvs.height = '", image_wh, "'; var offs_ctx = offs_cvs.getContext('2d'); offs_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, "); offs_ctx.imageSmoothingEnabled = false; offs_ctx.drawImage(this_cvs, ", tlp[1], ", ", tlp[2], ", ", abs(brp[1] - tlp[1]), ", ", abs(brp[2] - tlp[2]), ");",
                             ## now clear the on-screen one
                             "var this_ctx = this_cvs.getContext('2d'); this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, "); this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(offs_cvs, 0, 0, ", image_wh, ", ", image_wh, ");")
                evaljs(js)
            }
            ## TODO perhaps, instead of doing the intermediate redraw of each layer individually, collapse them all into a single layer so that we don't get the temporary out-of-sync effect
            cat("out zoom\n")
        }

        clear_canvas <- function(z) {
            plotid <- paste0(id, "-plot", z)
            js <- paste0("var this_ctx = document.getElementById('", plotid, "').getContext('2d'); this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, ");")
            evaljs(js)
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
