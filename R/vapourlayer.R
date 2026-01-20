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
#' @param view_wh numeric: two-element vector giving the width and height of the map widget as percentages of the browser window width and height in valid css units (e.g. `view_wh = c("400px", "250px")` or `view_wh = c("60vw", "40vh")`
#' @param image_wh integer: image size in pixels. Depending on the rendering mechanism, the native image size might not actually be this, but the browser will scale to this size
# @param min_scale scalar: the smallest cell size (in projected units), used when maximally zoomed in
## no, min_scale should be determined by the data. Or maybe allow min_scale to be specified but by default it's NA, in which case it falls back to the native data res
#' @param initial_view list: a named list with components:
#' * tiles_per_side integer: e.g. a value of 2 means that we will generate the image as 2x2 set of tiles. If > 1 must be even? TODO note that >2 currently doesn't work properly when zooming
#' * extent numeric: the initial image extent c(xmin, xmax, ymin, ymax) in projected coordinates
#' * res numeric: the resolution (in m) to use for the image when shown at its intial extent. The resolution will change as the map is zoomed in/out
# ## not yet implemented * max_extent numeric: as for `extent`, but defining the limits that will be shown. The map will not be extended beyond these bounds as it is zoomed/panned. If not provided, the initial extent will be used
#' @param layerdef reactive: TBD
#' @param target_crs string: target projection CRS string
#' @param cache logical or cachem: either TRUE/FALSE to use/not the default memory-based cache, or a `cachem` object (as returned by e.g. [cachem::cache_mem()])
#'
#' @return The UI and server components of a Shiny module. When instantiated, the server returns a list with components:
#' * click function:
#' * layer_data list: a list of reactive objects, where each object contains the raster data associated with the corresponding layer (as a `terra::rast` object). Note that the data will be NULL for anything other than a raster layer of type "raster_data" (i.e. "raster_image_rgb", "raster_image_grey", or vector layers will all be NULL)
#'
# @examples
#'
#' @export
#' @rdname vl_map_module
vl_map_ui <- function(id, view_wh = c("40vw", "40vh")) {
    tagList(
        tags$head(singleton(htmltools::includeCSS(system.file("extdata/css/vapourlayer.css", package = "cloudymapr"))),
                  tags$style(paste0("#", id, "-plot", 1:9, " { z-index:-", 9:1, "; }", collapse = " ")), ## plot1 lowest, plot9 top-most
                  tags$style(paste0("#", id, " { width:", view_wh[1], "; height:", view_wh[2], "; }")),
                  singleton(tags$script("$(document).on('shiny:sessioninitialized', function() { Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) }); });")),
                  tags$script(HTML(paste(gsub("$ID$", id, readLines(system.file("extdata/js/vapourlayer.js", package = "cloudymapr"), warn = FALSE, encoding = "UTF-8"), fixed = TRUE), collapse = "\n")))
                  ),
        tags$div(style = "position:relative; margin-top:1px;",
                 tags$div(id = NS(id, "plot_controls"), class = "vl-plot-controls",
                          tags$button(id = NS(id, "zoom_in"), class = "btn btn-default", icon("magnifying-glass-plus", id = NS(id, "zoom-in-icon")), title = "Zoom in"),
                          tags$button(id = NS(id, "zoom_out"), class = "btn btn-default", icon("magnifying-glass-minus", id = NS(id, "zoom-out-icon")), title = "Zoom out"),
                          actionButton(NS(id, "pan_button"), class = "btn btn-default", label = icon("hand", id = NS(id, "pan-icon")), title = "Pan map", onclick = paste0("$('#", NS(id, "pan-icon"), "').removeClass('icon-disabled'); $('#", NS(id, "select-icon"), "').addClass('icon-disabled'); cm_", id, ".select_mode='pan';"))##,
                          ## temporarily actionButton(NS(id, "select_button"), class = "btn btn-default", label = icon("object-group", id = NS(id, "select-icon"), class = "icon-disabled"), title = "Select region", onclick = paste0("$('#", NS(id, "select-icon"), "').removeClass('icon-disabled'); $('#", NS(id, "pan-icon"), "').addClass('icon-disabled'); cm_", id, ".select_mode='select';"))
                          ),
                 tags$div(id = id, class = "viewport",
                          tags$canvas(id = NS(id, "canvas"), class = "viewport-canvas"), ## used for the panning, rectangle-dragging, etc
                          do.call(tags$div, c(list(id = NS(id, "pannable"), class = "viewport-pannable"),
                                              lapply(9:1, function(z) tags$canvas(id = NS(id, paste0("plot", z)), class = "viewport-image")))))
                 )
    )
}

#' @export
#' @rdname vl_map_module
vl_map_ui_postamble <- function() {
    htmltools::includeScript(system.file("extdata/js/vapourlayer-post.js", package = "cloudymapr"))
}

#' @export
#' @rdname vl_map_module
vl_map_server <- function(id, image_wh = 4096, initial_view = list(tiles_per_side = 1L, extent = c(-1, 1, -1, 1) * 2048e4, res = 32e3), layerdef, target_crs = "EPSG:3031", cache = TRUE) {

    ## config
    .debug <- 1L
    .clear_canvas_before_drawing <- TRUE ## for vector layers, do we clear the full canvas before replotting?
    .use_ugd <- FALSE ## for vector layers, use unigd:ugd to generate svg. If FALSE use png format
    .svg_as_file <- TRUE ## if FALSE, send the svg string directly to the browser as b64-encoded data: but think that using a file is better for caching behaviour
    .use_fastpng <- TRUE ## if FALSE use png()
    .png_in_memory <- FALSE
    .png_compression_level <- 2L ## 0L is no compression, ~0.1s to write a 2500x2500 image (25MB file); 2L ~1.4s for the same image (9.5MB) or ~0.9s with .use_png_filter = FALSE (11MB file)
    .use_png_filter <- .png_compression_level < 1 ## see fastpng::write_png
    .plotres <- 96 ## dpi, only used by png graphics device
    .warp_opts <- c("-wm", "999")
    .resampling_method <- "near" ## or "bilinear" ## TODO allow this to be specified on a source-by-source basis? Be aware that bilinear sampling can cause issues when the source is raster_data and it has special values like a land mask (255) but valid values are (say) 0-100: interpolation near land will give values > 100 and < 255
    ## the next two should not ever need to be TRUE any more
    .clear_on_zoom <- FALSE ## clear plots on zoom? Or leave them visible while refreshing?
    .clear_on_pan <- FALSE ## clear plots on pan? Or leave them visible while refreshing?

    ## initial arg checking
    iv <- initial_view
    if (is.null(iv$tiles_per_side)) iv$tiles_per_side <- 1L
    if (is.null(iv$extent) || is.null(iv$res)) stop("initial_view must contain `extent` and `res` components")
    if (is.null(iv$max_extent)) iv$max_extent <- iv$extent ## currently ignored
    initial_view <- iv

    if (is.logical(cache)) {
        cache <- if (isTRUE(cache)) cachem::cache_mem() else NULL
    } else if (!inherits(cache, "cachem")) {
        warning("`cache` should be a cachem object (e.g. cachem::cache_mem()), replacing with default memory-based cache")
        cache <- cachem::cache_mem()
    }

    moduleServer(id, function(input, output, session) {
        tmpd <- tempfile()
        dir.create(tmpd)
        cat("plots temporary directory is:", tmpd, "\n")
        onSessionEnded(function() unlink(tmpd, recursive = TRUE))
        addResourcePath("plots", tmpd)

        ## funs
        is_raster_layer <- function(i) {
            !is.null(layerdef()[[i]]) && "dsn" %in% names(layerdef()[[i]])
        }
        which_are_raster_layers <- function() {
            isolate(which(vapply(seq_along(layerdef()), is_raster_layer, FUN.VALUE = TRUE)))
        }
        which_are_vector_layers <- function() {
            isolate(which(!vapply(seq_along(layerdef()), is_raster_layer, FUN.VALUE = TRUE)))
        }

        ## xy centres of tiles in map coords
        ext_to_c_mu <- function(extent, tiles_per_side, xygrid) {
            ## xygrid is normalized coords of centres in [-1 1 -1 1] space
            if (!missing(xygrid)) {
                ## this is slower
                data.frame(x = (xygrid$x + 1) / 2 * diff(extent[1:2]) + extent[1],
                           y = (xygrid$y + 1) / 2 * diff(extent[3:4]) + extent[3])
            } else {
                as.data.frame(expand.grid(x = seq(extent[1], extent[2], length.out = tiles_per_side + 1)[-1] - diff(extent[1:2]) / tiles_per_side / 2,
                                          y = seq(extent[3], extent[4], length.out = tiles_per_side + 1)[-1] - diff(extent[3:4]) / tiles_per_side / 2))
            }
        }

        ## tile width and height in map units
        tile_wh_mu <- function(ext, tiles_per_side) {
            c(diff(ext[1:2]), diff(ext[3:4])) / tiles_per_side
        }

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

        ## we will always align our tiles so that they align with (x) initial_view$extent[1] + N * CURRENT_tile_w and (y) initial_view$extent[3] + N * CURRENT_tile_h
        ##view_ref <- list(x0 = initial_view$extent[1], y0 = initial_view$extent[3], 

        ## xy centres of tiles in normalized [-1 1 -1 1] coords
        temp_xygrid <- expand.grid(x = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side,
                                   y = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side)
        image_def <- reactiveVal(list(tiles_per_side = initial_view$tiles_per_side,
                                      n_tiles = initial_view$tiles_per_side ^ 2,
                                      xy_grid = temp_xygrid, ## xy centres of tiles in normalized [-1 1 -1 1] coords, these do not change (only dependent on n tiles per side)
                                      ext = initial_view$extent, ## the extent value is maintained on the client side, and sent here to the server when it changes
                                      res = initial_view$res, zoom = 1))
        ## testing stopifnot(isTRUE(all.equal(as.matrix(ext_to_c_mu(extent = initial_view$extent, xygrid = temp_xygrid)), as.matrix(ext_to_c_mu(extent = initial_view$extent, tiles_per_side = initial_view$tiles_per_side))))) ## as.matrix just to avoid the attributes on the tiles_per_side version
        tile_wh <- round(image_wh / initial_view$tiles_per_side) ## in pixels

        xywh_to_ext <- function(x, y, w, h) unname(c(x + c(-1, 1) * w / 2, y + c(-1, 1) * h / 2))

        ## set initial scaling parms for conversion of map units to pixels
        init_done <- FALSE
        observeEvent(input$request_init, {
            extstr <- paste0("[", initial_view$extent[1], ",", initial_view$extent[2], ",", initial_view$extent[3], ",", initial_view$extent[4], "];")
            ctrstr <- paste0("[", (initial_view$extent[1] + initial_view$extent[2]) / 2, ",", (initial_view$extent[3] + initial_view$extent[4]) / 2, "];")
            alstr <- paste0("[", paste(sapply(layerdef(), function(w) w$z), collapse = ","), "];") ## active layers, 1-indexed
            ## TODO allow centre to be specifed as something else?
            evaljs(paste0("cm_", id, ".xsc=", diff(initial_view$extent[1:2]) / image_wh, ";",
                          "cm_", id, ".ysc=", diff(initial_view$extent[3:4]) / image_wh, ";",
                          "cm_", id, ".xoff=0;cm_", id, ".yoff=0;",
                          "cm_", id, ".imxoff=", image_wh / 2, ";cm_", id, ".imyoff=", image_wh / 2, ";",
                          "cm_", id, ".image_wh=", image_wh, ";",
                          "cm_", id, ".res=", initial_view$res, ";",
                          "cm_", id, ".viewport_ctr=", ctrstr,
                          "cm_", id, ".active_layers=", alstr,
                          "cm_", id, ".ext=", extstr, "cm_", id, ".ext0=", extstr
                          ))
            init_done <<- TRUE
        })

        ## initial setup
        ## this doesn't need to be reactive, it only relies on constants (image_wh, id) and the dom being initialized, which it will be by the time this is called
        ## set the canvas sizes and store array of contexts
        evaljs(paste0("$('#", id, "-pannable').width('", image_wh, "px').height('", image_wh, "px'); ",
                      id, "_ctxlist = []; for (let i = 1; i <= 9; i++) { var this_ctx = document.getElementById('", id, "-plot' + i).getContext('2d'); this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, "; ", id, "_ctxlist[i] = this_ctx; }"))

        observeEvent(input$do_zoom, {
            cat("do_zoom: ", input$do_zoom, "\n")
            if (.clear_on_zoom) clear_tiles_data()
            i <- image_def()
            i$ext <- input$do_zoom[1:4]
            i$res <- input$do_zoom[5]
            i$zoom <- input$do_zoom[6]
            cat("idef:", str(i), "\n")
            image_def(i)
        })

        observeEvent(input$pan_extend, {
            cat("pan extend: ", input$pan_extend, "\n")
            ## update the image_def()$ext to the new value just sent through
            if (.debug > 0) cat("--> extending tiles\n")
            if (.clear_on_pan) clear_tiles_data()
            i <- image_def()
            i$ext <- input$pan_extend
            ## the viewport centre (in map units) is input$pan_extend[5:6]
            ## TODO constrain the limits to the initial_view$max_extent
            ## TODO we can optimize the next rendering by shuffling the tiles data: if we've panned left with a 2x2 tile arrangement, then the left-hand tile data can be moved into the right-hand tiles
            image_def(i) ## update the image reactive
            ## ask for pan to be reset when the next plot occurs (don't do it directly here, otherwise we're panning before the re-plot)
        })

        send_plot <- function(plot_contents, plotnum, image_def, ext_mu, clear = .clear_canvas_before_drawing, as = "file") { ## x = 0, y = 0, w = image_wh, h = image_wh, 
            ## send_plot is used for vector layers, not tiled layers (see draw_tile for those)
            cat("sendplot: plot", plotnum, "updated, sending to js ")
            plotid <- paste0(id, "-plot", plotnum)
            if (missing(ext_mu)) ext_mu <- image_def$ext
            ## idef <- isolate(image_def())
            if (as == "svg" && !.svg_as_file) {
                cat("as svg\n")
                plot_contents <- paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)))
            } else {
                ## plot_contents is a file or raw vector
                if (is.raw(plot_contents)) {
                    cat("as b64 png from raw\n")
                    plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                } else {
                    cat("as image from file\n")
                    plot_contents <- paste0("plots/", basename(plot_contents))
                }
            }
            image_js(z = plotnum, ext_mu = ext_mu, plot_contents = plot_contents, image_def = image_def, clear = clear)
        }

        clear_plot <- function(plotnum) {
            ## can be multiple plotnums
            cat("clearing plot", plotnum, "\n")
            evaljs(paste0("var this_ctx = ", id, "_ctxlist[", plotnum, "]; if (this_ctx) { this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, "); }"))
            vector_plot_sources[plotnum] <<- NA_character_
        }

        tiles_data <- reactiveVal(rep(list(list(img = NULL)), 9)) ## the tiles for each layer, as will be fetched with gdal
        layer_data <- lapply(1:9, function(z) {
            reactive({
                if (isTRUE(isolate(layerdef()[[z]])$type %in% c("raster_data", "raster_image_grey"))) {
                    terra::rast(tiles_to_matrix(tiles_data()[[z]]$img$data), crs = target_crs, extent = image_def()$ext)
                } else {
                    ## TODO other image types. RGB should be able to return RGB colours as a 3-band raster
                    NULL
                }
            })
        })

        clear_tiles_data <- function(z) {
            if (missing(z)) z <- which_are_raster_layers()
            td <- isolate(tiles_data())
            for (i in z) td[[i]] <- list(img = NULL)
            tiles_data(td)
            clear_plot(z)
        }

        tiles_to_matrix <- function(td_img_data) {
            tdim <- Filter(Negate(is.null), lapply(td_img_data, attr, "dimension"))
            tdim <- if (length(tdim) > 0) tdim[[1]] else return(NULL) ## if we have no dimension, we can't build the matrix
            m1 <- function(z) matrix(if (is.null(z)) NA_real_ else if (is.raw(z)) as.integer(z) else z, nrow = tdim[2], ncol = tdim[1], byrow = TRUE) ## matrix constructor
            idef <- isolate(image_def())
            n <- idef$tiles_per_side ## image is composed of a square arrangement of tiles (which may be only one tile)
            ord <- order(idef$xy_grid$x, -idef$xy_grid$y) ## tile ordering
            if (.debug > 1) temp <- proc.time()["elapsed"]
            out <- do.call(cbind, lapply(seq_len(n), function(ci) {
                do.call(rbind, lapply((ci - 1) * n + seq_len(n), function(ri) {
                    m1(td_img_data[[ord[ri]]][[1]])
                }))
            }))
            if (.debug > 1) {
                temp <- proc.time()["elapsed"] - temp
                message("data matrix assembly time: ", round(temp, 3), "s")
            }
            out
        }

        handle_tile_data <- function(result) {
            td <- isolate(tiles_data()) ## tiles for all layers
            td2 <- td[[result$z]] ## tiles for layer z
            if (result$id %in% td2$img$ids) {
                i <- which(result$id == td2$img$ids)
                td2$img$data[[i]] <- result$data
                td2$img$data_hash <- rlang::hash(td2$img$data)
                td[[result$z]] <- td2
                tiles_data(td)
                isolate(draw_tile(z = result$z, td = td2, i = i, image_def = image_def(), layerdef = layerdef()))
            }
        }

        draw_tile <- function(z, td, i, image_def, layerdef, clear = TRUE, as = "file") {
            message("draw_tile for layer ", z, ", index ", i)
            ## z is the layer index (e.g. into tiles_data()
            ## td is that tiles_data() entry
            ## i is the index within the layer (> 1 if we have multiple tiles per side)
## this should not be needed now because we do an initial re-draw of each canvas on the client side after panning or zooming
##             if (clear) {
##                 ## if we are about to draw a tile, then we also need to clear the tiles that have NULL data (e.g. we've just panned, and we don't yet have data for some tiles. If we don't clear these, the old tiles will remain shown)
##                 for (j in setdiff(seq_along(td$img$data), i)) {
##                     if (is.null(td$img$data[[j]])) {
##                         message("img data is NULL for tile ", j, ", clearing tile")
##                         message("TODO adjust clear code for zooming!") ## TODO
## ##                        ## image_def$xy_grid gives the xy centres of tiles in normalized [-1 1 -1 1] coords
## ##                        half_nwh <- 2 / image_def$tiles_per_side / 2 ## tile half-width and half-height in normalized [-1 1 -1 1] coords
## ##                        xn <- (image_def$xy_grid$x[j] + c(-half_nwh, half_nwh) + 1)/2 ## x-extent of tile in 0-1 coords
## ##                        yn <- 1 - (image_def$xy_grid$y[j] + c(-half_nwh, half_nwh) + 1) / 2 ## y-extent of tile in 0-1 coords
## ##                        cat("xn is: ", xn, ", yn is: ", yn, "\n")
## ##                        tlp <- c(xn[1], yn[2]) * image_wh
## ##                        brp <- c(xn[2], yn[1]) * image_wh
## ##                        js <- paste0("this_ctx = ", id, "_ctxlist[", z, "]; this_ctx.clearRect(", tlp[1], ", ", tlp[2], ", ", abs(brp[1] - tlp[1]), ", ", abs(brp[2] - tlp[2]), ");")
## ##                        evaljs(js)
##                     }
##                 }
##             }
            ## send tile to canvas
            plot_contents <- tile_to_png(td = td, i = i, layerdef = layerdef[[z]])
            if (!is.null(plot_contents)) {
                iext_mu <- image_def$ext ## data extent in map units
                tile_ext_mu <- attr(td$img$data[[i]], "extent")
                ## the normalized x and y extents of this tile in that canvas extent
                xn <- (tile_ext_mu[1:2] - iext_mu[1]) / diff(iext_mu[1:2])
                yn <- 1 - (tile_ext_mu[3:4] - iext_mu[3]) / diff(iext_mu[3:4]) ## 1 - because we draw from top
                ## and thus the region in pixels in the canvas for this tile
                tlp <- c(xn[1], yn[2]) * image_wh
                brp <- c(xn[2], yn[1]) * image_wh

                cat("xn is: ", xn, ", yn is: ", yn, "\n")
                ## top-left/bottom-right (in pixels) of this tile in the image, remembering that the canvas origin (0, 0) is top-left
                                        #            tlp <- (image_def$xy_grid[i, ] + 1)/2 + 1/image_def$tiles_per_side/2 * c(-1, 1); tlp[2] <- 1 - tlp[2]; tlp <- tlp * image_wh
                                        #            brp <- (image_def$xy_grid[i, ] + 1)/2 + 1/image_def$tiles_per_side/2 * c(1, -1); brp[2] <- 1 - brp[2]; brp <- brp * image_wh

                cat("sending tile to js ")
                plotid <- paste0(id, "-plot", z)

                ## send the extent in map units to the client and let it work out the tlbr coords in the canvas
                if (as == "svg" && !.svg_as_file) {
                    cat("as svg\n")
                    ## plot contents is an svg string
                    ## js <- paste0("var image_", id, "_", z, " = new Image(); image_", id, "_", z, ".onload = function() { this_ctx = ", id, "_ctxlist[", z, "];",
                    ##              if (clear) paste0("this_ctx.clearRect(", tlp[1], ", ", tlp[2], ", ", brp[1] - tlp[1], ", ", brp[2] - tlp[2], ");"),
                    ##              "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, ", tlp[1], ", ", tlp[2], ", ", brp[1] - tlp[1], ", ", brp[2] - tlp[2], "); ", panjs, "}; image_", id, "_", z, ".src = 'data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)), "';")
                    ## evaljs(js)
                    plot_contents <- paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)))
                } else {
                    ## plot_contents is a file or raw vector
                    if (is.raw(plot_contents)) {
                        cat("as b64 png from raw\n")
                        plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                    } else {
                        cat("as image from file\n")
                        plot_contents <- paste0("plots/", basename(plot_contents))
                    }
                }
                image_js(z = z, ext_mu = tile_ext_mu, plot_contents = plot_contents, image_def = image_def, clear = clear)
            }
        }

        ## draw image on a canvas (and return the js string). If `do_eval` is FALSE, just return the string
        image_js <- function(z, ext_mu, plot_contents, image_def = 1, clear = TRUE, do_eval = TRUE) {
            js <- paste0("var image_", id, "_", z, " = new Image(); image_", id, "_", z, ".onload = function() { this_ctx = ", id, "_ctxlist[", z, "];",
                         "var tl = cm_", id, ".m2px([", ext_mu[1], ", ", ext_mu[4], "]); var br = cm_", id, ".m2px([", ext_mu[2], ", ", ext_mu[3], "]);",
                         ## "console.log('tl: ' + tl + ', br: ' + br);",
                         if (clear) paste0("this_ctx.clearRect(tl[0], tl[1], br[0] - tl[0], br[1] - tl[1]);"),
                         "this_ctx.imageSmoothingEnabled = false; this_ctx.drawImage(this, tl[0], tl[1], br[0] - tl[0], br[1] - tl[1]); };",
                         "image_", id, "_", z, ".onerror = function(e) { console.log(e) };",
                         "image_", id, "_", z, ".src = '", plot_contents, "';"##,
                         )
            if (do_eval) {
                evaljs(js)
                invisible(js)
            } else {
                js
            }
        }

        ## render data to png, but with caching
        tile_to_png <- function(...) {
            keydata <- list(...)
cat("tile_to_png\n")
cat(utils::str(keydata))
            if (!nzchar(names(keydata)[1])) names(keydata)[1] <- "td"
            keydata$td <- if (keydata$i > length(keydata$td$img$data)) NULL else keydata$td$img$data[[keydata$i]] ## don't use other bits of td for cache key calculation
            keydata$i <- NULL ## so that if we ask for a tile in slot N that was previously in slot M, the cached copy can be used
            key <- rlang::hash(keydata)
            cat("key:", key, "\n")
            cat(utils::str(keydata))
            if (!is.null(cache) && cache$exists(key)) {
                if (file.exists(cache$get(key))) {
                    message("got cached png: ", cache$get(key))
                    return(cache$get(key))
                } else {
                    cache$remove(key)
                }
            }
            message("no cached png available")
            pltf <- tile_to_png_inner(...)
            if (!is.null(cache) && !is.null(pltf)) cache$set(key, pltf) ## cache it
            pltf
        }

        tile_to_png_inner <- function(td, i, layerdef, res = .plotres, use_fastpng = .use_fastpng, png_compression_level = .png_compression_level, use_png_filter = .use_png_filter, png_in_memory = .png_in_memory) {
cat("tile_to_png_inner\n")
cat(utils::str(td), "\n")
            if (i > length(td$img$data) || is.null(td$img$data[[i]][[1]])) return(NULL)
            pltf <- if (use_fastpng && png_in_memory) NULL else tempfile(tmpdir = tmpd, fileext = ".png")
            message("rendering raster tile to png:", pltf)
            if (use_fastpng) {
                if (layerdef$type == "raster_data") {
                    zl <- if (is.null(layerdef$zlims[[1]])) stop("need z limits") else layerdef$zlims[[1]]
                    cmap <- layerdef$cmap[[1]]
                    tdim <- attr(td$img$data[[i]], "dimension")
                    m <- pmax(pmin(round((matrix(td$img$data[[i]][[1]], nrow = tdim[2], byrow = TRUE) - zl[1]) / abs(diff(zl)) * (length(cmap) - 1L)) + 1L, length(cmap)), 1L) - 1L ## construct matrix, scale by given zlim and then map to colour range, using zero-based indexing
                    ## any NA/NaN's will still be present, make them transparent
                    cmap <- c(cmap, "#FFFFFF00") ## transparent
                    m[is.na(m)] <- length(cmap) - 1L
                    rgs <- list(m, palette = cmap, file = pltf, compression_level = png_compression_level, use_filter = use_png_filter)
                } else if (layerdef$type == "raster_image_grey") {
                    ## greyscale image
                    dat <- as.vector(td$img$data[[i]][[1]])
                    tdim <- attr(td$img$data[[i]], "dimension")
                    rgs <- list(dat, file = pltf, compression_level = png_compression_level, raw_spec = fastpng::raw_spec(width = tdim[1], height = tdim[2], depth = 1, bits = 8), use_filter = use_png_filter)
                } else {
                    ## image or rgb
                    nara <- inherits(td$img$data[[i]][[1]], "nativeRaster")
                    dat <- if (nara) td$img$data[[i]][[1]] else as.vector(matrix(unlist(td$img$data[[i]][[1]]), byrow = TRUE, nrow = 3))
                    tdim <- attr(td$img$data[[i]], "dimension")
                    rgs <- list(dat, file = pltf, compression_level = png_compression_level, raw_spec = fastpng::raw_spec(width = tdim[1], height = tdim[2], depth = 3, bits = 8), use_filter = use_png_filter)
                }
                if ("extra_args" %in% names(layerdef) && !is.null(layerdef$extra_args[[1]])) rgs <- c(rgs, layerdef$extra_args[[1]])
                if (.debug > 1) temp <- proc.time()["elapsed"]
                pltf <- do.call(fastpng::write_png, rgs)
                if (.debug > 1) message("png generation time: ", round(proc.time()["elapsed"] - temp, 3), "s", if (!.png_in_memory) paste0(", png file size: ", round(file.size(pltf) / 1e6, 1), "MB"))
            } else {
                stop("not coded")
            }
            pltf
        }

        update_tiles_data <- function(t, z = 1) {
            cat("--> in update_tiles_data() for layer", z, "\n")
            isolate({
                ## cat("updating tiles data for z =", utils::capture.output(utils::str(z)), "\n")
                ## t here is image_def()
                xy_hash <- rlang::hash(list(t, layerdef()[[z]])) ##^^^ is xy_hash needed/used??
                saved_hash <- isolate(tiles_data())[[z]]$xy_hash
                cat("xy_hash is:", xy_hash, ", saved hash is:", saved_hash, "\n")
                if (identical(xy_hash, saved_hash)) {
                    cat("not updating tiles_data", z, "- xy_hash is unchanged\n")
                } else {
                    cat("updating tiles_data", z, "\n")
                    ## generate new IDs, one per tile
                    ids <- uuid::UUIDgenerate(n = nrow(t$xy_grid))
                    td <- isolate(tiles_data())
                    td[[z]]$img <- list(ids = ids, data = rep(list(NULL), nrow(t$xy_grid)), xy_hash = xy_hash, data_hash = "") ##IMSRC src = rep(NA_character_, nrow(t$xy_grid)),
                    tiles_data(td)
                    ld <- layerdef()[[z]]
                    for (i in seq_len(nrow(t$xy_grid))) mirai_fetch_tile(t, z, ids, i)
                }
            })
        }

        mirai_queue <- list()
        mirai_fetch_tile <- function(t, z, ids, i) {
            ## t here is image_def()
            xy <- ext_to_c_mu(extent = t$ext, tiles_per_side = t$tiles_per_side)
            twh <- tile_wh_mu(ext = t$ext, tiles_per_side = t$tiles_per_side)
            this_ext <- xywh_to_ext(x = xy$x[i], y = xy$y[i], w = twh[1], h = twh[2])
            cat("fetching data: ext ", this_ext, ", res ", t$res, "\n")
            ld <- layerdef()[[z]]
            rgs <- list(ext = this_ext, z = z, dsn = ld$dsn, res = t$res, type = ld$type, target_crs = target_crs, warp_opts = .warp_opts, resampling = .resampling_method)
            key <- rlang::hash(rgs)
            if (!is.null(cache) && cache$exists(key)) {
                result <- cache$get(key)
                result$i <- i
                result$id <- ids[i]
                if (.debug > 2) message("got cached data for layer ", result$z, " tile ", result$i, " (id ", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)))
                handle_tile_data(result)
            } else {
                ## if a fetch request is already happening with this key, don't re-issue a new fetch request
                if (key %in% sapply(mirai_queue, function(job) job$key)) {
                    ## cat("request with key", key, "is already in the queue, not re-queueing\n")
                } else {
                    rgs <- c(rgs, list(i = i, id = ids[i], key = key))
                    ## cat("fetching:\n"); cat(utils::str(rgs))
                    ##if (.debug > 1)
                    if (.debug > 2) temp <- proc.time()["elapsed"]
                    if (!"cloudymapr" %in% rownames(installed.packages())) {
                        mid <- mirai::mirai(do.call(fetch_a_tile, rgs), rgs = rgs, fetch_a_tile = fetch_a_tile)
                    } else {
                        mid <- mirai::mirai(do.call(cloudymapr:::fetch_a_tile, rgs), rgs = rgs) ## don't pass the function here, it's super slow, fetch_a_tile = fetch_a_tile)
                    }
                    if (.debug > 2) {
                        temp <- proc.time()["elapsed"] - temp
                        message("mirai dispatch time: ", round(temp, 3), "s")
                    }
                    if (!is.null(mid)) mirai_queue[[length(mirai_queue) + 1]] <<- list(mid = mid, key = key)
                }
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
                        if (.debug > 2) message("got async data type", result$type, "for layer", result$z, "tile", result$i, "(id", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)))
                        if (!mirai::is_mirai_error(result)) {
                            if (!is.null(cache)) cache$set(result$key, result[setdiff(names(result), c("i", "id", "key"))]) ## cache it
                            handle_tile_data(result)
                        } else {
                            cat("async data failed: ", result, "\n")
                        }
                    }
                }
            }
            if (length(done) > 0) mirai_queue <<- mirai_queue[-done]
            shiny::invalidateLater(100)
        })
##        ^^^
        observe({
            req(image_def())
            if (init_done) {
cat("--> in vector layer plotter\n")
            ## deal with non-raster layers for which the user has provided a plot function
for (i in seq_along(layerdef())) do_vector_plot(i, image_def = image_def())
            } else {
                invalidateLater(200)
            }
        }, priority = 9)

        vector_plot_sources <- rep(NA_character_, 9)
        do_vector_plot <- function(i, image_def, iext) {
            ld <- isolate(layerdef())
            if (missing(iext)) iext <- image_def$ext
            if (!is.null(ld[[i]]) && "fun" %in% names(ld[[i]])) {
                z <- ld[[i]]$z
                pltf <- tempfile(tmpdir = tmpd, fileext = if (.use_ugd) ".svg" else ".png")
                cat("rendering plot", i, "layer", z, "as", if (.use_ugd && .svg_as_file) paste("svg file", pltf) else if (.use_ugd) "svg" else paste("png", pltf), "\n")
                ##tictoc::tic()
                ##IMSRC need to keep track of pltf for each non-raster source as well
                if (!.use_ugd) {
                    png(pltf, height = image_wh, width = image_wh, res = .plotres, bg = NA)
                } else {
                    unigd::ugd(width = image_wh, height = image_wh, bg = "transparent")
                }
                opar <- par(no.readonly = TRUE)
                par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") ## zero margins
                ok <- ld[[i]]$fun(xlim = iext[1:2], ylim = iext[3:4], zoom = image_def$zoom)
                if (ok && .use_ugd) {
                    if (.svg_as_file) {
                        unigd::ugd_save(file = pltf)
                    } else {
                        pltf <- unigd::ugd_render(as = "svg")
                    }
                }
                par(opar)
                dev.off() ## for both ugd and png devices
                ##tictoc::toc()
                if (!ok) {
                    clear_plot(z)
                } else if (.use_ugd && !.svg_as_file) {
                    send_plot(pltf, z, image_def = image_def, as = "svg")
                } else {
                    vector_plot_sources[z] <<- pltf
                    send_plot(pltf, z, image_def = image_def)
                }
            }
        }

        observe({
            req(layerdef(), image_def())
            if (init_done) {
                if (.debug > 1) cat("triggering generic update_tiles_data() because image_def() or layerdef() has changed\n")
                for (z in which_are_raster_layers()) update_tiles_data(image_def(), z)
            } else {
                invalidateLater(200)
            }
        })

        mapclick <- reactiveVal(NULL)
        observeEvent(input$mapclick, {
            req(input$mapclick)
            ## cat("mapclick: ", str(input$mapclick), "\n")
            mapclick(input$mapclick)
        })

        list(click = mapclick, layer_data = layer_data)
    })
}
