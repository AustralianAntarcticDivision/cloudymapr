## TODO allow centre to be specifed as something else?
## wrap in long-lat mode? otherwise deal with the large non-visible part of the canvas?
## TODO need to remove IDs of tile fetch jobs if they become superseded, e.g. user zooms twice in quick succession

image_def_from_view <- function(initial_view, zoom = 1) {
    ## xy centres of tiles in normalized [-1 1 -1 1] coords
    temp_xygrid <- expand.grid(x = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side,
                               y = seq(-1, 1, length.out = initial_view$tiles_per_side + 1)[-1] - 1 / initial_view$tiles_per_side)
    list(tiles_per_side = initial_view$tiles_per_side,
         n_tiles = initial_view$tiles_per_side ^ 2,
         xy_grid = temp_xygrid, ## xy centres of tiles in normalized [-1 1 -1 1] coords, these do not change (only dependent on n tiles per side)
         ext = initial_view$extent, ## the extent value is maintained on the client side, and sent here to the server when it changes
         res = initial_view$res, zoom = zoom)
}

is_raster_layer <- function(i, lyrdef) { !is.null(lyrdef[[i]]) && "dsn" %in% names(lyrdef[[i]]) }

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

xywh_to_ext <- function(x, y, w, h) unname(c(x + c(-1, 1) * w / 2, y + c(-1, 1) * h / 2))

#' Vapourlayer map shiny module
#'
#' @param id string: HTML element id
#' @param view_wh numeric: two-element vector giving the width and height of the map widget as percentages of the browser window width and height in valid css units (e.g. `view_wh = c("400px", "250px")` or `view_wh = c("60vw", "40vh")`
#' @param image_wh integer: image size in pixels. Depending on the rendering mechanism, the native image size might not actually be this, but the browser will scale to this size
# @param min_scale scalar: the smallest cell size (in projected units), used when maximally zoomed in
## no, min_scale should be determined by the data. Or maybe allow min_scale to be specified but by default it's NA, in which case it falls back to the native data res
#' @param initial_view list: a named list with components:
#' * tiles_per_side integer: e.g. a value of 2 means that we will generate the image as 2x2 set of tiles. If > 1 must be even, and the x- and y-extents must be even multiples of the number of tiles per side
#' * extent numeric: the initial image extent c(xmin, xmax, ymin, ymax) in projected coordinates
#' * res numeric: the resolution (in m) to use for the image when shown at its intial extent. The resolution will change as the map is zoomed in/out
#' * max_extent numeric: as for `extent`, but defining the limits that will be shown. The map will not be extended beyond these bounds as it is zoomed/panned. If not provided, the initial extent will be used
#' @param layerdef reactive: TBD
#' @param target_crs string or reactiveVal: target projection CRS string
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
                  singleton(tags$script(type = "text/javascript", src = "https://cdn.jsdelivr.net/npm/gdal3.js@2.8.1/dist/package/gdal3.js",
                                        integrity = "sha384-yW4c2Jx7lsREjJg58+ZI5U6gAso2bRAPw3LdzPWm7z8+rMJ24R7AS+EFyXDPxgYM", crossorigin = "anonymous")),
                  singleton(tags$script(HTML(paste(readLines(system.file("extdata/js/vapourlayer-common.js", package = "cloudymapr"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")))),
                  tags$script(HTML(paste(gsub("$ID$", id, readLines(system.file("extdata/js/vapourlayer.js", package = "cloudymapr"), warn = FALSE, encoding = "UTF-8"), fixed = TRUE), collapse = "\n")))
                  ),
        tags$div(style = "position:relative; margin-top:1px;",
                 tags$div(id = NS(id, "plot_controls"), class = "vl-plot-controls",
                          tags$button(id = NS(id, "zoom_in"), class = "btn btn-default", icon("magnifying-glass-plus", id = NS(id, "zoom-in-icon")), title = "Zoom in"),
                          tags$button(id = NS(id, "zoom_out"), class = "btn btn-default", icon("magnifying-glass-minus", id = NS(id, "zoom-out-icon")), title = "Zoom out"),
                          actionButton(NS(id, "pan_button"), class = "btn btn-default", label = icon("hand", id = NS(id, "pan-icon")), title = "Click and drag will pan the map", onclick = paste0("$('#", NS(id, "pan-icon"), "').removeClass('icon-disabled'); $('#", NS(id, "select-icon"), "').addClass('icon-disabled'); cm_", id, ".select_mode='pan';")),
                          actionButton(NS(id, "select_button"), class = "btn btn-default", label = icon("object-group", id = NS(id, "select-icon"), class = "icon-disabled"), title = "Click and drag will zoom to the selected region", onclick = paste0("$('#", NS(id, "select-icon"), "').removeClass('icon-disabled'); $('#", NS(id, "pan-icon"), "').addClass('icon-disabled'); cm_", id, ".select_mode='select';"))
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

    ## initial arg checking
    iv <- initial_view
    if (is.null(iv$tiles_per_side)) iv$tiles_per_side <- 1L
    if (is.null(iv$extent) || is.null(iv$res)) stop("initial_view must contain `extent` and `res` components")
    if (is.null(iv$max_extent)) iv$max_extent <- iv$extent
    ## extent overrides max_extent if there are inconsistencies
    iv$max_extent <- c(min(iv$extent[1], iv$max_extent[1]), max(iv$extent[2], iv$max_extent[2]),
                       min(iv$extent[3], iv$max_extent[3]), max(iv$extent[4], iv$max_extent[4]))
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
        if (.debug > 0) cat("plots temporary directory is:", tmpd, "\n")
        onSessionEnded(function() unlink(tmpd, recursive = TRUE))
        addResourcePath("plots", tmpd)

        ## funs
        which_are_raster_layers <- function() {
            isolate(which(vapply(seq_along(layerdef()), is_raster_layer, lyrdef = layerdef(), FUN.VALUE = TRUE)))
        }
        which_are_vector_layers <- function() {
            isolate(which(!vapply(seq_along(layerdef()), is_raster_layer, lyrdef = layerdef(), FUN.VALUE = TRUE)))
        }

        ## make target crs reactive if needed
        if (!is.reactive(target_crs)) {
            if (!(is.character(target_crs) && length(target_crs) == 1 && !is.na(target_crs) && nzchar(target_crs))) {
                stop("target_crs should be a non-empty/non-NA string")
            }
            target_crs <- reactiveVal(target_crs)
        }
        ## check that it's valid
        ok <- tryCatch(sf::st_crs(isolate(target_crs())), error = function(e) {
            if (grepl("invalid crs", conditionMessage(e), ignore.case = TRUE)) {
                warning("target_crs '", isolate(target_crs()), "' might be invalid")
            }
        })

        ## generate our image_def from the user-supplied initial_view
        ## initial_view is list(tiles_per_side, extent(xmin, xmax, ymin, ymax), res)

        ## tile width and height in map coords
        tile_w <- diff(initial_view$extent[1:2]) / initial_view$tiles_per_side
        tile_h <- diff(initial_view$extent[3:4]) / initial_view$tiles_per_side

        ## we need the view to be defined such that the xy extents are multiples of the number of tiles per side
        if (initial_view$tiles_per_side <= 0 || (initial_view$tiles_per_side / floor(initial_view$tiles_per_side) != 1)) stop("initial_view$tiles_per_side must be an even positive integer")
        if (initial_view$tiles_per_side > 1) {
            if (initial_view$tiles_per_side %% 2 != 0) stop("initial_view$tiles_per_side must be 1 or an even positive integer")
            chk <- c(tile_w, tile_h)
            if (any(abs(chk - round(chk)) > 0)) stop("initial_view must define the xy extents to be even multiples of the number of tiles per side")
        }

        image_def <- reactiveVal(image_def_from_view(initial_view, zoom = 1))
        tile_wh <- round(image_wh / initial_view$tiles_per_side) ## in pixels

        ## set initial scaling parms for conversion of map units to pixels
        init_done <- FALSE
        observeEvent(input$request_init, {
            extstr <- paste0("[", initial_view$extent[1], ",", initial_view$extent[2], ",", initial_view$extent[3], ",", initial_view$extent[4], "];")
            maxextstr <- paste0("[", initial_view$max_extent[1], ",", initial_view$max_extent[2], ",", initial_view$max_extent[3], ",", initial_view$max_extent[4], "];")
            ctrstr <- paste0("[", (initial_view$extent[1] + initial_view$extent[2]) / 2, ",", (initial_view$extent[3] + initial_view$extent[4]) / 2, "];")
            alstr <- paste0("[", paste(sapply(layerdef(), function(w) w$z), collapse = ","), "];") ## active layers, 1-indexed
            evaljs("cm_", id, ".xsc=", diff(initial_view$extent[1:2]) / image_wh, ";",
                   "cm_", id, ".ysc=", diff(initial_view$extent[3:4]) / image_wh, ";",
                   "cm_", id, ".xoff=0;cm_", id, ".yoff=0;",
                   "cm_", id, ".imxoff=", image_wh / 2, ";cm_", id, ".imyoff=", image_wh / 2, ";",
                   "cm_", id, ".crs='", target_crs(), "';",
                   "cm_", id, ".image_wh=", image_wh, ";",
                   "cm_", id, ".res=", initial_view$res, ";",
                   "cm_", id, ".viewport_ctr=", ctrstr,
                   "cm_", id, ".active_layers=", alstr,
                   "cm_", id, ".select_mode='pan';",
                   "cm_", id, ".ext=", extstr, "cm_", id, ".ext0=", maxextstr
                   )
            init_done <<- TRUE
        })

        ## initial setup
        ## this doesn't need to be reactive, it only relies on constants (image_wh, id) and the dom being initialized, which it will be by the time this is called
        ## set the canvas sizes and store array of contexts
        evaljs(paste0("$('#", id, "-pannable').width('", image_wh, "px').height('", image_wh, "px'); ",
                      id, "_ctxlist = []; for (let i = 1; i <= 9; i++) { var this_ctx = document.getElementById('", id, "-plot' + i).getContext('2d', { willReadFrequently: true }); this_ctx.canvas.height = ", image_wh, "; this_ctx.canvas.width = ", image_wh, "; ", id, "_ctxlist[i] = this_ctx; }"))

        observeEvent(input$pan_button, {
            js_add_class(paste0(id, "-select-icon"), "icon-disabled")
            js_remove_class(paste0(id, "-pan-icon"), "icon-disabled")
            evaljs("cm_", id, ".select_mode = 'pan';")
        })

        observeEvent(input$select_button, {
            js_remove_class(paste0(id, "-select-icon"), "icon-disabled")
            js_add_class(paste0(id, "-pan-icon"), "icon-disabled")
            evaljs("cm_", id, ".select_mode = 'select';")
        })

        observeEvent(input$do_zoom, {
            if (.debug > 0) cat("do_zoom: ", input$do_zoom, "\n")
            if (.clear_on_zoom) clear_tiles_data()
            i <- image_def()
            i$ext <- input$do_zoom[1:4]
            i$res <- input$do_zoom[5]
            i$zoom <- input$do_zoom[6]
            image_def(i)
        })

        observeEvent(input$pan_extend, {
            ## update the image_def()$ext to the new value just sent through
            if (.debug > 0) cat("--> extending tiles\n")
            if (.clear_on_pan) clear_tiles_data()
            i <- image_def()
            i$ext <- input$pan_extend
            image_def(i) ## update the image reactive
        })

        send_plot <- function(plot_contents, plotnum, image_def, ext_mu, clear = .clear_canvas_before_drawing, as = "file") {
            ## send_plot is used for vector layers, not tiled layers (see draw_tile for those)
            if (.debug > 0) cat("sendplot: plot", plotnum, "updated, sending to js ")
            plotid <- paste0(id, "-plot", plotnum)
            if (missing(ext_mu)) ext_mu <- image_def$ext
            ## idef <- isolate(image_def())
            if (as == "svg" && !.svg_as_file) {
                if (.debug > 0) cat("as svg\n")
                plot_contents <- paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)))
            } else {
                ## plot_contents is a file or raw vector
                if (is.raw(plot_contents)) {
                    if (.debug > 0) cat("as b64 png from raw\n")
                    plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                } else {
                    if (.debug > 0) cat("as image from file\n")
                    plot_contents <- paste0("plots/", basename(plot_contents))
                }
            }
            image_js(z = plotnum, ext_mu = ext_mu, plot_contents = plot_contents, image_def = image_def, clear = clear)
        }

        clear_plot <- function(plotnum) {
            ## can be multiple plotnums
            if (.debug > 0) cat("clearing plot", plotnum, "\n")
            evaljs(paste0("var this_ctx = ", id, "_ctxlist[", plotnum, "]; if (this_ctx) { this_ctx.clearRect(0, 0, ", image_wh, ", ", image_wh, "); }"))
            vector_plot_sources[plotnum] <<- NA_character_
        }

        tiles_data <- do.call(reactiveValues, setNames(rep(list(list(img = NULL)), 9), as.character(1:9))) ## the tiles for each layer, as will be fetched with gdal
        layer_data <- lapply(1:9, function(z) {
            reactive({
                if (z > length(layerdef())) {
                    NULL
                } else if (isTRUE(layerdef()[[z]]$type %in% c("raster_data", "raster_image_grey"))) {
                    if (is.null(tiles_data[[as.character(z)]]$img$data)) {
                        NULL
                    } else {
                        tryCatch(terra::rast(tiles_to_matrix(tiles_data[[as.character(z)]]$img$data), crs = target_crs(), extent = image_def()$ext), error = function(e) NULL)
                    }
                } else {
                    ## TODO other image types. RGB should be able to return RGB colours as a 3-band raster
                    NULL
                }
            })
        })

        clear_tiles_data <- function(z) {
            if (missing(z)) z <- which_are_raster_layers()
            for (i in z) tiles_data[[as.character(i)]] <- list(img = NULL)
            clear_plot(z)
        }

        tiles_to_matrix <- function(td_img_data) {
            gdr_format <- FALSE ## gdalraster format?
            tdim <- Filter(Negate(is.null), lapply(td_img_data, attr, "dimension"))
            if (length(tdim) < 1) {
                tdim <- Filter(Negate(is.null), lapply(td_img_data, function(tld) attr(tld, "gis")$dim))
                gdr_format <- TRUE
            }
            tdim <- if (length(tdim) > 0) tdim[[1]] else return(NULL) ## if we have no dimension, we can't build the matrix
            if (gdr_format) {
                m1 <- function(z) t(matrix(if (is.null(z)) NA_real_ else if (is.raw(z)) as.integer(z) else z, nrow = tdim[1])) ## matrix constructor
            } else {
                m1 <- function(z) matrix(if (is.null(z)) NA_real_ else if (is.raw(z)) as.integer(z) else z, nrow = tdim[2], ncol = tdim[1], byrow = TRUE) ## matrix constructor
            }
            idef <- isolate(image_def())
            n <- idef$tiles_per_side ## image is composed of a square arrangement of tiles (which may be only one tile)
            ord <- order(idef$xy_grid$x, -idef$xy_grid$y) ## tile ordering
            if (.debug > 1) temp <- proc.time()["elapsed"]
            out <- do.call(cbind, lapply(seq_len(n), function(ci) {
                do.call(rbind, lapply((ci - 1) * n + seq_len(n), function(ri) {
                    m1(if (gdr_format) td_img_data[[ord[ri]]] else td_img_data[[ord[ri]]][[1]])
                }))
            }))
            if (.debug > 1) {
                temp <- proc.time()["elapsed"] - temp
                cat("data matrix assembly time:", round(temp, 3), "s")
            }
            out
        }

        handle_tile_data <- function(result) {
            try({
                td2 <- isolate(tiles_data[[as.character(result$z)]]) ## tiles for layer z
                if (result$id %in% td2$img$ids) {
                    i <- which(result$id == td2$img$ids)
                    td2$img$data[[i]] <- result$data
                    td2$img$data_hash <- rlang::hash(td2$img$data) ## TODO does it help to include the zlim hash here?
                    tiles_data[[as.character(result$z)]] <- td2
                }
            })
        }

        ## trigger the drawing of a layer's tiles when its tiles_data changes and/or its layerdef changes (e.g. zlim)
        ## TODO make layerdef reactive to each layer separately so that every layer isn't redrawn when one part of layerdef changes
        for (tii in 1:9) {
            local({
                ti <- force(tii)
                observeEvent(list(tiles_data[[as.character(ti)]], layerdef()), {
                    for (i in seq_along(tiles_data[[as.character(ti)]]$img$data)) isolate(draw_tile(z = 1L, td = tiles_data[[as.character(ti)]], i = i, image_def = image_def(), layerdef = layerdef()))
                })
            })
        }

        draw_tile <- function(z, td, i, image_def, layerdef, clear = TRUE, as = "file") {
            if (.debug > 0) cat("draw_tile for layer ", z, ", index ", i, "\n", sep = "")
            ## z is the layer index (e.g. into tiles_data
            ## td is that tiles_data entry
            ## i is the index within the layer (> 1 if we have multiple tiles per side)
            ## send tile to canvas
            plot_contents <- tile_to_png(td = td, i = i, layerdef = layerdef[[z]], cache = cache, res = .plotres, use_fastpng = .use_fastpng, png_compression_level = .png_compression_level, use_png_filter = .use_png_filter, png_in_memory = .png_in_memory, debug = .debug, tmpd = tmpd)
            if (!is.null(plot_contents)) {
                tile_ext_mu <- attr(td$img$data[[i]], "extent")
                if (.debug > 0) cat("sending tile to js ")
                plotid <- paste0(id, "-plot", z)
                ## send the extent in map units to the client and let it work out the tl/br coords in the canvas
                if (as == "svg" && !.svg_as_file) {
                    if (.debug > 0) cat("as svg\n")
                    ## plot contents is an svg string
                    plot_contents <- paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(plot_contents)))
                } else {
                    ## plot_contents is a file or raw vector
                    if (is.raw(plot_contents)) {
                        if (.debug > 0) cat("as b64 png from raw\n")
                        plot_contents <- paste0("data:image/png;base64,", base64enc::base64encode(plot_contents))
                    } else {
                        if (.debug > 0) cat("as image from file\n")
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

        update_tiles_data <- function(imgdef, z = 1) {
            if (.debug > 0) cat("--> in update_tiles_data() for layer", z, "\n")
            isolate({
                xy_hash <- rlang::hash(list(imgdef, layerdef()[[z]])) ##^^^ is xy_hash needed/used??
                saved_hash <- isolate(tiles_data[[as.character(z)]])$xy_hash
                if (.debug > 1) cat("xy_hash is:", xy_hash, ", saved hash is: ", saved_hash, "\n", sep = "")
                if (identical(xy_hash, saved_hash)) {
                    if (.debug > 1) cat("not updating tiles_data", z, "- xy_hash is unchanged\n")
                } else {
                    if (.debug > 1) cat("updating tiles_data", z, "\n")
                    ## generate new IDs, one per tile
                    ids <- uuid::UUIDgenerate(n = nrow(imgdef$xy_grid))
                    td <- isolate(tiles_data[[as.character(z)]])
                    td$img <- list(ids = ids, data = rep(list(NULL), nrow(imgdef$xy_grid)), xy_hash = xy_hash, data_hash = "")
                    tiles_data[[as.character(z)]] <- td
                    ld <- layerdef()[[z]]
                    for (i in seq_len(nrow(imgdef$xy_grid))) outer_fetch_tile(imgdef, z, ids, i, use_mirai = .parallel_gdal_req)
                }
            })
        }

        outer_fetch_tile <- function(imgdef, z, ids, i, use_mirai = TRUE) {
            xy <- ext_to_c_mu(extent = imgdef$ext, tiles_per_side = imgdef$tiles_per_side)
            twh <- tile_wh_mu(ext = imgdef$ext, tiles_per_side = imgdef$tiles_per_side)
            this_ext <- xywh_to_ext(x = xy$x[i], y = xy$y[i], w = twh[1], h = twh[2])
            if (.debug > 1) cat("fetching data: ext ", this_ext, ", res ", imgdef$res, "\n", sep = "")
            ld <- layerdef()[[z]]
            rgs <- list(ext = this_ext, dsn = ld$dsn, res = imgdef$res, type = ld$type, target_crs = target_crs(), warp_opts = .warp_opts, resampling = .resampling_method)
            key <- key_from_args(rgs)
            if (!is.null(cache) && cache$exists(key)) {
                result <- cache$get(key)
                result$i <- i
                result$id <- ids[i]
                result$z <- z
                if (.debug > 2) cat("got cached data for layer ", z, " tile ", result$i, " (id ", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)), "\n", sep = "")
                handle_tile_data(result)
            } else {
                if (.debug > 2) cat("key ", key, " not in cache\n", sep = "")
                if (use_mirai) {
                    rgs <- c(rgs, list(z = z, i = i, id = ids[i], key = key))
                    mirai_fetch_tile(rgs, key)
                } else {
                    ## do the fetch synchronously
                    result <- do.call(fetch_a_tile, c(rgs, list(z = z)))
                    result$i <- i
                    result$id <- ids[i]
                    result$z <- z
                    handle_tile_data(result)
                }
            }
        }

        mirai_queue <- list()
        mirai_fetch_tile <- function(rgs, key) {
            ## if a fetch request is already happening with this key, don't re-issue a new fetch request
            if (!key %in% sapply(mirai_queue, function(job) job$key)) {
                ## cat("fetching:\n"); cat(utils::str(rgs))
                ##if (.debug > 1)
                if (.debug > 2) temp <- proc.time()["elapsed"]
                if (!"cloudymapr" %in% rownames(utils::installed.packages())) {
                    ## local dev without package being installed
                    mid <- mirai::mirai(do.call(fetch_a_tile, rgs), rgs = rgs, fetch_a_tile = fetch_a_tile)
                } else {
                    mid <- mirai::mirai(do.call(cloudymapr:::fetch_a_tile, rgs), rgs = rgs) ## don't pass the function here, it's super slow, fetch_a_tile = fetch_a_tile)
                }
                if (.debug > 2) {
                    temp <- proc.time()["elapsed"] - temp
                    cat("mirai dispatch time: ", round(temp, 3), "s\n", sep = "")
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
                    done <- c(done, ji)
                } else {
                    if (!mirai::unresolved(job)) {
                        result <- job$data
                        done <- c(done, ji)
                        result_was_ok <- !mirai::is_mirai_error(result) && !is.null(result) && tryCatch({ result$key; TRUE }, error = function(e) FALSE) ## sometimes we just get an error string back
                        if (result_was_ok) {
                            if (.debug > 2) cat("got async data type", result$type, "for layer", result$z, "tile", result$i, "(id", result$id, ")", utils::capture.output(utils::str(result$data, max.level = 1)), "\n", sep = "")
                            if (!is.null(cache)) cache$set(result$key, result[setdiff(names(result), c("z", "i", "id", "key"))]) ## cache it
                            handle_tile_data(result)
                        } else {
                            if (.debug > 0) cat("async data failed: ", result, "\n")
                        }
                    }
                }
            }
            if (length(done) > 0) mirai_queue <<- mirai_queue[-done]
            shiny::invalidateLater(100)
        })

        observe({
            req(image_def(), target_crs())
            if (init_done) {
                if (.debug > 0) cat("--> in vector layer plotter\n")
            ## deal with non-raster layers for which the user has provided a plot function
                for (i in seq_along(layerdef())) do_vector_plot(i, image_def = image_def(), crs = target_crs())
            } else {
                invalidateLater(200)
            }
        }, priority = 9)

        vector_plot_sources <- rep(NA_character_, 9)
        do_vector_plot <- function(i, image_def, iext, crs) {
            ld <- isolate(layerdef())
            if (missing(iext)) iext <- image_def$ext
            if (!is.null(ld[[i]]) && "fun" %in% names(ld[[i]])) {
                z <- ld[[i]]$z
                pltf <- tempfile(tmpdir = tmpd, fileext = if (.use_ugd) ".svg" else ".png")
                if (.debug > 0) cat("rendering plot", i, "layer", z, "as", if (.use_ugd && .svg_as_file) paste("svg file", pltf) else if (.use_ugd) "svg" else paste("png", pltf), "\n")
                ##tictoc::tic()
                if (!.use_ugd) {
                    png(pltf, height = image_wh, width = image_wh, res = .plotres, bg = NA)
                } else {
                    unigd::ugd(width = image_wh, height = image_wh, bg = "transparent")
                }
                opar <- par(no.readonly = TRUE)
                par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") ## zero margins
                ok <- ld[[i]]$fun(xlim = iext[1:2], ylim = iext[3:4], zoom = image_def$zoom, crs = crs)
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
            req(layerdef(), image_def(), target_crs())
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
            ## cat("mapclick: ", utils::str(input$mapclick), "\n")
            mapclick(input$mapclick)
        })

        observeEvent(input$reproj, {
            if (isTRUE(input$reproj$crs != target_crs())) {
                if (.debug > 0) cat("reproj: ", utils::str(input$reproj), "\n")
                i <- image_def()
                i$ext <- unlist(input$reproj$extent)
                image_def(i) ## update the image reactive
                target_crs(input$reproj$crs)
            }
        })

        set_crs <- function(crs) evaljs(paste0("cm_", id, ".reproj('", crs, "');")) ## reprojection happens client-side first, then triggers the input$reproj block above
        get_crs <- reactive(target_crs())

        list(click = mapclick, layer_data = layer_data, set_crs = set_crs, crs = get_crs)
    })
}
