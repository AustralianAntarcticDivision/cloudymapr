## render data to png, but with caching
tile_to_png <- function(..., cache, res, use_fastpng, png_compression_level, use_png_filter, png_in_memory, debug = 0, tmpd) {
    keydata <- list(...)
    if (!nzchar(names(keydata)[1])) names(keydata)[1] <- "td"
    keydata$td <- if (keydata$i > length(keydata$td$img$data)) NULL else keydata$td$img$data[[keydata$i]] ## don't use other bits of td for cache key calculation
    keydata$i <- NULL ## so that if we ask for a tile in slot N that was previously in slot M, the cached copy can be used
    ## for now, no caching of pngs because changes in zlim are not always being correctly handled
    ## key <- rlang::hash(keydata)
    ## if (!is.null(cache) && cache$exists(key)) {
    ##     if (file.exists(cache$get(key))) {
    ##         if (debug > 0) cat("got cached png with key: ", key, "\n", sep = "")
    ##         return(cache$get(key))
    ##     } else {
    ##         cache$remove(key)
    ##     }
    ## }
    ## if (debug > 0) cat("no cached png available\n")
    pltf <- tile_to_png_inner(..., res = res, use_fastpng = use_fastpng, png_compression_level = png_compression_level, use_png_filter = use_png_filter, png_in_memory = png_in_memory, debug = debug, tmpd = tmpd)
    ## if (!is.null(cache) && !is.null(pltf)) cache$set(key, pltf) ## cache it
    pltf
}

tile_to_png_inner <- function(td, i, layerdef, res, use_fastpng, png_compression_level, use_png_filter, png_in_memory, debug = 0, tmpd) {
    if (i > length(td$img$data) || is.null(td$img$data[[i]][[1]])) return(NULL)
    pltf <- if (use_fastpng && png_in_memory) NULL else tempfile(tmpdir = tmpd, fileext = ".png")
    if (debug > 0) cat("rendering raster tile to png:", pltf, "\n")
    if (use_fastpng) {
        if (layerdef$type == "raster_data") {
            cmap <- layerdef$cmap[[1]]
            zl <- if (is.null(layerdef$zlims[[1]]) && !is.function(cmap)) stop("need z limits") else layerdef$zlims[[1]] ## NULL zlims ok if function?
            if (debug > 0) cat("  zlim is:", zl[1], " to ", zl[2], "\n", sep = "")
            ## construct matrix
            if (!is.null(attr(td$img$data[[i]], "gis"))) {
                ## gdalraster format
                tdim <- attr(td$img$data[[i]], "gis")$dim
                m <- t(matrix(td$img$data[[i]], nrow = tdim[1]))
            } else {
                tdim <- attr(td$img$data[[i]], "dimension")
                m <- matrix(td$img$data[[i]][[1]], nrow = tdim[2], byrow = TRUE)
            }
            if (is.function(cmap)) {
                temp <- cmap(m, zlim = zl) ## returns a list with palette indices and palette
                cmap <- temp[[2]]
                m <- matrix(temp[[1]] - 1L, nrow = nrow(m)) ## zero-based
            } else {
                ## scale by given zlim and then map to colour range, using zero-based indexing
                m <- pmax(pmin(round((m - zl[1]) / abs(diff(zl)) * (length(cmap) - 1L)) + 1L, length(cmap)), 1L) - 1L
            }
            ## any NA/NaN's will still be present, make them transparent
            cmap <- c(cmap, "#FFFFFF00") ## transparent
            m[is.na(m)] <- length(cmap) - 1L
            rgs <- list(m, palette = cmap, file = pltf, compression_level = png_compression_level, use_filter = use_png_filter)
        } else if (layerdef$type == "raster_image_grey") {
            ## greyscale image
            if (!is.null(attr(td$img$data[[i]], "gis"))) {
                ## gdalraster format
                tdim <- attr(td$img$data[[i]], "gis")$dim
                dat <- t(matrix(td$img$data[[i]], nrow = tdim[1]))
            } else {
                tdim <- attr(td$img$data[[i]], "dimension")
                dat <- as.vector(td$img$data[[i]][[1]])
            }
            rgs <- list(dat, file = pltf, compression_level = png_compression_level, raw_spec = fastpng::raw_spec(width = tdim[1], height = tdim[2], depth = 1, bits = 8), use_filter = use_png_filter)
        } else {
            ## image or rgb
            nara <- inherits(td$img$data[[i]][[1]], "nativeRaster")
            if (nara) {
                ## native raster via vapour
                tdim <- attr(td$img$data[[i]], "dimension")
                dat <- td$img$data[[i]][[1]]
            } else if (!is.null(attr(td$img$data[[i]], "gis"))) {
                ## gdalraster rgb format
                tdim <- attr(td$img$data[[i]], "gis")$dim
                dat <- aperm(array(td$img$data[[i]], dim = c(tdim[1:2], 3)), c(2, 1, 3))
            } else {
                ## rgb raster via vapour
                tdim <- attr(td$img$data[[i]], "dimension")
                dat <- as.vector(matrix(unlist(td$img$data[[i]][[1]]), byrow = TRUE, nrow = 3))
            }
            rgs <- list(dat, file = pltf, compression_level = png_compression_level, raw_spec = fastpng::raw_spec(width = tdim[1], height = tdim[2], depth = 3, bits = 8), use_filter = use_png_filter)
        }
        if ("extra_args" %in% names(layerdef) && !is.null(layerdef$extra_args[[1]])) rgs <- c(rgs, layerdef$extra_args[[1]])
        ## temp <- proc.time()["elapsed"]
        pltf <- do.call(fastpng::write_png, rgs)
        ## cat("png generation time: ", round(proc.time()["elapsed"] - temp, 3), "s", if (!png_in_memory) paste0(", png file size: ", round(file.size(pltf) / 1e6, 1), "MB"), "\n", sep = "")
    } else {
        stop("not coded")
    }
    pltf
}
