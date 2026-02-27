key_from_args <- function(rgs, postproc) {
    rgs <- rgs[sort(setdiff(names(rgs), c("z", "i", "id", "key")))] ## never include z, i, etc, and sort by name for consistency
    ## enforce some types for consistency
    if ("res" %in% names(rgs)) rgs$res <- as.numeric(rgs$res)
    if ("ext" %in% names(rgs)) rgs$ext <- as.numeric(rgs$ext)
    if (!missing(postproc) && is.function(postproc)) rgs <- postproc(rgs) ## any fiddling to do before calculating the key
    rlang::hash(rgs)
}

## NOTE that zoom levels are c(1, 2, 4, 8, etc)
seed_cache <- function(layerdef, initial_view, crs, warp_opts = .warp_opts, resampling_method = .resampling_method, zoom_levels = 1, cache, key_args_postproc = NULL) {
    stopifnot(all(log2(zoom_levels) %in% 0:20))
    raster_layers <- which(vapply(seq_along(layerdef), is_raster_layer, lyrdef = layerdef, FUN.VALUE = TRUE))
    for (zoom in sort(zoom_levels)) {
        imgdef <- image_def_from_view(view_at_zoom_level(initial_view, zoom), zoom = zoom)
        for (z in raster_layers) {
            for (i in seq_len(nrow(imgdef$xy_grid))) cache_fetch_tile(imgdef, z = z, i = i, layerdef = layerdef, crs = crs, warp_opts = warp_opts, resampling_method = resampling_method, cache = cache, key_args_postproc = key_args_postproc) ## ids
        }
    }
}

view_at_zoom_level <- function(initial_view, zoom, zoom0 = 1) {
    stopifnot(zoom0 == 1) ## assume that initial_view corresponds to zoom level 1 TODO generalize
    list(tiles_per_side = initial_view$tiles_per_side * zoom, res = initial_view$res / zoom, extent = initial_view$extent) ## i.e. extent remains at the full extent, but with more/smaller tiles TODO cope with max extent
}

cache_fetch_tile <- function(imgdef, z, i, layerdef, crs, warp_opts, resampling_method, cache, key_args_postproc) {
    xy <- ext_to_c_mu(extent = imgdef$ext, tiles_per_side = imgdef$tiles_per_side)
    twh <- tile_wh_mu(ext = imgdef$ext, tiles_per_side = imgdef$tiles_per_side)
    this_ext <- xywh_to_ext(x = xy$x[i], y = xy$y[i], w = twh[1], h = twh[2])
    cat("fetching data: ext ", this_ext, ", res ", imgdef$res, "\n")
    rgs <- list(ext = this_ext, dsn = layerdef[[z]]$dsn, res = imgdef$res, type = layerdef[[z]]$type, target_crs = crs, warp_opts = warp_opts, resampling = resampling_method)
    key <- key_from_args(rgs, key_args_postproc)
    if (!cache$exists(key)) {
        ## cat("fetching:\n"); cat(utils::str(rgs))
        result <- do.call(fetch_a_tile, c(rgs, list(z = z)))
        cache$set(key, result[setdiff(names(result), c("z", "i", "id", "key"))]) ## cache it
    }
}
