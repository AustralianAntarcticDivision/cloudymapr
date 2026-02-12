## fetch_a_tile_vapour <- function(ext, dsn, res, type, target_crs, warp_opts, resampling, ...) {
##     if (missing(warp_opts) || length(warp_opts) < 1) warp_opts <- character()
##     tryCatch({
##         dt <- if (type == "raster_data") {
##                   vapour::gdal_raster_data(dsn, target_res = res, target_crs = target_crs, target_ext = ext, resample = resampling, options = warp_opts)
##               } else if (type == "raster_image_rgb") {
##                   vapour::gdal_raster_nara(dsn, bands = 1:3, target_res = res, target_crs = target_crs, target_ext = ext, band_output_type = "Byte", resample = resampling, options = warp_opts)
##               } else {
##                   ## raster_image_grey
##                   vapour::gdal_raster_data(dsn, bands = 1, target_res = res, target_crs = target_crs, target_ext = ext, band_output_type = "Byte", resample = resampling, options = warp_opts)
##               }
##         list(data = dt, type = type, ...)
##     }, error = function(e) list(data = NULL, type = type, err = conditionMessage(e), ...))
## }

fetch_a_tile_gdalraster <- function(ext, dsn, res, type, target_crs, warp_opts, resampling, ...) {
    ## https://gdalcubes.github.io/source/concepts/config.html#recommended-settings-for-cloud-access
    gdalraster::set_config_option("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
    gdalraster::set_config_option("VSI_CACHE", "TRUE")
    gdalraster::set_config_option("GDAL_CACHEMAX","30%")
    gdalraster::set_config_option("VSI_CACHE_SIZE","10000000")
    gdalraster::set_config_option("GDAL_HTTP_MULTIPLEX","YES")
    gdalraster::set_config_option("GDAL_INGESTED_BYTES_AT_OPEN","128000") ## was 32k
    gdalraster::set_config_option("GDAL_HTTP_VERSION","2")
    gdalraster::set_config_option("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
    gdalraster::set_config_option("GDAL_NUM_THREADS", "ALL_CPUS")
    ## note that CPL_DEBUG seems to cause segfaults
    ## if (debug > 1) gdalraster::set_config_option("CPL_DEBUG", "ON") ##else gdalraster::set_config_option("CPL_DEBUG", "OFF")
    if (length(res) == 1) res <- c(res, res)
    tryCatch({
        outfile <- tempfile(tmpdir = "/vsimem")
        ## or warp to a GDALRaster dst_ds <- create("MEM", "", 20, 20, 1, "Byte", return_obj = TRUE)
        gdalraster::warp(dsn, dst_filename = outfile, t_srs = target_crs, cl_arg = c("-te", ext[1], ext[3], ext[2], ext[4], "-tr", res[1], res[2], "-r", resampling, if (type == "raster_image_rgb") c("-ot", "Byte", "-srcband", 1, "-srcband", 2, "-srcband", 3), if (type == "raster_image_grey") c("-ot", "Byte", "-srcband", 1), warp_opts)) ##, "-multi", "-wo", "NUM_THREADS=ALL_CPUS"))
        ## -multi
        ## Use multithreaded warping implementation. Two threads will be used to process chunks of image and perform input/output operation simultaneously. Note that computation is not multithreaded itself. To do that, you can use the ⁠-wo NUM_THREADS=val/ALL_CPUS⁠ option, which can be combined with -multi.
        ds <- new(gdalraster::GDALRaster, outfile)
        dt <- gdalraster::read_ds(ds)
        attr(dt, "extent") <- ext
        list(data = dt, type = type, ...)
    }, error = function(e) list(data = NULL, type = type, err = conditionMessage(e), ...))
}

fetch_a_tile <- fetch_a_tile_gdalraster
##fetch_a_tile <- fetch_a_tile_vapour
