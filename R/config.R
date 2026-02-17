## default config options
.debug <- 1L
.clear_canvas_before_drawing <- TRUE ## for vector layers, do we clear the full canvas before replotting?
.use_ugd <- FALSE ## for vector layers, use unigd:ugd to generate svg. If FALSE use png format
.svg_as_file <- TRUE ## if FALSE, send the svg string directly to the browser as b64-encoded data: but think that using a file is better for caching behaviour
.use_fastpng <- TRUE ## if FALSE use png()
.png_in_memory <- FALSE
.png_compression_level <- 2L ## 0L is no compression, ~0.1s to write a 2500x2500 image (25MB file); 2L ~1.4s for the same image (9.5MB) or ~0.9s with .use_png_filter = FALSE (11MB file)
.use_png_filter <- .png_compression_level < 1 ## see fastpng::write_png
.plotres <- 96 ## dpi, only used by png graphics device
.parallel_gdal_req <- TRUE ## make gdalwarp requests in parallel (using mirai)?
.warp_opts <- c("-wm", "999")
.resampling_method <- "near" ## or "bilinear" ## TODO allow this to be specified on a source-by-source basis? Be aware that bilinear sampling can cause issues when the source is raster_data and it has special values like a land mask (255) but valid values are (say) 0-100: interpolation near land will give values > 100 and < 255
## the next two should not ever need to be TRUE any more
.clear_on_zoom <- FALSE ## clear plots on zoom? Or leave them visible while refreshing?
.clear_on_pan <- FALSE ## clear plots on pan? Or leave them visible while refreshing?
