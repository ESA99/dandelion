#' Title
#'
#' @param raster_dir Character. Filepath to directory with the rasters to be clipped. Expects name to contain S2-Tilename.
#' @param s2_dir  Character. Filepath to directorywith the S2-scenes (just one jp200 or tif, not whole folder.) Must match the number of inout rasters and be named similar, as they are processed by order.
#' @param out_dir Character. Output directory for the final files.
#' @param out_basename Character. Basename to which the Sentinel tile name is appended.
#' @param filetype Character. Default "tif".
#'
#' @returns Rasters as tif cropped to extend of S2-images.
#' @export

crop_raster_with_s2 <- function(raster_dir, s2_dir, out_dir, out_basename, filetype = "tif"){

  rasters <- list.files(raster_dir, full.names = T)
  s2_tile <- list.files(s2_dir, full.names = T)

  if(length(rasters) != length(s2_tile) ){ stop("Number of raster inputs do not match!\n") }

  cat("Processing", length(rasters), "Raster files.\n")

  for (i in 1:length(rasters)) {

    cat("===== Raster",i,"=====")
    wc_rast <- terra::rast(rasters[i])
    s2_rast <- terra::rast(s2_tile[i])

    colour_table <- terra::coltab(wc_rast)
    cat("Colourtable extracted.\n")

    wc_trans <- terra::project(wc_rast, terra::crs(s2_rast))
    cat("CRS adjusted. Converting S2 scene to polygon...\n")

    s2_vect <- terra::as.polygons(s2_rast, dissolve = TRUE)
    cat("S2 raster converted to polygon. Cropping...\n")
    wc_crop <- terra::crop(wc_trans, s2_vect, mask = TRUE)
    # wc_crop <- terra::crop(wc_trans, s2_rast, mask = T)
    cat("Crop successfull. Applying colourtable and extracting tilename.\n")
    terra::coltab(wc_crop) <- colour_table

    tile_name <- tools::file_path_sans_ext(basename(rasters[i]))
    # tile_name <- sub("^WC_", "", tile_name)
    tile_name <- sub(".*?(\\d{2}[A-Za-z]{3}).*", "\\1", tile_name)

    # Save final worldcover raster
    out_file <- file.path(out_dir, paste0(out_basename, tile_name, ".", filetype))
    cat("Writing to:", out_file, "\n")
    terra::writeRaster(wc_crop, out_file, overwrite = TRUE)
    cat("Done with tile:", tile_name, "\n")


  }

  cat("\n ******* All rasters processed successfully. *******\n")

}
