#' Adjust worldcover image to the Structure of a Sentinel2 tile.
#'
#' @param wcover_tiles Character. File path to the worldcover image to be adjusted. CRS, extent and resolution gets modified if needed.
#' @param wc_tile_status Data frame with Tile names and status of the Worldcover image Tile (if it already was processed). Included for looping to not perform the same calculation again and again.
#'
#' @returns Creates a new tif that is adjusted to the Sentinel2 Format and overwrites the original input.
#' @export
#'
#' @example world_raster <- rast(nrows = 500, ncols = 500, xmin = 0, xmax = 500, ymin = 0, ymax = 500)
#'          wc_tile_status <- data.frame(tile_name = c("Tile_X"), edited = FALSE)
#'          worldcover_adjust(world_raster, wc_tile_status)
worldcover_adjust <- function(wcover_tiles, wc_tile_status){ # wcover_tiles = paths of the files, wc_tile_status = T/F if already edited/adjusted


  if (wc_tile_status$edited[wc_tile_status$tile_name == variables$tile_name[v]] == FALSE) {

    WC_Tile <-  wcover_tiles[
      grepl(substr(variables$tile_name[v],2,6), wcover_tiles)
    ]
    WC_Tile <- normalizePath(WC_Tile)

    WC_Tile_raster <- rast(WC_Tile)
    original_coltab <- terra::coltab(WC_Tile_raster)


    input_image <- list.files(path = img_folder, pattern = paste0(".*", variables$tile_name[v], ".*\\.zip$"), full.names = T)
    # Make sure a file was found
    if (length(input_image) == 0) stop("No matching ZIP file found.")

    # Create a temporary directory for unzipping
    tmp_dir <- tempfile(pattern = "unzipped_", tmpdir = "/home/emilio/canopy_height/temp")
    dir.create(tmp_dir)
    cat("Temp_dir created for unzipping and tif conversion:", tmp_dir, "\n")
    # Unzip contents to the temp directory
    unzip(input_image[1], exdir = tmp_dir)
    cat("Reference Image unzipped.\n")

    # List all files recursively inside the unzipped folder
    paths <- list.files(tmp_dir, recursive = TRUE, full.names = TRUE)

    # Filter just for the green band of one tile
    rgb_bands <- paths[
      grepl("IMG_DATA", paths) &
        grepl("R10m", paths) &
        grepl("\\.jp2$", paths) &
        grepl("B03", paths)
    ]
    cat(rgb_bands,"\n")

    # system("gdal_translate /home/emilio/canopy_height/temp/unzipped_8376c4f558105/S2A_MSIL2A_20200421T102021_N0500_R065_T33UUT_20230419T010724.SAFE/GRANULE/L2A_T33UUT_A025230_20200421T102321/IMG_DATA/R10m/T33UUT_20200421T102021_B03_10m.jp2 tmp.tif")
    # tmp_dir <- "/home/emilio/canopy_height/temp/unzipped_8376c4f558105"

    # Convert jp2 band file to tif using gdal
    temp_tif_jp2 <- file.path(tmp_dir,"S2_temp.tif")
    system(paste("gdal_translate", rgb_bands, temp_tif_jp2))
    Input_Raster <- terra::rast(temp_tif_jp2)
    cat("Raster loaded successfully.\n")
    # terra::plot(Input_Raster)

    changed <- FALSE  # Flag to track any change

    # Step 1: CRS
    if (!crs(Input_Raster) == crs(WC_Tile_raster)) {
      message("Aligning Worldcover CRS...")
      WC_Tile_raster <- project(WC_Tile_raster, Input_Raster)
      changed <- TRUE
    }

    # Step 2: Resolution
    if (!isTRUE(all.equal(res(Input_Raster), res(WC_Tile_raster)))) {
      message("Aligning Worldcover resolution...")
      WC_Tile_raster <- resample(WC_Tile_raster, Input_Raster, method = "bilinear")
      changed <- TRUE
    }

    # Step 3: Extent and dimension
    if (!ext(Input_Raster) == ext(WC_Tile_raster) || !all(dim(Input_Raster)[1:2] == dim(WC_Tile_raster)[1:2])) {
      message("Aligning Worldcover extent and dimensions...")
      WC_Tile_raster <- resample(WC_Tile_raster, Input_Raster, method = "bilinear")
      changed <- TRUE
    }

    # Final check
    if (compareGeom(Input_Raster, WC_Tile_raster, stopOnError = FALSE)) {
      message("Worldcover is now fully aligned.")
    } else {
      warning("Worldcover is still not fully aligned. Check manually.")
      changed <- FALSE
    }

    # Save only if changed
    if (changed) {
      out_path <- WC_Tile
      # out_path <- "/home/emilio/global-canopy-height-model/workbench/test.tif" # Test path for debugging
      terra::coltab(WC_Tile_raster) <- original_coltab
      message("Saving aligned raster to: ", out_path)
      writeRaster(WC_Tile_raster, out_path, overwrite = TRUE)
    } else {
      message("No changes made to Worldcover tile. Skipping save.")
    }

    unlink(tmp_dir, recursive = TRUE)
    cat("Temp dir unlinked. Worldcover adjustment finished.\n")
  }

}

