#' Manipulate single Bands of Sentinel 2 images by a percentage
#'
#' @param zip_path Path to the zip file to be manipulated
#' @param band_name Name of the S2 Band to be manipulated ("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10", "B11", "B12", "B8A", "SCL")
#' @param manipulation "increase" or "decrease"
#' @param increment Amount of change applied to the band
#'
#' @returns Returns a zip folder in the original structure (and name) with manipulated Band within a folder structure named after the tile and manipulation
#' @export
#'
#'
#'
s2_manipulate <- function(zip_path, band_name, manipulation = "increase", increment = 0.10) {

  valid_bands <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10", "B11", "B12", "B8A", "SCL")
  if (!(band_name %in% valid_bands)) {
    stop("Invalid band name. Please provide a valid Sentinel-2 band name (e.g., 'B02', 'B03', etc.).\n")
  }

  if (manipulation == "decrease" && increment > 1) {
    stop("The decrement value cannot be greater than 1 (100%), as it would result in negative pixel values. Please provide a valid decrement.\n")
  } else if (increment > 0.5) {
    warning("The increment value is greater than 0.5, which could result in excessively large or small pixel values. Proceed with caution!\n")
  }
  #
  # required_packages <- c("terra", "zip")
  # for (pkg in required_packages) {
  #   if (!requireNamespace(pkg, quietly = TRUE)) {
  #     stop(paste("The package", pkg, "is required but not installed. Please install it using install.packages(\"", pkg, "\")"))
  #   } else {
  #     library(pkg, character.only = TRUE)
  #   }
  # }

  subfolder_name <- paste0(band_name, "_", as.character(increment), "_", manipulation)
  tile <- regmatches(zip_path, regexpr("_T[0-9]{2}[A-Z]{3}", zip_path))
  tile <- sub("_", "", tile)  # remove the leading underscore


  modified_dir <- file.path(dirname(zip_path), "modified", tile, subfolder_name)
  if (!dir.exists(modified_dir)) {
    dir.create(modified_dir, recursive = TRUE)
  }



  temp_dir <- tempfile()  # This creates a new temporary directory
  dir.create(temp_dir)    # Make sure the directory is created



  zip::unzip(zip_path, exdir = temp_dir)

  files <- list.files(temp_dir, pattern = paste0(band_name, ".*\\.jp2$"), full.names = TRUE, recursive = TRUE) # load all the bands (tif files)
  jp2_files <- files[grepl(paste0("IMG_DATA.*/.*", band_name, ".*\\.jp2$"), files)]


  for (i in seq_along(jp2_files)) {

    band_to_modify <- terra::rast(jp2_files[i])

    if (manipulation == "increase") {
      band_to_modify <- band_to_modify * (1 + increment)
    } else if (manipulation == "decrease") {
      band_to_modify <- band_to_modify * (1 - increment)
    } else {
      stop("Invalid manipulation type. Use 'increase' or 'decrease'.")
    }

    # band_to_modify <- as.uint16(rast(jp2_files[i]))  # This keeps data in the range 0-65535, suitable for 16-bit data
    # band_to_modify <- clamp(band_to_modify, 0, 65535, values=TRUE)  # Ensures the values are within [0, 65535]

    terra::values(band_to_modify) <- as.integer(terra::values(band_to_modify))


    terra::writeRaster(band_to_modify, jp2_files[i], overwrite = TRUE, filetype = "JP2OpenJPEG", datatype = "INT2U")

    cat(i, "out of",length(jp2_files),"files written.","\n")
  }


  # Zip the folder back into the 'modified' directory
  new_zip_path <- file.path(modified_dir, basename(zip_path))

  zip::zipr(zipfile = new_zip_path,
      files = list.files(temp_dir, full.names = TRUE, recursive = TRUE))

  # Clean up temporary files
  unlink(temp_dir, recursive = TRUE)

  cat("\nFiles saved in", modified_dir)
}

