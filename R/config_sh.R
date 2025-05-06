#' Create bash config file for the canopy height model
#'
#' @param directory Parent directory of the model deployment
#' @param tile Tile to be predicted
#' @param year Year as character
#'
#' @returns bash script file
#' @export
#'
#' @examples config_sh("C:/Users/esanc/Documents/", "T32ULC", "2020")
config_sh <- function(directory, tile, year){

  img_folder <- paste0("./deploy_example/sentinel2/", year, "/")

  ### TXT file
  zip_files <- list.files(path = img_folder,
                          pattern = paste0(".*", tile, ".*\\.zip$"),  # Match tile name and end with .zip
                          full.names = FALSE)

  dir.create(paste0(directory,"/deploy_example/image_paths/",year,"/"), recursive=T)
  output_file <- paste0(directory,"/deploy_example/image_paths/",year,"/",tile, ".txt")

  writeLines(zip_files, output_file)
  cat("Created zip file list as text file:", output_file, "with", length(zip_files), "entries.\n")



  ### Configure File
  config_values <- list(
    GCHM_DEPLOY_PARENT_DIR = directory,
    YEAR = year,
    DEPLOY_IMAGE_PATH= paste0(img_folder, zip_files[1]),
    GCHM_DEPLOY_DIR= paste0("./deploy_example/predictions/",year),
    tile_name=tile
  )

  # Path to the config file
  config_path <- paste0(directory,"global_config.sh")

  # Format config lines
  lines <- sapply(names(config_values), function(key) {
    val <- config_values[[key]]
    if (is.character(val)) {
      paste0(key, '="', val, '"')
    } else {
      paste0(key, '=', val)
    }
  })


  writeLines(lines, config_path) # Write the new config
  cat("Updated config-file written to", config_path, "\n")


  cat("Expects images in this folder:", img_folder,"\n")
  cat("Note: Worldcover path are not set here. Edit run_tile_deploy_merge.sh to change.")
}
