#' Create a data frame of parameter combinations
#'
#' @param tiles Vector of Senitnel-2 tile names. Ex: c("T32TMT")
#' @param bands Vector of Sentinel Bands to be manipulated
#' @param increments Integer vector. Increments by which the Bands should be increased or decreased.
#' @param decrease Character. If "True" increments are decreased, if "False" increment is increased.
#' @param year Character. Year of the Senitnel-2 imagery.
#' @param base_folder Character. Directory that is used for the analysis.
#' @param worldcover Character. Default is "2020".
#'
#' @returns A data frame with all possible combinations of variables.
#' @export
#'
#' @examples variables <- create_param_df(tiles = c("T32TMT"), bands = c("B02","B08"),increments = c(0.05, 0.1, 0.15, 0.2), decrease = c("False"), year = "2020", base_folder = "/home/emilio/wip/global-canopy-height-model")

create_param_df <- function(tiles, bands, increments, decrease, year, base_folder, worldcover = "2020") {

  df <- expand.grid(
    tile_name = tiles,
    band = bands,
    decrease = decrease,
    increment = increments,
    year = year,
    rootDIR = base_folder,
    WC_year = worldcover,
    stringsAsFactors = FALSE
  )

  base_folder <- normalizePath(base_folder)

  # Build path to tile folder with the images
  df$tile_name <- trimws(df$tile_name)
  tile_folder <- file.path(base_folder, "deploy_example","sentinel2", year)
  df$tile_folder <- file.path(tile_folder, df$tile_name)

  # Build output name that is used for saving/copying
  df$out_name <- paste0(df$tile_name,"_",
                        df$band,"_",
                        # Extract the digits after the . in increment, while formatting all increments to always keep two digits:
                        sub("0\\.", "", formatC(df$increment, format = "f", digits = 2)),"_",
                        ifelse(df$decrease == "False","I","D"))

  # Add one row per tile for the original prediction (no manipulation)
  for (t in unique(tiles)) {
    extra_row <- data.frame(
      tile_name = t,
      band = df$band[1],
      decrease = "True",
      increment = 0,
      year = year[1],  # or another default if length(year) > 1
      rootDIR = base_folder,
      WC_year = worldcover,
      stringsAsFactors = FALSE
    )
    extra_row$tile_folder <- file.path(tile_folder, t)
    extra_row$out_name <- paste0(t, "_original")

    df <- rbind(extra_row, df)
  }

  df$out_dir <- file.path(base_folder, "final_results")

  # df <- df[dir.exists(df$tile_folder), ]
  return(df)
}

