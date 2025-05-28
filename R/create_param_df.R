#' Create a data frame of parameter combinations
#'
#' @param tiles Vector of Senitnel-2 tile names. Ex: c("T32TMT")
#' @param bands Vector of Sentinel Bands to be manipulated
#' @param increments Increments by which the Bands should be increased or decreased.
#' @param decrease Character. If "True" increments are decreased, if "False" increment is increased.
#' @param year Year of the Senitnel-2 imagery.
#' @param base_folder Directory that is used for the analysis.
#' @param worldcover Default is "2020".
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

  df$tile_name <- trimws(df$tile_name)
  base_folder <- normalizePath(base_folder)
  tile_folder <- file.path(base_folder, "deploy_example","sentinel2", year)
  df$tile_folder <- file.path(tile_folder, df$tile_name)
  df$out_name <- paste0(df$tile_name,"_",df$band,"_",df$increment,"_", ifelse(df$decrease == "False","I","D"))

  # Add one row per tile for the original prediction (no manipulation)
  for (t in unique(tiles)) {
    extra_row <- data.frame(
      tile_name = t,
      band = NA,
      decrease = NA,
      increment = 0,
      year = year[1],  # or another default if length(year) > 1
      rootDIR = base_folder,
      WC_year = worldcover,
      stringsAsFactors = FALSE
    )
    extra_row$tile_folder <- file.path(tile_folder, t)
    extra_row$out_name <- paste0(t, "_original")

    df <- rbind(df, extra_row)
  }

  # df <- df[dir.exists(df$tile_folder), ]
  return(df)
}

