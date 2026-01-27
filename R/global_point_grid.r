#' Global Point grid
#'
#' @param lon_grid Numeric. Number of Longitude points spanning the grid.
#' @param lat_grid Numeric. Number of Latitude points spanning the grid.
#' @param lon_border Vector, Numeric. Outer Longitude borders of the desired grid.
#' @param lat_border Vector, Numeric. Outer Latitude borders of the desired grid.
#' @param coast_buffer Numeric. Buffer around Coastlines.
#'
#' @returns Character vector of Latitude and Longitude pairs, spanning a global point grid over the landmasses of earth.
#' @export
#'
#' @examples global_point_grid(lon_grid = 20, lat_grid = 15, lon_border = c(-180,180),
#'                             lat_border = c(-60,60), coast_buffer = -0.05)
#'

global_point_grid <- function(lon_grid = 20, lat_grid = 15, lon_border = c(-180,180), lat_border = c(-60,60), coast_buffer = -0.05){

  # Load land polygons
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  crs_land <- terra::crs(land)

  # Define grid (systematic longitudinal lines)
  n_lines <- lon_grid
  n_lat_points <- lat_grid

  lons <- round(seq(lon_border[1], lon_border[2], length.out = n_lines), digits = 0)
  lats <- round(seq(lat_border[1], lat_border[2], length.out = n_lat_points), digits = 0)

  all_points <- list()

  # Generate points along each longitude and keep only land points
  for (lon in lons) {
    line_points <- data.frame(lon = rep(lon, n_lat_points),
                              lat = lats)
    points_sf <- sf::st_as_sf(line_points, coords = c("lon", "lat"), crs = crs_land)

    # Keep only points on buffered land
    intersections <- sf::st_intersects(points_sf, land)
    land_points <- points_sf[lengths(intersections) > 0, ]

    all_points[[as.character(lon)]] <- land_points
  }

  # Combine all land points
  systematic_points <- do.call(rbind, all_points)

  # Extract coordinates if needed
  new_positions <- sf::st_coordinates(systematic_points)

  # Assuming new_pos is a matrix with columns X (lon) and Y (lat)
  new_pos_char <- apply(new_positions, 1, function(row) paste(row[1], row[2]))

  message("Lat-Lon point grid on land-masses created.")
  return(new_pos_char)

}
