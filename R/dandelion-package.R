#' dandelion: Neural-Network Deployment Tools for Geodata
#'
#' @description
#' The **dandelion** package provides functions, supporting the systematical deployment of neural networks using geodata.
#' It is designed to work in the context of GCHM deployments, making it easy to preprocess,
#' align, and manipulate raster tiles efficiently.
#'
#' @details
#' Main functions include:
#' \itemize{
#'   \item Creating a data frame of parameters to use in loop deployment.
#'   \item Cropping, aligning, and manipulating raster datasets.
#'   \item Tools for looping over multiple tiles and avoiding redundant processing.
#'   \item Efficient file handling.
#'   \item Avoiding repetitive tasks.
#' }
#'
#' The package expects certain variables (like `variables`, `v`, and `img_folder`)
#' to be defined in the deployment context when using some functions.
#'
#' @section Core Deployment Functions:
#' \describe{
#'   \item{\code{create_param_df()}}{Create a data frame of parameter combinations.}
#'   \item{\code{worldcover_adjust()}}{Adjusts Worldcover images to match Sentinel2 structure.}
#' }
#'
#' @section Preparation functions:
#' \describe{
#'   \item{\code{crop_raster_with_s2()}}{Crop Raster with Sentinal-2 imagery.}
#'   \item{\code{config_sh()}}{Create bash config file for the canopy height model.}
#'   \item{\code{s2_manipulate()}}{Manipulate single Bands of Sentinel 2 images by a percentage.}
#'   }
#' @section Further useful functions:
#' \describe{
#'   \item{\code{bibtex_citation_session()}}{Creates Bibtex citation text file of Session.}
#'   \item{\code{file_sorter()}}{Sort files within a directory into folders by group.}
#'  }
#'
#' @seealso
#' Deployment of functions in context of global canopy height prediction: \url{https://github.com/ESA99/canopy_height}
#'
#' @keywords internal
"_PACKAGE"
