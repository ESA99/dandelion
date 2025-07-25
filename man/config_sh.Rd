% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_sh.R
\name{config_sh}
\alias{config_sh}
\title{Create bash config file for the canopy height model}
\usage{
config_sh(directory, tile, year, wcover = "2020")
}
\arguments{
\item{directory}{character string. Parent directory of the model deployment.}

\item{tile}{character string. Name of the Sentinel-2 Tile.}

\item{year}{character string. Year of the imagery used for prediction.}

\item{wcover}{character string. Year of the Worldcover data to be used. Defaults to "2020". Valid values: c("2020", "2021"). Important: Data has to be available in ./deploy_example/ESAworldcover/Year}
}
\value{
bash script file
}
\description{
Create bash config file for the canopy height model
}
\examples{
config_sh("./Users/esanc/Documents/", "T32ULC", "2020")
}
