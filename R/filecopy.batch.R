
#' Batch extraction by file type sorted in folders
#'
#' @param path  A file path to the desired folder - needs to end with "/"
#' @param by default "type" | no other options
#'
#' @returns A set of folders for every filetype in the starting directory including subfolders
#' @export
#'
#'
#'
filecopy.batch <- function(path, by="type"){ #important: end path with /
  z <- list.files(path, full.names = T, recursive = F)

  for (i in 1:length(z)) {

    x <- list.files(z[i], full.names = T, recursive = T) # files of one course
    x <- substr(x, nchar(path)+1, nchar(x))
    f <- unique(substr(x, nchar(x)-3, nchar(x)))
    f <- sub(".*\\.", "", f)                              # file types of one course without dots

    for (j in 1:length(f)) {
      ifelse(!dir.exists(paste0(z[i], "/", f[j])), dir.create(paste0(z[i], "/", f[j])), "Folder exists already")

      u <- x[grepl(paste0(".*", paste(f[j], collapse = "|"), "$"), x)]

      for (k in 1:length(u)) {
        setwd(path)
        file.copy(u[k], paste0(z[i], "/", f[j],"/"))
      }
    }
  }

  print("Task finished")
}

