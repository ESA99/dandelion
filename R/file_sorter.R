#' Sort files within a directory into folders by group
#'
#' @param path  character. A file path to the folder containing the files that should be organised. Accepts also zip-folders.
#' @param by character. Default "type". Parameter by which the folders should be organised. Valid values c("type", "date").
#'
#' @returns Returns the folder "files_sorted" including all the files sorted into folders by their file-type. A copy summary is displayed after execution.
#' @export
#'
#'
file_sorter <- function(path, by = "type") {
  is_zip <- grepl("\\.zip$", path, ignore.case = TRUE)

  # Normalize base path
  if (is_zip) {
    base_path <- dirname(path)
    zip_file <- path
  } else {
    if (!endsWith(path, "/")) path <- paste0(path, "/")
    base_path <- path
  }

  sorted_base <- file.path(base_path, "files_sorted")
  if (!dir.exists(sorted_base)) dir.create(sorted_base)

  log <- list()
  rename_counter <- 0
  temp_dirs <- c()

  # Determine input sources
  if (is_zip) {
    unzip_dir <- tempfile(pattern = "unzipped_")
    dir.create(unzip_dir)
    unzip(zip_file, exdir = unzip_dir)
    sources <- unzip_dir
    temp_dirs <- c(temp_dirs, unzip_dir)
  } else {
    entries <- list.files(base_path, full.names = TRUE, recursive = FALSE)
    sources <- entries[dir.exists(entries) | grepl("\\.zip$", entries, ignore.case = TRUE)]
  }

  for (entry in sources) {
    if (dir.exists(entry)) {
      source_dir <- entry
    } else if (grepl("\\.zip$", entry, ignore.case = TRUE)) {
      unzip_dir <- tempfile(pattern = "unzipped_")
      dir.create(unzip_dir)
      unzip(entry, exdir = unzip_dir)
      source_dir <- unzip_dir
      temp_dirs <- c(temp_dirs, unzip_dir)
    } else {
      next
    }

    files <- list.files(source_dir, full.names = TRUE, recursive = TRUE)

    for (file in files) {
      if (!file.exists(file) || dir.exists(file)) next

      if (by == "type") {
        category <- tools::file_ext(file)
        if (category == "") category <- "no_ext"
      } else if (by == "date") {
        mod_time <- file.info(file)$mtime
        category <- format(mod_time, "%Y-%m-%d")
      } else {
        stop("Invalid 'by' argument. Use 'type' or 'date'.")
      }

      dest_dir <- file.path(sorted_base, category)
      if (!dir.exists(dest_dir)) dir.create(dest_dir)

      dest_file <- file.path(dest_dir, basename(file))

      # Name conflict resolution
      if (file.exists(dest_file)) {
        base <- tools::file_path_sans_ext(basename(file))
        ext <- tools::file_ext(file)
        counter <- 1
        repeat {
          new_name <- paste0(base, "_", counter, ifelse(ext != "", paste0(".", ext), ""))
          dest_file <- file.path(dest_dir, new_name)
          if (!file.exists(dest_file)) break
          counter <- counter + 1
        }
        rename_counter <- rename_counter + 1
      }

      copied <- file.copy(file, dest_file)
      if (copied) {
        log[[length(log) + 1]] <- category
      }
    }
  }

  # Cleanup
  unlink(temp_dirs, recursive = TRUE)

  # Summary
  log_table <- table(unlist(log))
  cat("Copy Summary:\n")
  cat(" - Total files copied:", length(log), "\n")
  if (length(log_table) > 0) {
    cat(" - Files sorted into:\n")
    print(log_table)
  } else {
    cat(" - No files found or copied.\n")
  }
  cat(" - Files renamed due to conflict:", rename_counter, "\n")

  invisible(log_table)
}
