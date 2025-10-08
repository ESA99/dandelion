#' Bibtex citation of Session
#'
#' @param output_file Character. Name/Location of the TXT file where Bibtex citations of currently loaded packages will be saved.
#'
#' @returns TXT-File with Bibtex citations of all packages currently loaded in the Session, ready to be pasted into Latex. Citation keys are automatically generated, using the package name.
#' @export
#'
#' @examples
#' bibtex_citation_session(tempfile(fileext = ".txt"))
bibtex_citation_session <- function(output_file = "R_attached_packages_citations.txt") {
  # Get packages currently attached (those explicitly loaded)
  attached_pkgs <- .packages()

  # Exclude base packages
  base_pkgs <- c("base", "compiler", "datasets", "graphics",
                 "grDevices", "grid", "methods", "parallel",
                 "splines", "stats", "stats4", "tcltk", "tools", "utils")
  pkgs <- setdiff(attached_pkgs, base_pkgs)

  if (length(pkgs) == 0) stop("No non-base packages are currently attached.")

  # Initialize list for citations
  citations_list <- list()

  for (pkg in pkgs) {
    cat("Processing:", pkg, "\n")
    tryCatch({
      cit <- utils::citation(pkg)
      bib <- utils::toBibtex(cit)

      # Ensure one string per package
      bib_text <- paste(bib, collapse = "\n")

      # Replace or insert proper key
      bib_text <- sub(
        "@[A-Za-z]+\\s*\\{[^,]*,",
        paste0("@Manual{", pkg, ","),
        bib_text
      )

      # Add header comment
      bib_entry <- paste0("% ---- ", pkg, " ----\n", bib_text, "\n")
      citations_list[[pkg]] <- bib_entry
    }, error = function(e) {
      warning(paste("Could not get citation for package:", pkg))
    })
  }

  # Combine all entries and save
  all_citations <- paste(unlist(citations_list), collapse = "\n\n")
  writeLines(all_citations, con = output_file)

  cat("BibTeX citations with keys saved to:", normalizePath(output_file), "\n")
}


