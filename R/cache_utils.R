# Script for caching - Cache utilities
# Note that much of this code is inspired (cough *taken*) from the lovely bcmaps() function. All credit to the authors and contributors of this package.
# January 15, 2021, Ashlee Jollymore, BC River Forecast Centre


#' Function for returning the correct data cache directory
#' @export
#' @keywords internal
#' @examples \dontrun{}

data_dir <- function() {
  if (R.Version()$major >= 4) {
    getOption("bcsnowsbi.data_dir", default = tools::R_user_dir("bcsnowsbi", "cache"))
  } else {
    getOption("bcsnowsbi.data_dir", default = rappdirs::user_cache_dir("bcsnowsbi"))
  }

}

#' Function for returning the files within the cached directory
#' @export
#' @keywords internal
#' @examples \dontrun{}
show_cached_files <- function() {
  file.path(list.files(data_dir(), full.names = TRUE))
}

#' Function for returning the files within the cached directory
#' @param dir data directory
#' @param ask Whether to ask the user whether the program can create a directory if it doesn't exist.
#' @export
#' @keywords internal
#' @examples \dontrun{}

check_write_to_data_dir <- function(dir, ask) {

  if (ask) {
    ans <- gtools::ask(paste("bcsnowsbi would like to store this layer in the directory:",
                             dir, "Is that okay?", sep = "\n"))
    if (!(ans %in% c("Yes", "YES", "yes", "y"))) stop("Exiting...", call. = FALSE)
  }

  if (!dir.exists(dir)) {
    message("Creating directory to hold bcsnowsbi data at ", dir)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  } else {
    message("Saving to bcsnowsbi data directory at ", dir)
  }
}
