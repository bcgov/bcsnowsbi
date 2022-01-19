
#' Functions for unwinding lists created by SBI_sites_function()
#' @export
#' @keywords internal
#' @examples \dontrun{}
unwind_SBI <- function(list) {
  temp <- do.call("cbind.data.frame", list$SBI)
}

#' Function for unwinding
#' @export
#' @keywords internal
#' @examples \dontrun{}
unwind_stations <- function(list) {
  temp <- do.call("cbind.data.frame", list$SBI_sites)
}
