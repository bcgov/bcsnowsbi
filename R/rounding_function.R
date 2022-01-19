
#' function for rounding all numerics within a dataframe to a certain significant figure
#' @param x data
#' @param digits number of digits that you want to round to
#' @export
#' @keywords internal
#' @examples \dontrun{}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, is.numeric)
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
