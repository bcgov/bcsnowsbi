# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
# ========================================================

#' Function for returning the SBI for a specific subset of sites that you manually determine
#'
#' @param sites String of site IDs that you want to calculate SBI values for
#' @param date_SBI date of the SBI you want to retrieve ("YYYY-mm-dd")
#' @param incorrect_sites Site ID's for manual sites with incorrect data. Defaults to NA.
#' @param incorrect_data Corrected data for manual sites that have incorrect data. Defaults to NA.
#' @param force Whether all of the data should be recalculated rather than retrieved from the statistics data cache. Defaults to FALSE or no.
#' @param ask Whether to ask the user to create a data cache directory. Defaults to FALSE or no.
#' @keywords Get manual SBI data
#' @importFrom magrittr %>%
#' @export
#' @examples
#' snow_basin_average()

snow_basin_average <- function(sites, date_sbi,
                               incorrect_sites = NA, incorrect_data = NA,
                               force = FALSE,
                               ask = FALSE, ...) {
  #browser()
  df_f <- list() # empty list for populating
  # ===========
  # Calculate the basin statistics by sites
  # ===========

  # Rename variables to to avoid recursive error
  sites_i <- sites
  date_SBI_i <- date_SBI
  incorrect_sites_i <- incorrect_sites
  incorrect_data_i <- incorrect_data

  # Get all of the statistics data for all of the sites you are using across all basins
  SBI_data <- get_SBI_data(sites = sites_i,
                           date_SBI = date_SBI_i,
                           incorrect_sites = incorrect_sites_i,
                           incorrect_data = incorrect_data_i)

  df_f[[1]] <- SBI_data  # save as a list

  # ===========
  # Calculate the SBI from site statistics
  # ===========
  df_f[[2]] <- SBI_function(data = SBI_data,
                            sites = sites_i,
                            date_SBI = date_SBI_i,
                            basins = NA) # Save SBI as a list
  return(df_f)
}
