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
# =====================================

#' Function that gets data to calculate SBI and then calculates the SBI using the manual and ASWE data retrieved.
#'
#' This function collects the statistics data for individual sites using the get_SBI_data() function, and then calculates the SBI by basin using SBI_sites_function()
#' @param date_sbi Date that you want to calculate the SBI value for
#' @param sites Sites that you want to get data and use to calculate SBI data for
#' @param incorrect_sites Manual site IDs that are incorrect within current survey date
#' @param incorrect_data Data for the manual sites that are incorrect - this is the corrected data that the suer wants to replace with
#' @param force Whether to force the download and recalculation of snow site statistics that have already been run and cached. Defaults to FALSE
#' @param ask Whether to ask the user if it is ok to create a cached directory that holds cached data. Defaults to FALSE
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

get_SBI_year <- function(date_sbi,
                         sites,
                         incorrect_sites, incorrect_data,
                         force = FALSE,
                         ask = FALSE,...) {

  # convert the survey_period into the right format for manual stations to get the right sites for that basin
  #if (survey_period == "01-01"){
  #  time_period <- "01-Jan"
  #} else if (survey_period == "02-01"){
  #  time_period <-  "01-Feb"
  #} else if (survey_period == "03-01"){
  #  time_period <-  "01-Mar"
  #} else if (survey_period == "04-01"){
  #  time_period <-  "01-Apr"
  #} else if (survey_period == "05-01"){
  #  time_period <-  "01-May"
  #} else if (survey_period == "05-15"){
  #  time_period <-  "15-May"
  #} else if (survey_period == "06-01"){
  #  time_period <-  "01-Jun"
  #} else if (survey_period == "06-15"){
  #  time_period <-  "15-Jun"
  #} else {
  #  time_period <- survey_period
  #}

  # get sbi date
  #date_SBI <- format(as.Date(paste0(survey_period, "-", get_year), format = "%m-%d-%Y"), "%d-%m-%Y")

  # Calculate the statistics, including the snow basin index
  # First calculate the statistics for all sites across all basins (attempt to speed up the function)
  all_sites <- paste(sites$stations, collapse = ";")
  all_sites_1 <- unlist(strsplit(as.character(all_sites), ";"))
  all_sites_2 <- gsub("\t", "", all_sites_1)
  all_sites_3 <- unique(gsub(" ", "", all_sites_2))

  # Get all of the statistics data for all of the sites you are using across all basins
  SBI_data <- get_SBI_data(sites = all_sites_3,
                           date_sbi,
                           incorrect_sites,
                           incorrect_data,
                           force,
                           ask) %>%
    dplyr::arrange(id)

  # Assign the basin name to the sites using the site
  SBI_data_basin <- dplyr::full_join(SBI_data, site_basinname(id = SBI_data$id)) %>%
    dplyr::filter(basin %in% unique(sites$basin)) # filter for only the basins that you are wanting to actually calculate an SBI value for

  # Calculate the SBI by basin, associating the statistical data for a site with the basin itself
  basins <- unique(SBI_data_basin$basin)

  SBI <- lapply(basins,
                SBI_sites_function,
                date_sbi = date_sbi,
                SBI_data = SBI_data_basin)

  # Unwind the list you just created
  test_SBI <- tibble::as_tibble(data.table::rbindlist(lapply(SBI, unwind_SBI), fill = TRUE))

  out <- list(SBI = test_SBI, SBI_stations = SBI_data_basin)
  return(out)
}
