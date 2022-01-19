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

#' Function for getting extra snow data (such as SWE for previous years and snow depth) to append to statistics table for ASWE data
#'
#' Meant to be an internal function called within an lapply() loop by the aswe_sbidata() function. The lapply() approach is used in case data retrieve functions miss data when asked to retrieve a large amount of data
#' @param ASWE_sites_i ASWE sites that you want to get data and use to calculate SBI data for
#' @param get_year Year
#' @param survey_period Survey period
#' @importFrom magrittr %>%
#' @importFrom lubridate year month day
#' @export
#' @keywords internal
#' @examples \dontrun{}

getextra_snowdata <- function(ASWE_sites_i, get_year, survey_period) {

  # Snow depth
  SD_1 <- bcsnowdata::get_aswe_databc(station_id = ASWE_sites_i,
                        get_year = get_year,
                        parameter_id = "Snow_Depth",
                        force = FALSE,
                        ask = FALSE)

  SD <- SD_1 %>%
    dplyr::mutate(date_dmy = as.Date(date_utc)) %>%
    dplyr::group_by(date_dmy, station_id) %>%
    dplyr::mutate(mean_day_sd = mean(value, na.rm = TRUE)) %>%
    dplyr::filter(date_dmy %in% paste0(get_year, "-", survey_period)) %>%
    dplyr::distinct(mean_day_sd, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(station_id, date_dmy, mean_day_sd)

  #if dataframe is empty, assign NaN
  if (dim(SD)[1] < 1) {
    SD <- data.frame(station_id = ASWE_sites_i,
                     mean_day_sd = NaN)
  }

  # Add the mean SWE from the previous 2 years
  year_n2 <- bcsnowdata::get_aswe_databc(station_id = ASWE_sites_i,
                             get_year = c(as.numeric(get_year) - 2),
                             parameter_id = "SWE",
                             force = FALSE,
                             ask = FALSE) %>%
    dplyr::mutate(date_dmy = as.Date(date_utc)) %>%
    dplyr::filter(as.numeric(lubridate::month(date_dmy)) == as.numeric(lubridate::month(paste0(get_year, "-", survey_period)))) %>%
    dplyr::filter(as.numeric(lubridate::day(date_dmy)) == as.numeric(lubridate::day(paste0(get_year, "-", survey_period)))) %>%
    dplyr::group_by(date_dmy, station_id) %>%
    dplyr::mutate(swe_y_2 = mean(value, na.rm = TRUE)) %>% #take the mean SWE by day
    dplyr::distinct(date_dmy, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(station_id, swe_y_2)

  #if dataframe is empty, assign NaN
  if (dim(year_n2)[1] < 1) {
    year_n2 <- data.frame(station_id = ASWE_sites_i,
                          swe_y_2 = NaN)
  }

  year_n1 <- bcsnowdata::get_aswe_databc(station_id = ASWE_sites_i,
                             get_year = c(as.numeric(lubridate::year(paste0(get_year, "-", survey_period))) - 1),
                             parameter_id = "SWE",
                             force = FALSE,
                             ask = FALSE) %>%
    dplyr::mutate(date_dmy = as.Date(date_utc)) %>%
    dplyr::filter(as.numeric(lubridate::month(date_dmy)) == as.numeric(lubridate::month(paste0(get_year, "-", survey_period)))) %>%
    dplyr::filter(as.numeric(lubridate::day(date_dmy)) == as.numeric(lubridate::day(paste0(get_year, "-", survey_period)))) %>%
    dplyr::group_by(date_dmy, station_id) %>%
    dplyr::mutate(swe_y_1 = mean(value, na.rm = TRUE)) %>% #take the mean SWE by day
    dplyr::distinct(date_dmy, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(station_id, swe_y_1)

  #if dataframe is empty, assign NaN
  if (dim(year_n1)[1] < 1) {
    year_n1 <- data.frame(station_id = ASWE_sites_i,
                          swe_y_1 = NaN)
  }

  # bind together within one data frame
  bind_1 <- dplyr::full_join(SD, year_n1, by = "station_id")
  d_out <- dplyr::full_join(bind_1, year_n2, by = "station_id")
  return(d_out)
}
