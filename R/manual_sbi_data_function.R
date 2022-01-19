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

#' Function for assembling manual data for SBI calculation
#'
#' Function calculates statistics for the manual sites that you specify for the specific year and survey period
#' @param survey_period Survey period you are calculating SBI for
#' @param manual_sites Manual sites that you want to get data and use to calculate SBI data for
#' @param get_year Year that you are calculating the SBI for
#' @param normals_manual previous normals calculated for manual sites. This is data within the /data folder
#' @param colnames_data_manual Column names of the manual data that you will eventually export
#' @param normal_max Date (year) of max year for normal period
#' @param normal_min Date (year) of min year for normal period
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

manual_sbi_data <- function(survey_period, manual_sites, get_year, normals_manual, colnames_data_manual,
                            normal_max, normal_min,
                            incorrect_sites = NA,
                            incorrect_data = NA) {

  # Convert the survey_period into the right format for manual stations to get the right normal
  if (survey_period == "01-01"){
    time_period <- "01-Jan"
  } else if (survey_period == "02-01"){
    time_period <-  "01-Feb"
  } else if (survey_period == "03-01"){
    time_period <-  "01-Mar"
  } else if (survey_period == "04-01"){
    time_period <-  "01-Apr"
  } else if (survey_period == "05-01"){
    time_period <-  "01-May"
  } else if (survey_period == "05-15"){
    time_period <-  "15-May"
  } else if (survey_period == "06-01"){
    time_period <-  "01-Jun"
  } else if (survey_period == "06-15"){
    time_period <-  "15-Jun"
  } else if (survey_period == "latest"){
    time_period <- "latest"
  } else if (survey_period == "All"){
    time_period <- "All"
  } else{
    time_period <- survey_period
  }

  # Get the statistical data for the manual sites using the percentiles_MSWE()
  data_manual <- bcsnowstats::stats_MSWE(station_id = manual_sites,
                                         survey_period = time_period,
                                         get_year = get_year,
                                         normal_min,
                                         normal_max,
                                         incorrect_sites = incorrect_sites,
                                         incorrect_data = incorrect_data)

  if (dim(data_manual)[2] > 2 && dim(data_manual)[1] >= 1) { #Arrange the statistics dataframe if the percentile function returns data
    data_manual_s <- data_manual %>%
      dplyr::select(station_id,
                    snow_course_name,
                    date_utc,
                    survey_period,
                    swe_mm, swe_mean, Q50, normal_swe_mean, normal_Q50,
                    date_min_utc, date_max_utc,
                    percent_Q50, percent_normal_mean, percent_normal_median,
                    min, max, percentile,
                    numberofyears_raw, numberofyears_estimated,
                    current_rank_min, current_rank_max) %>%
      dplyr::rename(station_name = snow_course_name)

    # Append the correct normal for that survey date
    if (time_period == "01-Jan") {
      prev_norm_time <- "JAN_1"
    } else if (time_period == "01-Feb") {
      prev_norm_time <- "FEB_1"
    } else if (time_period == "01-Mar") {
      prev_norm_time <- "MAR_1"
    } else if (time_period == "01-Apr") {
      prev_norm_time <- "APR_1"
    } else if (time_period == "01-May") {
      prev_norm_time <- "MAY_1"
    } else if (time_period == "15-May") {
      prev_norm_time <- "MAY_15"
    } else if (time_period == "01-Jun") {
      prev_norm_time <- "JUN_1"
    } else if (time_period == "15-Jun") {
      prev_norm_time <- "JUN_15"
    } else {
      print("Error in survey_period")
      prev_norm_time <- time_period
    }

    #select only the date you want. manual_normals_1981t2010 within bcsnowstats package
    normals_prev <- manual_normals_1981t2010 %>%
      dplyr::filter(STATIONID %in% manual_sites) %>% #filter by the manual stations
      dplyr::select(STATIONID, dplyr::contains(paste0(prev_norm_time, "_SWE"))) %>% # filter by SWE columns and date
      dplyr::rename(swenormal_prev = paste0(prev_norm_time, "_SWE"), station_id = STATIONID)

    # append to dataframe; Calculate the percent of normal from previously calculated normals
    data_manual_0 <- dplyr::full_join(data_manual_s, normals_prev, by = "station_id") %>%
      dplyr::mutate(station_type = "manual") %>%
      dplyr::mutate(percent_normal_prev = round(swe_mm / swenormal_prev * 100, digits = 2))

    ## Append the extra variables for the snow basin table: Elevation, snow depth, code, historic 2017 SWE, historic 2016 SWE,
    # Snow depth
    SD <- data_manual %>%
      dplyr::select(station_id, snow_depth_cm) %>%
      dplyr::rename(mean_day_sd = snow_depth_cm)

    # append to the list
    data_manual_sd <- dplyr::full_join(data_manual_0, SD)

    # Add the mean SWE from the previous 2 years
    year_n2 <- bcsnowdata::get_manual_swe(station_id = manual_sites,
                                          get_year = c(as.numeric(get_year) - 2),
                                          survey_period = time_period,
                                          force = FALSE,
                                          ask = FALSE) %>%
      dplyr::mutate(date_dmy = as.Date(date_utc)) %>%
      dplyr::rename(swe_y_2 = swe_mm) %>%
      dplyr::select(station_id, swe_y_2)

    # SWE from the previous year
    year_n1 <- bcsnowdata::get_manual_swe(station_id = manual_sites,
                                          get_year = c(as.numeric(get_year) - 1),
                                          survey_period = time_period,
                                          force = FALSE,
                                          ask = FALSE) %>%
      dplyr::rename(swe_y_1 = swe_mm) %>%
      dplyr::select(station_id, swe_y_1)

    #Compile the SWE from the previous two years together
    years_n12 <- dplyr::full_join(year_n1, year_n2)

    # Join to dataframe
    data_manual_prev <- dplyr::full_join(data_manual_sd, years_n12)

  } else {
    tem <- setNames(data.frame(matrix(ncol = length(colnames_data_manual), nrow = 1)), colnames_data_manual)
    data_manual_prev <- dplyr::bind_rows(tem, data_manual) %>%
      dplyr::filter(!is.na(station_id)) %>%
      dplyr::mutate(station_type = "manual")
  }

  # if there is missing station data, add empty rows
  missing <- tibble::tibble(station_id = manual_sites[!(manual_sites %in% unique(data_manual_prev$station_id))])
  data_manual_1 <- dplyr::bind_rows(data_manual_prev, missing)

  return(data_manual_1)
}
