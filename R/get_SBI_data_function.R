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

#' Function for aggregating all of the data needed to calculate the SBI for a basin.
#'
#' Inputs are a list of sites, the date, any incorrect manual sites and the corrected values for these sites.
#' @param sites Sites that you want to get data and use to calculate SBI data for
#' @param date_sbi date that you want to calculate the SBI value for
#' @param incorrect_sites Manual site IDs that are incorrect within current survey date
#' @param incorrect_data Data for the manual sites that are incorrect - this is the corrected data that the suer wants to replace with
#' @param normals_ASWE previously calculated normals. defaults to bcsnowstats::ASWE_normals_1981t2010
#' @param normals_manual Previously calculated normals. Defaults to bcsnowstats::manual_normals_1981t2010
#' @importFrom magrittr %>%
#' @importFrom utils read.csv write.csv
#' @export
#' @keywords internal
#' @examples \dontrun{}
normals_ASWE <- bcsnowstats::ASWE_normals_1981t2010
normals_manual <- bcsnowstats::manual_normals_1981t2010

# ======================================
# Function for assembling the individual site data to calculate the SBI
# ======================================
get_SBI_data <- function(sites,
                         date_sbi,
                         incorrect_sites,
                         incorrect_data) {

  # ============================
  # Get and format normals
  # ============================

  # Get the survey period for using with data
  date_sbi <- as.Date(date_sbi, format = "%d-%m-%Y")
  survey_period <- format(date_sbi, "%m-%d")
  get_year <- format(date_sbi, "%Y")

  # =============================
  # Get dates of the normal max and min depending on when the year is
  if (get_year > 2020) {
    normal_max <- 2020
    normal_min <- 1991
  } else if (get_year <= 2020 && get_year > 2010) {
    normal_max <- 2010
    normal_min <- 1981
  } else if (get_year <= 2010 && get_year > 2000) {
    normal_max <- 2000
    normal_min <- 1971
  } else if (get_year <= 2000 && get_year > 1990) {
    normal_max <- 1990
    normal_min <- 1961
  } else if (get_year <= 1990 && get_year > 1980) {
    normal_max <- 1980
    normal_min <- 1961
  } else if (get_year <= 1980 && get_year > 1970) {
    normal_max <- 1970
    normal_min <- 1951
  } else if (get_year <= 1970 && get_year > 1960) {
    normal_max <- 1960
    normal_min <- 1941
  } else {
    print("Issue with normal period definition")
    }

  # Column names of final dataframe
  colnames_data_manual <- c("id", "station_name", "date_utc", "survey_period", "swe_mm", "swe_mean",
                            "Q50", "normal_swe_mean", "date_min_utc", "date_max_utc",
                            "normal_Q50", "percent_Q50", "percent_normal_mean", "percent_normal_median",
                            "min", "max",
                            "percentile",
                            "numberofyears",
                            "swenormal_prev", "station_type", "percent_normal_prev", "mean_day_sd",
                            "swe_y_1", "swe_y_2", "current_rank_min", "current_rank_max")

  colnames_data <- c("id", "station_name",  "date_utc", "swe_mm","swe_mean", "percent_mean",
                     "Q50","percent_Q50" ,"normal_swe_mean","percent_normal_mean", "normal_Q50", "percent_normal_median",
                     "min","date_min_utc", "max", "date_max_utc", "percentile",
                     "current_rank_min","current_rank_max",
                     "date_peak_mean", "peak_mean_swe", "daystopeak_mean",
                     "date_peak_median", "peak_median_swe", "daystopeak_median",
                     "numberofyears", "survey_period", "swenormal_prev", "station_type", "percent_normal_prev",
                     "mean_day_sd", "swe_y_1", "swe_y_2")

  ## Partition by manual and ASWE sites
  # ASWE sites
  ASWE_sites <- sites[sites %in% bcsnowdata::snow_auto_location()$LOCATION_ID]

  # Manual sites
  manual_sites <- sites[sites %in% bcsnowdata::snow_manual_location()$LOCATION_ID]

  ## Check to make sure that all of the sites were associated either with the manual or the automated sites.
  left_over_sites <- sites[which(!sites %in% c(manual_sites, ASWE_sites))]

  if (length(left_over_sites) > 1) {
    sites_missing <- left_over_sites # save to the list you are creating that is output from the function
  } else {
    sites_missing <- data.frame(c("All sites accounted for"))
  }

  # If the survey date is not one of the regular survey periods, reassign the manual sites to be non ( calculating the SBI between regular samnling periods)
  if (survey_period %in% c("01-01", "02-01", "03-01", "04-01", "05-01", "05-15", "06-01", "06-15")) {
    manual_sites <- manual_sites
  } else {
    manual_sites <- c()
  }

  # ===================
  # Filter the sites by those sites that have data for the survey period you are looking for
  # So you don't retrieve data you don't need
  # ===================
  # not sure if this is possible

  # ===================
  #### Get statistics data from the manual and the ASWE sites
  # ===================

  ## Get the statistics for all of the manual sites
  if (length(manual_sites) > 0) {

    # Get the manual data for the SBI calculation using the manual_sbidata() function
    data_manual <- manual_sbi_data(survey_period = survey_period,
                                   manual_sites = manual_sites,
                                   get_year = get_year,
                                   normals_manual = normals_manual,
                                   colnames_data_manual = colnames_data_manual,
                                   normal_max = normal_max,
                                   normal_min = normal_min,
                                   force = force) %>%
      dplyr::mutate(date_utc = date_sbi) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      unique()

  } else { # if there are no sites, create empty dataframe

    data_manual <- data.frame(station_id = as.character(NA))
  } # end of getting data from the manual sites

  # ====================================================================================================
  # ASWE Sites - get and calculate data
  # ====================================================================================================

  if (length(ASWE_sites) > 0) { # if there are ASWE site, then get relevant data

    # Get ASWE data using function
    data_aswe <- aswe_sbidata(ASWE_sites = ASWE_sites,
                                date_sbi = date_sbi,
                                survey_period = survey_period, get_year = get_year, colnames_data = colnames_data,
                                normal_max = normal_max, normal_min = normal_min, force = force)

  } else { # create an empty data frame if you didn't have any aswe sites to get data for (all within cache, etc)
    data_aswe <- setNames(data.frame(matrix(ncol = 33, nrow = 1)), colnames_data)
  }

  # If "m_d" columns exists within the data you created data, get rid of it prior to going forward. This will create issues when you bind with cached data
  if ("m_d" %in% colnames(data_aswe)) {
    data_aswe <- data_aswe %>%
      dplyr::select(-m_d)
  }

  # ================================================
  # End of process to get manual and ASWE data

  # Ensure that the manual data and ASWE data has the same col names

  if (all(is.na(data_manual$swe_mm))) { # if all the manual stations are NA, just export the ASWE value

    all <- data_aswe %>%
      dplyr::mutate(date_max_utc = as.Date(date_max_utc), date_min_utc = as.Date(date_min_utc)) %>%
      # dplyr::mutate(Survey_period = paste0(survey_period.aswe, "-", get.year)) %>%
      dplyr::filter(!is.na(id)) %>%
      dplyr::mutate(survey_period = survey_period)

  } else {

    # Bind manual and snow sites together
    ASWE_t <- data_aswe %>%
      dplyr::mutate(date_max_utc = as.Date(date_max_utc), date_min_utc = as.Date(date_min_utc)) %>%
      dplyr::mutate(normal_swe_mean = as.numeric(as.character(normal_swe_mean))) %>%
      dplyr::mutate(normal_Q50 = as.numeric(as.character(normal_Q50))) #%>% #Reading as character?
      #dplyr::rename(station_type = Station_type)

    manual_t <- data_manual %>%
      dplyr::mutate(station_name = as.character(station_name)) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::filter(!is.na(swe_mm))  #only return data!

    all <- dplyr::full_join(ASWE_t, manual_t) %>%
      # dplyr::mutate(Survey_period = paste0(survey_period.aswe, "-", get.year)) %>%
      dplyr::filter(!is.na(id)) %>%
      dplyr::mutate(survey_period = survey_period)
  }

  return(all)
}
