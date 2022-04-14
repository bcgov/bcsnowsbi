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
#' @param force Whether to force the download and recalculation of snow site statistics that have already been run and cached. Defaults to FALSE
#' @param ask Whether to ask the user if it is ok to create a cached directory that holds cached data. Defaults to FALSE
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
                         incorrect_data,
                         force,
                         ask) {

  # ============================
  # Get and format normals
  # ============================
  # Read in the previously calculated normal values for each site - ASWE
  # Read in the previously calculated normal values for each site - manual


  #df <- list() #empty dataframe

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
  ASWE_sites_i <- sites[sites %in% bcsnowdata::snow_auto_location()$LOCATION_ID]

  # Manual sites
  manual_sites_i <- sites[sites %in% bcsnowdata::snow_manual_location()$LOCATION_ID]

  ## Check to make sure that all of the sites were associated either with the manual or the automated sites.
  left_over_sites <- sites[which(!sites %in% c(manual_sites_i, ASWE_sites_i))]

  if (length(left_over_sites) > 1) {
    sites_missing <- left_over_sites # save to the list you are creating that is output from the function
  } else {
    sites_missing <- data.frame(c("All sites accounted for"))
  }

  # If the survey date is not one of the regular survey periods, reassign the manual sites to be non ( calculating the SBI between regular samnling periods)
  if (survey_period %in% c("01-01", "02-01", "03-01", "04-01", "05-01", "05-15", "06-01", "06-15")) {
    manual_sites_i <- manual_sites_i
  } else {
    manual_sites_i <- c()
  }

  # ===================
  # Filter the sites by those sites that have data for the survey period you are looking for
  # So you don't retrieve data you don't need
  # ===================
  # not sure if this is possible

  # ===================
  #### Get statistics data from the manual and the ASWE sites
  # ===================

  # Check to ensure that the manual archived data has been cached on the user's computer and is up to date
  fname_manual <- paste0("sbi_manual_archive.rds")
  dir_man <- data_dir()
  fpath_man <- file.path(dir_man, fname_manual)

  # If the file doesn't exist or the user decides NOT to force the download, calculate the normal data for the station and save it
  if (file.exists(fpath_man) & force == FALSE) {

    # Check that the directory exists
    check_write_to_data_dir(dir_man, ask)

    stats_manual_cached <- readRDS(fpath_man)

    # Check to see whether the data exists for the site and the survey time you want
    data_manual_cached <- stats_manual_cached %>%
      dplyr::filter(station_id %in% manual_sites_i) %>%
      dplyr::filter(survey_period %in% format(date_sbi, "%d-%B")) %>%
      dplyr::filter(lubridate::year(date_utc) %in% get_year)

    # Get the sites that are present for the survey day within the cached data. Creates a list of sites without cached data
    manual_sites <- manual_sites_i[!(manual_sites_i %in% unique(data_manual_cached$station_id))]

    if ("Station_type" %in% colnames(data_manual_cached)) {
      data_manual_cached <- data_manual_cached %>%
        dplyr::select(-Station_type)
    }

  } else if (!file.exists(fpath_man) | force == TRUE) {

    # If you want to forcibly get all of the data or the cache doesn't exist
    manual_sites <- manual_sites_i

    data_manual_cached <- setNames(data.frame(matrix(ncol = length(colnames_data_manual), nrow = 0)), colnames_data_manual)

    if ("Station_ID" %in% colnames(data_manual_cached)) {
      data_manual_cached <- data_manual_cached %>%
        dplyr::select(-Station_ID)
    }
  }

  ## Get the statistics for all of the manual sites without cached data or if you want to force the download
  if (length(manual_sites) > 0) {

    # Get the manual data for the SBI calculation using the manual_sbidata() function
    data_manual_1 <- manual_sbi_data(survey_period, manual_sites, get_year,
                                    normals_manual, colnames_data_manual,
                                    normal_max, normal_min,
                                    force) %>%
      dplyr::mutate(date_utc = date_sbi) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      unique()

  } else { # if there are no sites, create empty dataframe

    data_manual_1 <- data.frame(station_id = as.character(NA))
  } # end of getting data from the manual sites

  # ====================
  # Merge the new with the cached data and update the cache
  # Combined with any cached data
  if (dim(data_manual_1)[1] > 0 && dim(data_manual_cached)[1] > 0) { # if there is both cached and new data

    manual_data <- dplyr::full_join(data_manual_1, data_manual_cached)

  } else if (dim(data_manual_1)[1] > 0 && dim(data_manual_cached)[1] == 0) { # if there is only new data

    manual_data <- data_manual_1

  } else if (dim(data_manual_1)[1] == 0 && dim(data_manual_cached)[1] > 0) { # if there is only cached data

    manual_data <- data_manual_cached

  } else if (dim(data_manual_1)[1] == 0 && dim(data_manual_cached)[1] == 0) { # if there is no data at all

    manual_data <- setNames(data.frame(matrix(ncol = 27, nrow = 1)), colnames_data_manual)
  }

  # =============
  # Update the cache
  # =============

  # If there is no cached data, write it directly (or if you are choosing to force the download)
  if (!file.exists(fpath_man) | force) {
    check_write_to_data_dir(dir_man, ask)

    saveRDS(manual_data, fpath_man) # only save instances when there is data
  } else {

    # Append the data you just retrieved to the data you got from the cache
    data_to_cache_man <- dplyr::full_join(manual_data, stats_manual_cached) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::filter(!is.na(swe_mm)) %>% # only save instances where there is swe data
      dplyr::distinct(.keep_all = TRUE) # filter out any duplicates that might have slipped through

    saveRDS(data_to_cache_man, fpath_man)
  }

  # ====================================================================================================
  # ASWE Sites
  # ====================================================================================================

  # Load cached statistics for ASWE sites previously calculated - if a cached file exists - and excempt from the big loop below

  # Check to ensure that the ASWE archived data has been cached on the user's computer and is up to date
  fname <- paste0("sbi_aswe_archive.rds")
  dir <- data_dir()
  fpath <- file.path(dir, fname)

  # If the file exists or the user decides to force the download, calculate the normal data for the station and save it
  if (file.exists(fpath) & force == FALSE) {

    # Check that the directory exists
    check_write_to_data_dir(dir, ask)

    stats_ASWE_cached <- readRDS(fpath)

    # If there is a m-d column in the cached data, remove it. Otherwise you can't merge with the most recent data
    if ("m_d" %in% colnames(stats_ASWE_cached)) {
     stats_ASWE_cached <- stats_ASWE_cached %>%
       dplyr::select(-m_d)
    }

    survey_date_full <- paste0(get_year, "-", survey_period)

    # Check to see whether the data exists for the site and the survey time you want
    data_aswe_cached <- stats_ASWE_cached %>%
     dplyr::filter(station_id %in% ASWE_sites_i) %>%
     dplyr::filter(date_utc %in% as.Date(survey_date_full))

    # Get the sites that are present for the survey day within the cached data. Creates a list of sites without cached data
    ASWE_sites <- ASWE_sites_i[!(ASWE_sites_i %in% unique(data_aswe_cached$station_id))]

   } else if (!file.exists(fpath) | force == TRUE) { # if the user wants to overwrite data or if there is no cached data, get the data for all the sites
    ASWE_sites <- ASWE_sites_i
    data_aswe_cached <- setNames(data.frame(matrix(ncol = length(colnames_data), nrow = 0)), colnames_data)
   }

  # ================================
  # ASWE sites - get and calculate data
  if (length(ASWE_sites) > 0) { # if there are ASWE site, then get relevant data

    # Get ASWE data using function
    data_aswe_1 <- aswe_sbidata(ASWE_sites = ASWE_sites,
                                date_sbi,
                                survey_period, get_year, colnames_data,
                                normal_max, normal_min, force)

  } else { # create an empty data frame if you didn't have any aswe sites to get data for (all within cache, etc)
    data_aswe_1 <- setNames(data.frame(matrix(ncol = 33, nrow = 1)), colnames_data)
  }

  # If "m_d" columns exists within the data you created data, get rid of it prior to going forward. This will create issues when you bind with cached data
  if ("m_d" %in% colnames(data_aswe_1)) {
    data_aswe_1 <- data_aswe_1 %>%
      dplyr::select(-m_d)
  }

  # Combined with any cached data
  if (dim(data_aswe_1)[1] > 0 && dim(data_aswe_cached)[1] > 0) { # if there is both cached and new data
    ASWE_data <- dplyr::full_join(data_aswe_1, data_aswe_cached)
  } else if (dim(data_aswe_1)[1] > 0 && dim(data_aswe_cached)[1] == 0) { # if there is only new data
    ASWE_data <- data_aswe_1
  } else if (dim(data_aswe_1)[1] == 0 && dim(data_aswe_cached)[1] > 0) { # if there is only cached data
    ASWE_data <- data_aswe_cached
  } else if (dim(data_aswe_1)[1] == 0 && dim(data_aswe_cached)[1] == 0) { # if there is no data at all
    ASWE_data <- setNames(data.frame(matrix(ncol = 27, nrow = 1)), colnames_data_manual)
  }

  # =============
  # Update the cache - append_cache variable. Choices are "Yes", "No" and "Overwrite"
  # =============

  # If there is no cached data, write it directly (or if you are choosing to force the download)
  if (!file.exists(fpath) | force) {
    check_write_to_data_dir(dir, ask)

    saveRDS(ASWE_data, fpath)
  } else {

    # Append the data you just retrieved to the data you got from the cache
    data_to_cache <- dplyr::full_join(ASWE_data, stats_ASWE_cached) %>%
      dplyr::mutate(date_utc = as.Date(date_utc)) %>%
      dplyr::filter(!is.na(swe_mm)) %>% # save only instances where there is swe data
      dplyr::distinct(station_id, date_utc, swe_mm, Q50, normal_swe_mean, percent_Q50,
                      percent_normal_mean,
                      percentile, .keep_all = TRUE) # filter out any duplicates that might have slipped through

    saveRDS(data_to_cache, file = fpath)

  }

  # ================================================
  # End of process to get manual and ASWE data

  # Ensure that the manual data and ASWE data has the same col names

  if (all(is.na(manual_data$swe_mm))) { # if all the manual stations are NA, just export the ASWE value

    all <- ASWE_data %>%
      dplyr::mutate(date_max_utc = as.Date(date_max_utc), date_min_utc = as.Date(date_min_utc)) %>%
      # dplyr::mutate(Survey_period = paste0(survey_period.aswe, "-", get.year)) %>%
      dplyr::filter(!is.na(id)) %>%
      dplyr::mutate(survey_period = survey_period)

  } else {

    # Bind manual and snow sites together
    ASWE_t <- ASWE_data %>%
      dplyr::mutate(date_max_utc = as.Date(date_max_utc), date_min_utc = as.Date(date_min_utc)) %>%
      dplyr::mutate(normal_swe_mean = as.numeric(as.character(normal_swe_mean))) %>%
      dplyr::mutate(normal_Q50 = as.numeric(as.character(normal_Q50))) #%>% #Reading as character?
      #dplyr::rename(station_type = Station_type)

    manual_t <- manual_data %>%
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
