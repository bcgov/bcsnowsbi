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

#' Function for getting the ASWE data and statistics to calculate SBI values for. SBI function, Ashlee Jollymore 8Feb2021
#'
#' Inputs are a list of sites, the date, any incorrect manual sites and the corrected values for these sites.
#' @param ASWE_sites ASWE sites that you want to get data and use to calculate SBI data for
#' @param date_sbi date that you want to calculate the SBI value for
#' @param survey_period Survey period that you want to calculate the SBI for
#' @param get_year Water year you want to calculate the SBI for
#' @param colnames_data Column names for the final dataframe fo ASWE data that will then be used to calculate SBI data
#' @param normal_max Max year of normal period
#' @param normal_min Min year of normal period
#' @param force Force recalculation of statistics
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

aswe_sbidata <- function(ASWE_sites, date_sbi, survey_period, get_year, colnames_data, normal_max, normal_min, force) {

 # Get the statistics for today by site using the stats_aswe() function from the bcsnowstats:: function
 data_aswe <- bcsnowstats::stats_aswe(station_id = ASWE_sites,
                                     survey_period = survey_period,
                                     get_year = get_year,
                                     normal_min = normal_min,
                                     normal_max = normal_max,
                                     force = force)


 # Filter the ASWE sites by the survey period - include the normal mean - s2
 if (dim(data_aswe)[2] > 2) { #Arrange the statistics dataframe if there is data returned for the sites

   # Get only one value per day- the mean SWE and rename as the SWE_mm
   data_aswe_0 <- data_aswe %>%
     dplyr::select(id, date_utc, mean_day, swe_mean, percent_mean, Q50, percent_Q50, normal_swe_mean, percent_normal_mean,
                  normal_Q50, percent_normal_median,
                  min, date_min_utc,
                  max, date_max_utc, percentile,
                  current_rank_min, current_rank_max,
                  date_peak_median, peak_median_swe,
                  daystopeak_median,
                  numberofyears) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(date_utc = as.Date(date_utc)) %>%
     dplyr::rename(swe_mm = mean_day) %>%
     dplyr::ungroup() %>%
     #dplyr::select(-m_d) %>%
     dplyr::distinct(id, swe_mm, .keep_all = TRUE)

   # Append the normal for that station
   if (survey_period == "01-01") {
     prev_norm_time <- "JAN_1"
   } else if (survey_period == "02-01") {
     prev_norm_time <- "FEB_1"
   } else if (survey_period == "03-01") {
     prev_norm_time <- "MAR_1"
   } else if (survey_period == "04-01") {
     prev_norm_time <- "APR_1"
   } else if (survey_period == "05-01") {
     prev_norm_time <- "MAY_1"
   } else if (survey_period == "05-15") {
     prev_norm_time <- "MAY_15"
   } else if (survey_period == "06-01") {
     prev_norm_time <- "JUN_1"
   } else if (survey_period == "06-15") {
     prev_norm_time <- "JUN_15"
   } else {
   }

   if (exists("prev_norm_time")) { # if there is a normal value for that day, append it
     #select only the date you want from the normals data previously calculated
     normals_prev_ASWE <- normals_ASWE %>%
       dplyr::filter(STATIONID %in% ASWE_sites) %>% #filter by the ASWE stations
       dplyr::select(STATIONID, dplyr::contains(paste0(prev_norm_time, "_SWE"))) %>% # filter by SWE columns and date
       dplyr::rename(swenormal_prev = paste0(prev_norm_time, "_SWE"), id = STATIONID)

     # append to dataframe
     data_aswe_0 <- dplyr::full_join(data_aswe_0, normals_prev_ASWE, by = c("id")) %>%
       dplyr::mutate(station_type = "ASWE")
   } else {
     data_aswe_0 <- data_aswe_0 %>%
       dplyr::mutate(swenormal_prev = NA) %>%
       dplyr::mutate(station_type = "ASWE")
   }

   # Calculate the percent of normal from previously calculated normals
   data_aswe_0 <- data_aswe_0 %>%
     dplyr::mutate(percent_normal_prev = round(swe_mean / swenormal_prev * 100, digits = 2))

   # ===================
   # Calculate the snow depth and previous years SWE (previous two years)
   # Append the extra variables for the snow basin table: Elevation, snow depth, code, historic 2017 SWE, historic 2016 SWE
   # ===================
   # Need for function and loop? Error in cannot allocate large vector if there are a large number of stations.
   # Have only if you are using this on a large number of stations?

   # Tried lapply loop with this function rather than calling a large number of stations. Data loss within the get_aswe_databc() function!! Can't trust it!

   # test lapply function
   lapply_solution <- lapply(ASWE_sites, getextra_snowdata,
                             get_year, survey_period)

   # unwrap the created list with previous years stats
   stats_prev_unlisted <- do.call("rbind.data.frame", lapply_solution) %>%
     dplyr::arrange(station_id)

   # Join to dataframe - snow data you already have
   data_aswe_prev <- dplyr::full_join(data_aswe_0, stats_prev_unlisted, by = "station_id")

 } else { # create en empty row if there is no data retrieved

   data_aswe_prev <- dplyr::bind_rows(setNames(data.frame(matrix(ncol = 33, nrow = 1)), colnames_data), data_aswe) %>%
     dplyr::filter(!is.na(station_id))
 }

 # Add in any missing stations
 missing_aswe <- tibble::tibble(station_id = ASWE_sites[!(ASWE_sites %in% unique(data_aswe_prev$station_id))])
 data_aswe_1 <- dplyr::bind_rows(data_aswe_prev, missing_aswe)

 # If the m_d column exists within the data, get rid of it.
 if ("m_d" %in% colnames(data_aswe_1)) {
  data_aswe_1 <- data_aswe_1 %>%
    dplyr::select(-m_d)
 }
 return(data_aswe_1)
}
