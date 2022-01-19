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

#' Function for calculating the SBI once the data is collected for individual sites (output from get_SBI_data() function)
#'
#' @param data dataframe of sites that you are looking to calculate the SBI for
#' @param date_sbi Date you are calculating the SBI for. Defaults to current
#' @param basins Basins that you want to calculate SBI for
#' @importFrom magrittr %>%
#' @importFrom "stats" "median" "setNames"
#' @export
#' @keywords internal
#' @examples \dontrun{}

SBI_function <- function(data, date_sbi = Sys.Date(), basins) {

  #browser()
  #get the survey period for using with data
  date_sbi <- as.Date(date_sbi, format = "%d-%m-%Y")
  survey_period <- format(date_sbi, "%m-%d")
  get_year <- format(date_sbi, "%Y")

  # If you haven't assigned basins as a variable, assign it as NA
  if (!exists("basins")) {
    basins <- NA
  }

  # Get the sites from the data in
  sites_in <- data %>%
    dplyr::filter(basin %in% basins) %>%
    dplyr::select(station_id)

  sites <- sites_in$station_id

  ## Partition by manual and ASWE sites
  # ASWE sites
  ASWE_sites <- sites[sites %in% bcsnowdata::snow_auto_location()$LOCATION_ID]

  # List of manual sites
  manual_sites <- sites[sites %in% bcsnowdata::snow_manual_location()$LOCATION_ID]

  ## Need a line or two to make sure that sites that aren't included in either manual or ASWE are still included!!
  left_over_sites <- sites[which(!sites %in% c(manual_sites, ASWE_sites))]
  if (length(left_over_sites) > 1) {
    sites_missing <- left_over_sites # save to the list you are creating that is output from the function
  } else {
    sites_missing <- tibble::as_tibble(c("All sites accounted for"))
  }

  # Calculate the average of the SWE within all sites = average SWE/average normal SWE
  # if statement in case there is no data returned
  if (length(data) > 1) {
    # calculate the percent of normal from the previously calculated SWE normal
    all <- data %>%
      dplyr::mutate(percent_normal_prev = swe_mm / swenormal_prev * 100)

    all_1 <- all %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(swe_mm)) # remove entries that do not have a current SWE measurement

    # ============
    # if there is a normal for the station, calculate the SBI according to the previous normals
    # ============
    #calculate SBI via new method using the previously calculated normal
    SBI_previous_prelim <- all_1 %>%
      dplyr::filter(!is.na(swe_mm)) %>% # remove entries that do not have a current SWE measurement
      dplyr::filter(!is.na(swenormal_prev)) # remove entries that do not have a  previous normal calculation to calculate the SBI_new_oldnormals

    if (dim(SBI_previous_prelim)[1] > 0) {
      SBI_previous <- SBI_previous_prelim %>%
        dplyr::mutate(SBI_new_oldnormals = round(mean(swe_mm, na.rm = TRUE)/mean(swenormal_prev, na.rm = TRUE)*100, digits = 2)) %>%
        dplyr::select(SBI_new_oldnormals) %>%
        dplyr::distinct(SBI_new_oldnormals, .keep_all = FALSE) %>%
        dplyr::mutate(basin = basins)

    } else { # if there are no normals for the sites within a particular basin, assign the basin name with an NA value
      SBI_previous <- tibble::as_tibble(basins, NaN)
      colnames(SBI_previous) = c("basin", "SBI_new_oldnormals")
    }

    #Identify the sites used to calculate the SBI_new_oldnormals
    sites_SBI_previous <- SBI_previous_prelim %>%
      dplyr::select(station_id)
    sites_SBI_previous <- paste0(sites_SBI_previous) # identify sites that were used with SBI_new_prevnorm

    if (dim(SBI_previous)[1] < 1) {
      SBI_previous[1, ] <- NaN
    }

    # ============
    # Calculate the SBI with the new normal if it exists for the stations
    # ============
    SBI_newnormals_prelim <- all_1 %>%
      dplyr::filter(!is.na(swe_mm)) %>% # remove entries that do not have a current SWE measurement
      dplyr::filter(!is.na(normal_swe_mean)) # remove entries that do not have a  new normal calculation to calculate the SBI_new_newnormals

    if (dim(SBI_newnormals_prelim)[1] > 0) {
      SBI_newnormals <- SBI_newnormals_prelim %>%
        dplyr::mutate(SBI_newnewnormals_mean = round(mean(swe_mm, na.rm = TRUE) / mean(normal_swe_mean, na.rm = TRUE) * 100, digits = 2)) %>% # Calculate the SBI with new normals according to the mean
        dplyr::select(SBI_newnewnormals_mean) %>%
        dplyr::distinct(SBI_newnewnormals_mean, .keep_all = FALSE) %>%
        dplyr::mutate(basin = basins)
    } else { # if there are no normals for the sites within a particular basin, assign the basin name with an NA value
      SBI_newnormals <- tibble::as_tibble(basins, NaN)
      colnames(SBI_newnormals) <- c("basin", "SBI_newnewnormals_mean")
    }

    # Identify the stations used to calculate the SBI with the new normals
    sites_SBI_newnormals_mean <- SBI_newnormals_prelim %>%
      dplyr::select(station_id)
    sites_SBI_newnormals_mean <- paste0(sites_SBI_newnormals_mean) # identify sites that are used within the newSBI_newnormals calculation

    if (dim(SBI_newnormals)[1] < 1) {
      SBI_newnormals[1, ] <- NaN
    }

    # ============
    # Calculate the SBI with the new normal - median values
    # ============
    SBI_newnormals_prelim_median <- all_1 %>%
      dplyr::filter(!is.na(swe_mm)) %>% # remove entries that do not have a current SWE measurement
      dplyr::filter(!is.na(normal_Q50)) # remove entries that do not have a  new normal calculation to calculate the SBI_new_newnormals

    if (dim(SBI_newnormals_prelim_median)[1] > 0) {
      SBI_newnormals_median <- SBI_newnormals_prelim_median %>%
        dplyr::mutate(SBI_newnewnormals_median = round(median(swe_mm, na.rm = TRUE) / median(normal_Q50, na.rm = TRUE) * 100, digits = 2)) %>% # Calculate the SBI with new normals according to the mean
        dplyr::select(SBI_newnewnormals_median) %>%
        dplyr::distinct(SBI_newnewnormals_median, .keep_all = FALSE) %>%
        dplyr::mutate(basin = basins)
    } else { # if there are no normals for the sites within a particular basin, assign the basin name with an NA value
      SBI_newnormals_median <- tibble::as_tibble(basins, NaN)
      colnames(SBI_newnormals_median) <- c("basin", "SBI_newnewnormals_median")
    }

    # Identify the stations used to calculate the SBI with the new normals
    sites_SBI_newnormals_median <- SBI_newnormals_prelim_median %>%
      dplyr::select(station_id)
    sites_SBI_newnormals_median <- paste0(sites_SBI_newnormals_median) # identify sites that are used within the newSBI_newnormals calculation

    # Join both SBI values together
    SBI_all_1 <- dplyr::full_join(SBI_previous, SBI_newnormals, by = "basin")
    SBI_all <- dplyr::full_join(SBI_all_1, SBI_newnormals_median, by = "basin")

    if (dim(SBI_all)[1] < 1) { # if there is no data, replace with NaN
      SBI_all[1, ] <- NaN
    }

    # Calculate statistics
    all_stat <- do.call(data.frame,
                        list(dplyr::summarise(all_1, mean_swe = round(mean(swe_mm, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_Q50 = round(mean(Q50, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_normal = round(mean(normal_swe_mean, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_normal_prev = round(mean(swenormal_prev, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_percentQ50 = round(mean(percent_Q50, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_percentile = round(mean(percentile, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_percentnormal_mean = round(mean(percent_normal_mean, na.rm = TRUE), digits = 2)),
                             dplyr::summarise(all_1, mean_percentnormal_sbiprev = round(mean(percent_normal_prev, na.rm = TRUE), digits = 2))))

    # Bind statistics with the SBI calculation (new method) calculated using both the new and old normals
    all_stat_SBI <- dplyr::bind_cols(all_stat, SBI_all)

    all_stat_1 <- all_stat_SBI %>%
      dplyr::mutate(n_sites = length(all_1$swe_mm)) %>%
      dplyr::mutate(sites_with_data = list(as.matrix(unique(all_1$station_id)))) %>%
      dplyr::mutate(sites_with_data = as.character(sites_with_data)) %>%
      dplyr::mutate(InitialSitesused = as.character(list(as.matrix(unique(all$station_id))))) %>%
      dplyr::mutate(Sitesused_SBInew_oldnormals = ifelse(length(sites_SBI_previous) > 0,
                                                         as.character(list(as.matrix(sites_SBI_previous))),
                                                         "No sites with old normals")) %>%
      dplyr::mutate(Sitesused_SBInew_newnormals_mean = ifelse(length(sites_SBI_newnormals_mean) > 0,
                                                              as.character(list(as.matrix(sites_SBI_newnormals_mean))),
                                                              "No sites with new normals - mean")) %>%
      dplyr::mutate(Sitesused_SBInew_newnormals_median = ifelse(length(sites_SBI_newnormals_median) > 0,
                                                                as.character(list(as.matrix(sites_SBI_newnormals_median))),
                                                                "No sites with new normals - median")) %>%
      dplyr::mutate(MissedSites = as.character(list(as.matrix(sites_missing)))) %>%
      dplyr::mutate(Survey_period = paste0(survey_period, "-", get_year))

  } else {
    all_stat_1 <- setNames(data.frame(matrix(ncol = 19, nrow = 1)),
                           c("mean_swe", "mean_Q50", "mean_normal",
                             "mean_normal_prev", "mean_percentQ50", "mean_percentile",
                             "mean_percentnormal_mean",
                             "mean_percentnormal_sbiprev",
                             "basin",
                             "SBI_new_oldnormals",
                             "SBI_new_newnormals_mean",
                             "SBI_newnewnormals_median",
                             "n_sites", "sites_with_data",
                             "InitialSitesused", "Sitesused_SBInew_oldnormals",
                             "Sitesused_SBInew_newnormals_mean",
                             "Sitesused_SBInew_newnormals_median",
                             "MissedSites", "Survey_period"))
  }
}
