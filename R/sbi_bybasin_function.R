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
# ==========================================================

#' SBI Function. For calculating SBI by basin. Function for calculating and saving the SBI by year.
#' Select a year, a survey period (can be all), and this function calculates the SBI and the sites statistics and saves both as two csv files.
#'
#' This function gives you the SBI for one, multiple, or all basins by water year for a specific day or survey period.
#' @param date_sbi Date that you want to calculate SBI for. Must be in date format (i.e., as.Date("YYYY-MM-DD"))
#' @param all_basins What basin you want to calculate the SBI for. Defaults to "Yes", which will return the SBI for all of the current snow basins.
#' @param exceptions Any sites that you want to exclude from the analysis. Defaults to NA
#' @param incorrect_sites Manual site IDs that are showing incorrect or suspect SWE values. Defaults to NA.
#' @param incorrect_data a string of numeric values that are the correct values for the incorrect sites identified in incorrect_sites. Defaults to NA.
#' @param save_csv Whether the user wants to save a csv file of the final SBI values as well as the statistics from the sites used to calculate the SBI values. Can be 'Yes' or "No", but defaults to "No".
#' @param path Path that you want to save the SBI result to
#' @param force Whether you want to force the fresh download of the statistics for the sites used to calculate SBI values. Defaults to FALSE
#' @param ask Whether to ask the user if they want to create a cached data directory. Defaults to FALSE
#' @param use_sbi_cache Whether to try and retrieve SBI values from the cache. If the data exists, then the SBI values will NOT be recalculated. Defaults to FALSE
#' @keywords SBI
#' @importFrom magrittr %>%
#' @export
#' @examples
#' sbi_bybasin_function()

sbi_bybasin_function <- function(date_sbi,
                                all_basins = "Yes",
                                exceptions = NA,
                                incorrect_sites = NA, incorrect_data = NA,
                                save_csv = c("No"),
                                path,
                                force = FALSE,
                                ask = FALSE,
                                use_sbi_cache = FALSE) {

  date_sbi <- as.Date(date_sbi, format = "%d-%m-%Y")

  year_sbi <- bcsnowdata::wtr_yr(date_sbi)

  current_wr <- bcsnowdata::wtr_yr(Sys.Date())

  # Check to ensure that the ASWE archived data has been cached on the user's computer and is up to date
  fname_sbi <- paste0("sbi_current")
  dir_sbi <- data_dir()
  fpath_sbi <- file.path(dir_sbi, fname_sbi)

  # If the user wants to retrieve previously calculated SBI data AND if it exists within the cache AND the water year for the SBI date you want is the same as the current water year
  if (file.exists(fpath_sbi) & use_sbi_cache & year_sbi == current_wr) {

    # Check that the directory exists
    check_write_to_data_dir(dir_sbi, ask)

    # Get the cached SBI values
    sbi_cache <- readRDS(fpath_sbi)

    # Check to ensure that the data contains statistics with the right normal range. Filter for the range you are looking for
    check <- sbi_cache %>%
      dplyr::mutate(survey_period_date = as.Date(Survey_period, format = "%m-%d-%Y")) %>%
      dplyr::filter(survey_period_date == date_sbi) %>%
      dplyr::select(-survey_period_date)

    # if you are looking for one basin,
    if (!(all_basins %in%  c("Yes", "yes", "YES", "all"))) {
      check <- check %>%
        dplyr::filter(basin %in% all_basins)
    }

    # If you are looking for all of the basins, make sure that the archive contains all of the basins
    check_basins <- check$basin

    if (all_basins %in% c("Yes", "yes", "YES", "all")) {
       input_basins <- paste0(basin_sites(get_basin =  "all", exceptions = exceptions)$basin)
    } else {
       input_basins <- all_basins
    }

    # Remove the basins that are present in the cache
    missing_basins <- input_basins[!(input_basins %in% check_basins)]

    # If there is no data for the survey period you want to retrieve, remove the 'check' variable
    if (dim(check)[1] < 1) {
      remove(check)
    }
  } else {
    # Assign the missing basins to a length
    missing_basins <- numeric()
  }

  # If either the check variable doesn't exist, or if the use_sbi_cache variable is FALSE OR if there are missing basins in the cache, calculate the SBI
  if (!exists("check") | use_sbi_cache == FALSE | length(missing_basins) >= 1) {
   #===================================
   # Associate the basin with the sites within the basin that you will use to calculate the SBI value
   #===================================

   # Compensate for any sites that may have been retrieved within the cache by re-assigning all_basins varaible to those basins missing from the
   if (length(missing_basins) >= 1) {
     all_basins <- missing_basins
   }

   # If the user wants to specify sites with a csv file. Make sure that this is within the /data folder
   if (all_basins[1] %in% "csv") {
    # get the file with that shows what sites were associated with the specific basins
    sites_file <- read.csv("data/Sites_byBasin.csv", header = TRUE, na.strings = "") # data file with

    sites_file <- sites_file[, 1:3]
    colnames(sites_file) <- c("Basin", "Date", "Station_ID_used")

    # filter by the year you chose - doesn't matter about water year as water year = year for the survey periods.
    sites_first <- sites_file %>%
      # filter(Basin == "Boundary") %>%
      dplyr::mutate(Date = (as.Date(Date, format = "%d-%b-%y"))) %>%
      dplyr::mutate(Date_year = lubridate::year(Date)) %>%
      dplyr::filter(Date_year == get_year)

    #Subset by the time period
    sites_first <- sites_first %>%
      #dplyr::filter(Date == paste0(time_period, "-", get_year))
      dplyr::filter(Date == as.Date(paste0(survey_period, "-", get_year), format = "%d-%m-%Y"))

    # get a list of basins
    basins_all <- unique(sites_file$Basin)
   }

   # If you want to just associate the sites to a basin according to their ID
   if (all_basins[1] %in% c("Yes", "yes", "YES", "all")) {

    # get all of the sites within the archive - only the active sites
    sites_first <- basin_sites(get_basin =  "all", exceptions = exceptions)
   }

   # If you only wanted to get one basin
   if (all(all_basins %in% unique(basin_sites(get_basin = "All")$basin))) {

    sites_first <- basin_sites(get_basin = all_basins, exceptions) %>%
      dplyr::filter(basin %in% all_basins)
   }

   #===================================
   # Calculate the SBI and site statistics for the basins you've defined
   #===================================

   # df_year <- lapply(survey_period, get_SBI_year, get_year, basins = basins_all, sites = sites_first) # NOT WORkING!!!!
   # This will only work with one survey period
   SBI_basins_year <- get_SBI_year(date_sbi,
                                  sites = sites_first,
                                  incorrect_sites, incorrect_data,
                                  force,
                                  ask
                                  )

   # Unwind data frame
   SBI_year <- do.call("cbind.data.frame", SBI_basins_year$SBI)
   SBI_sites_year <- do.call("cbind.data.frame", SBI_basins_year$SBI_stations) %>%
    # filter(Station_ID %in% unique(c(snow_manual_current()$Number, snow_auto_current()$Station_ID))) # Filter by active sites %>% only works for 2019
    dplyr::filter(!is.na(swe_mm)) %>% # filter only the stations that have data
    dplyr::filter(!(basin %in% c("Province", "Fraser"))) %>%
    dplyr::arrange(station_id, basin)

   # If some of the data was retrieved from the cache, add it to the SBI data
   if (exists("check")) {
     SBI_year <- dplyr::full_join(check, SBI_year) %>%
       dplyr::mutate(survey_period_date = as.Date(Survey_period, "%m-%d-%Y")) %>%
       dplyr::filter(survey_period_date == date_sbi)%>% # make sure that only the survey period you want is added
       dplyr::select(-survey_period_date)
   }

   # If you want to save as a csv as well (defaults to FALSE)
   if (save_csv == "Yes") {

     if (all_basins == "Yes") {
      write.csv(SBI_year, paste0(path, "/SBI_AllBasins_", date_sbi, ".csv"), row.names = FALSE)
      write.csv(SBI_sites_year, paste0(path, "/SBI_sites_AllBasins_", date_sbi,  ".csv"), row.names = FALSE)
     }

     if (all_basins %in% unique(basin_sites(get_basin = "All", exceptions = NaN)$basin)) { # saving one particular basin

      all_basin_name <- gsub(" ", "", c(paste(all_basins, collapse = "_")), fixed = TRUE)

      write.csv(SBI_year, paste0(path, "/SBI_", all_basin_name, "_", date_sbi, ".csv"), row.names = FALSE)
      write.csv(SBI_sites_year, paste0(path, "/SBI_sites_", all_basin_name, "_", date_sbi,  ".csv"), row.names = FALSE)
     }
   } else { # don't save
   }


   # If you are running this for all basins, archive a version (warm state file) of the SBI values. Overwrite the previous SBI values
   # Also only archive if you are calculating SBI values for the current year
   if (year_sbi == current_wr) {

    # Does the cache exist? If not, create it
    if (!file.exists(fpath_sbi)) {

      # Check that the directory exists and create it if not
      check_write_to_data_dir(dir_sbi, ask)

      saveRDS(SBI_year, fpath_sbi)
    } else {
      # Get the cached data
      sbi_cache <- readRDS(fpath_sbi)

      # Get the basin and survey period for the data you just calculated
      survey_basin_new <- paste(SBI_year$Survey_period, SBI_year$basin, sep = "_")

      # Filter out any previously calculated SBI values
      prev_SBI <- sbi_cache %>%
        dplyr::mutate(survey_basin = paste(Survey_period, basin, sep = "_")) %>%
        dplyr::filter(!(survey_basin %in% survey_basin_new)) %>%
        dplyr::select(-survey_basin)

      # Replace with the SBI values you just calculated
      sbi_cache_final <- dplyr::full_join(prev_SBI, SBI_year) %>%
        unique()

      # Save updated cache
      saveRDS(sbi_cache_final, fpath_sbi)
    }
  }

  # If you retrieved data from the archive, send to out
  } else {
    SBI_year <- check
    SBI_sites_year <- NA
  }

  out <- list(SBI = SBI_year, SBI_sites = SBI_sites_year)
}

