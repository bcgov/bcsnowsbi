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
                                path) {

  date_sbi <- as.Date(date_sbi, format = "%d-%m-%Y")

  year_sbi <- bcsnowdata::wtr_yr(date_sbi)

  current_wr <- bcsnowdata::wtr_yr(Sys.Date())

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
   if (all_basins[1] %in% c("Yes", "yes", "YES", "all", "ALL", "All")) {

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
   # This will only work with one survey period
   SBI_basins_year <- get_SBI_year(date_sbi,
                                  sites = sites_first,
                                  incorrect_sites, incorrect_data)

   # Unwind data frame
   SBI_year <- do.call("cbind.data.frame", SBI_basins_year$SBI)
   SBI_sites_year <- do.call("cbind.data.frame", SBI_basins_year$SBI_stations) %>%
    # filter(Station_ID %in% unique(c(snow_manual_current()$Number, snow_auto_current()$Station_ID))) # Filter by active sites %>% only works for 2019
    dplyr::filter(!is.na(swe_mm)) %>% # filter only the stations that have data
    dplyr::filter(!(basin %in% c("Province", "Fraser"))) %>%
    dplyr::arrange(id, basin)

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

  out <- list(SBI = SBI_year, SBI_sites = SBI_sites_year)
}

