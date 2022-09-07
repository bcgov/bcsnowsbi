# SBI testing
rm(list = ls())
library(bcsnowdata)
library(bcsnowstats)
library(bcsnowsbi)


SBI_2022 <- bcsnowsbi::sbi_bybasin_function(date_sbi = as.Date("2022-04-01"),
                                 all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                 exceptions = c(), # No sites to remove from analysis
                                 incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                 save_csv = "No", # Do you want to save
                                 path = "G:/Snow/sbi_archive/")
t <- SBI_2022[[1]]

SBI_2022 <- sbi_bybasin_function(date_sbi = "01-04-2022",
                                 all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                 exceptions = c(), # No sites to remove from analysis
                                 incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                 save_csv = "Yes", # Do you want to save
                                 path = "C:/Users/AJOLLYMO/RProjects/SnowData_archive/SBI_cache", # Path where you want to save a csv version of the results
                                 force = FALSE, # Force re-analysis of data rather than getting data from cache
                                 use_sbi_cache = FALSE)

drive = "\\\\DRAIN.dmz\\Shared"
test <- sbi_bybasin_function(date_sbi = Sys.Date(),
                                all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                exceptions = c(), # No sites to remove from analysis
                                incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                save_csv = "Yes", # Do you want to save
                                path = paste0(drive, "/Real-time_Data/ASP_daily_interactive/data/SBI/") # Path where you want to save a csv version of the results
                             )
t <- test[[1]]

drive = "\\\\DRAIN.dmz\\Shared"
vancouverisland <- sbi_bybasin_function(date_sbi = "2022-05-01",
                             all_basins = "Vancouver Island", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                             exceptions = c(), # No sites to remove from analysis
                             incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                             save_csv = "Yes", # Do you want to save
                             path = paste0(drive, "/Real-time_Data/ASP_daily_interactive/data/SBI/") # Path where you want to save a csv version of the results
)
va_t <- vancouverisland[[2]]
ASWE_sites <- bcsnowdata::snow_auto_location()$LOCATION_ID
manual <- bcsnowstats::get_snow_stats(station_id = manual_sites,
                            survey_period = time_period,
                            get_year = get_year,
                            normal_min = normal_min,
                            normal_max = normal_max, force = FALSE)

# Function to save historic SBI

historic_sbi <- function(date) {
  t <- sbi_bybasin_function(date_sbi = as.Date("2021-12-01"),
                       all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                       exceptions = c(), # No sites to remove from analysis
                       incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                       save_csv = "Yes", # Do you want to save
                       path = paste0("G:/Snow/sbi_archive/"))
}

year <- 2018

dates <- c(as.Date(paste0(year-1, "-12-01")), as.Date(paste0(year, "-01-01")),
            as.Date(paste0(year, "-02-01")),
            as.Date(paste0(year, "-03-01")), as.Date(paste0(year, "-04-01")),
            as.Date(paste0(year, "-05-01")), as.Date(paste0(year, "-05-15")),
            as.Date(paste0(year, "-06-01")), as.Date(paste0(year, "-06-15")))

lapply(dates,
       sbi_bybasin_function,
       all_basins = "Yes",
       incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
       save_csv = "Yes", # Do you want to save
       path = paste0("G:/Snow/sbi_archive/"))
