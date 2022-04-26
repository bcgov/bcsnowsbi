# SBI testing
rm(list = ls())
library(bcsnowdata)
library(bcsnowstats)
library(bcsnowsbi)


SBI_2022 <- bcsnowsbi::sbi_bybasin_function(date_sbi = Sys.Date(),
                                 all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                 exceptions = c(), # No sites to remove from analysis
                                 incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                 save_csv = "Yes", # Do you want to save
                                 path = "C:/Users/AJOLLYMO/RProjects/SnowData_archive/SBI_cache", # Path where you want to save a csv version of the results
                                 force = FALSE, # Force re-analysis of data rather than getting data from cache
                                 use_sbi_cache = FALSE)


SBI_2022 <- sbi_bybasin_function(date_sbi = "01-04-2022",
                                 all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                 exceptions = c(), # No sites to remove from analysis
                                 incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                 save_csv = "Yes", # Do you want to save
                                 path = "C:/Users/AJOLLYMO/RProjects/SnowData_archive/SBI_cache", # Path where you want to save a csv version of the results
                                 force = FALSE, # Force re-analysis of data rather than getting data from cache
                                 use_sbi_cache = FALSE)

drive = "\\\\DRAIN.dmz\\Shared"
test <- bcsnowsbi::sbi_bybasin_function(date_sbi = Sys.Date() - 1,
                                all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                exceptions = c(), # No sites to remove from analysis
                                incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                save_csv = "Yes", # Do you want to save
                                path = paste0(drive, "/Real-time_Data/ASP_daily_interactive/data/SBI/") # Path where you want to save a csv version of the results
                             )
