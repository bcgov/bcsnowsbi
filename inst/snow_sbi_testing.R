# Script for testing SBI functions
# taken from bc_snow_ops
# 20Jan2012
# A day on which the world breathed a sigh of relief...

# ===============================================================================
# clear all
rm(list = ls())

# Get the necessary scripts and libraries
library(bcsnowsbi)
library(bcsnowstats)

# the available basins
basins <- basin_sites(get_basin = "All")$basin

# Issue with basins[3] - central coast
time_start <- Sys.time()
SBI_test <- sbi_bybasin_function(date_sbi = as.Date("01-01-2022", "%d-%m-%Y"),
                                 exceptions = NA,
                                 all_basins = "Yes",
                                 save_csv = "Yes",
                                 path = "V:/Real-time_Data/ASP_daily_interactive/data/SBI",
                                 force = FALSE,
                                 use_sbi_cache = FALSE)
time_total <- Sys.time() - time_start

sites_test <- SBI_test[[2]]
sbi_test_only <- SBI_test[[1]]

# Test the bcsnowstats package functions
# Get the statistical data for the manual sites using the percentiles_MSWE()
data_manual <- bcsnowstats::stats_MSWE(station_id = "3C07",
                                       survey_period = "02-01",
                                       get_year = "2021",
                                       normal_min = 1991,
                                       normal_max = 2020)

data_aswe_check <- bcsnowstats::stats_aswe(station_id = "3C08P",
                                       survey_period = "02-01",
                                       get_year = "2021",
                                       normal_min = 1991,
                                       normal_max = 2020)


# Problem with 85 - 3C07
test_3c07 <- bcsnowdata::get_manual_swe(station_id = "3C07")

SBI_2021_st <- sbi_bybasin_function(date_sbi = as.Date("01-05-2021", "%d-%m-%Y"),
                                 exceptions <- c("1D17P"),
                                 all_basins = "South Thompson",
                                 save_csv = "Yes",
                                 path = "V:/Real-time_Data/ASP_daily_interactive/data/SBI",
                                 force = FALSE,
                                 use_sbi_cache = FALSE)
