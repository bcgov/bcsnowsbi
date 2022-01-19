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

#' Script for calculating the SBI for one basin. This is the function called within a lapply loop to go over multiple basins.
#' Meant to be called after get_SBI_data() within "SWE_SBI_getdata_function.R".
#' This function will call the SBI_function() below to calculate the SBI for a particular basin.
#'
#' @param data dataframe of sites that you are looking to calculate the SBI for
#' @param basins_initial initial basins you want to calculate SBI for
#' @param date_sbi Date of SBI
#' @param SBI_data Site data that you are using to calculate SBI for
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

SBI_sites_function <- function(basins_initial, date_sbi, SBI_data) {

  stats_data <- SBI_data %>%
    dplyr::filter(basin == basins_initial) # Select only the sites within a basin

  # Calculate
  SBI <- SBI_function(data = stats_data,
                      basins = basins_initial,
                      date_sbi = date_sbi) %>%
    dplyr::mutate(basin = as.character(basins_initial))

  return(list(SBI = SBI))
}
