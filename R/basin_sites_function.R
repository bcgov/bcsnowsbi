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

#' Function for getting the sites within a specific basin
#'
#' Inputs are which basin you want to return sites for. Defaults to "All" sites
#' @param get_basin Basin that you want to get the snow sites within the basin for. Defaults to "All".
#' @param exceptions Stations that you want to exclude. Defaults to NULL
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples \dontrun{}

basin_sites <- function(get_basin = "All", exceptions = NULL) {

  # ================
  # Associate all of the snow sites with a basin used within the current snow map
  loc_sf <- site_basinname(id = "All")

  # Turn the dataframe around and associate the sites to each basin
  basins <- unique(loc_sf$basin)[!is.na(unique(loc_sf$basin))]

  # function for associating sites by basin - returns list
  basins_flip <- function(basin_in) {

    basin_select <- loc_sf %>%
      dplyr::filter(!(station_id %in% exceptions)) %>% # filter out the exceptions that you don't want to include in the analysis
       dplyr::filter(basin %in% basin_in)

    stations_s <- paste(basin_select$station_id, collapse = ";")

    data_out <- data.frame(basin = basin_in, stations = paste(basin_select$station_id, collapse = ";"))
  }

  # Run function over all basins and unfold the list you create and add the basins that aren't yet on the snow basin map. Also include the province
  list_stations <- lapply(basins, basins_flip)
  sites_first <- do.call("rbind.data.frame", list_stations)

  # Filter by the basin that the user wants to retrieve. Defaults to all.
  if (get_basin %in% c("All", "all", "ALL")) {
    sites_final <- sites_first
  } else {
    sites_final <- sites_first %>%
      dplyr::filter(basin %in% get_basin)
  }

  return(sites_final)
}
