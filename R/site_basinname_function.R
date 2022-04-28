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

#' Function for getting the snow basin name for a specific site (or all, or all active sites)
#'
#' Inputs are a list of sites,  "All" (that returns the basin name for all of the sites), or "active", which returns the basin name for all the current active sites
#' @param id Station ID that you want to get the basin name for. Defaults to "All", but can also be a string of sites, one site, or "active" sites.
#' @keywords Get basin name by ID
#' @importFrom magrittr %>%
#' @export
#' @examples
#' site_basinname()

site_basinname <- function(id = "All") {

  # Choose your sites
  if (id[1] %in% c("All", "all", "ALL")) {
    sites_i <- unique(c(bcsnowdata::snow_auto_location()$LOCATION_ID, bcsnowdata::snow_manual_location()$LOCATION_ID))

  } else if (id[1] %in% c("Active", "active", "ACTIVE")) { # if the choice is only active sites

    active_ASWE <- bcsnowdata::snow_auto_location() %>%
      dplyr::filter(STATUS == "Active")

    active_manual <- bcsnowdata::snow_manual_location() %>%
      dplyr::filter(STATUS == "Active")

    sites_i <- unique(c(active_ASWE$LOCATION_ID, active_manual$LOCATION_ID))

  } else  {
    # find the basin within the dataframe and return the basin name
    sites_i <- id
  }

  # Associate sites with basins
  # Get the snow basins from the BC Data Catalog
  # Locations of the sites that you want to return
  manual_loc_j <- bcsnowdata::snow_manual_location() %>%
    dplyr::select(LOCATION_ID, LATITUDE, LONGITUDE, geometry)

  aswe_loc_j <- bcsnowdata::snow_auto_location() %>%
    dplyr::select(LOCATION_ID, LATITUDE, LONGITUDE, geometry)

  loc <- rbind(manual_loc_j, aswe_loc_j) %>%
    dplyr::filter(LOCATION_ID %in% sites_i)

  loc <- sf::st_as_sf(loc,
               coords = c("LATITUDE", "LONGITUDE"),
               crs = 4326,
               agr = "constant") %>%
    bcmaps::transform_bc_albers()

  # Get the shape files for the snow basins - Data Catalogue
  #basin_shp <- bcdata::bcdc_query_geodata("9ec01cdb-7085-44fe-b059-9fe5aefb7497") %>%
  #  dplyr::collect()

  url <- "https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/Snow_Basins_Indices_View/FeatureServer/0"
  basin_shp <- esri2sf::esri2sf(url, geomType = "esriGeometryPolygon") %>%
    dplyr::rename(geometry = "geoms") %>%
    as("Spatial") %>%
    sp::spTransform(CRS("+proj=longlat +datum=WGS84")) %>%
    sp::spTransform(CRS("+init=epsg:4326"))

  basin_shp <- sf::st_as_sf(basin_shp, 4326) %>%
    dplyr::mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2)
    )

  basin_shp <- sf::st_make_valid(sf::st_as_sf(basin_shp, 4326)) %>%
    sf::st_transform(4326)

  loc <- sf::st_make_valid(sf::st_as_sf(loc, 4326)) %>%
    sf::st_transform(4326)

  # Put the snow points in the same coordinate system as the snow basins and assign
  loc_sf <- sf::st_join(loc, basin_shp, left = FALSE) %>%
    dplyr::rename(basin = basinName) %>%
    dplyr::arrange(basin) %>%
    dplyr::mutate(basin = ifelse(LOCATION_ID == "1A01P", "Upper Fraser East", basin)) %>%
    dplyr::mutate(basin = ifelse(LOCATION_ID == "1A01", "Upper Fraser East", basin)) %>%
    dplyr::mutate(basin = ifelse(LOCATION_ID == "2E04", NA, basin)) %>%
    dplyr::rename(id = LOCATION_ID) %>%
    dplyr::select(id, basin)

  # ==================
  # Add basins not in the snow map
  # Entire Province
  extra_basins_province <- data.frame(id = sites_i,
                             basin = "Province")

  #Fraser and Columbia Basins
  extra_basins_fc <- data.frame(id = sites_i,
                                    basin = ifelse(substring(sites_i, 1, 1) == "1", "Fraser",  # sites in the Fraser basin start with 1
                                                   ifelse(substring(sites_i, 1, 1) == "2", "Columbia", NA))) %>%
                                      dplyr::filter(!is.na(basin))
  # Nicola
  extra_nicola <- data.frame(id = sites_i,
                             basin = ifelse(sites_i %in% c("1C01",	"1C09",	"1C19",	"1C25",	"1C29",	"2F13",	"2F18",	"2F23",	"2F24"),
                                            "Nicola", NA)) %>%
    dplyr::filter(!is.na(basin))

  #Fraser Plateau
  extra_fp <- data.frame(id = sites_i,
                             basin = ifelse(sites_i %in% c("1C08", "1C22", "1C21"),
                                            "FraserPlateau", NA)) %>%
                dplyr::filter(!is.na(basin))

  # West Road- Chilcotin
  extra_wr <- data.frame(id = sites_i,
                         basin = ifelse(sites_i %in% c("1C08", "1C21", "1C22"),
                                        "Bridge", NA)) %>%
    dplyr::filter(!is.na(basin))

  # Bridge
  extra_lb <- data.frame(id = sites_i,
                         basin = ifelse(sites_i %in% c("1C39", "1C38P", "1C38", "1C40P", "1C40", "1C12P", "1C14P", "1C14", "1C37", "1C05P", "1C05", "1C18P", "1C28"),
                                        "Bridge", NA)) %>%
    dplyr::filter(!is.na(basin))

  # Quesnel
  extra_quesnel <- data.frame(id = sites_i,
                         basin = ifelse(sites_i %in% c("1C33A", "1C13A", "1C17", "1C20P", "1C23", "1C41P"),
                                        "Quesnel", NA)) %>%
    dplyr::filter(!is.na(basin))

  # Lower Thompson
  extra_lt <- data.frame(id = sites_i,
                              basin = ifelse(sites_i %in% c("1C32", "1C09A", "1C19", "1C25", "1C29", "1C29P", "1C01", "1C06",
                                                            "1C42"),
                                             "LowerThompson", NA)) %>%
    dplyr::filter(!is.na(basin))

  # Bind all the extra basins together
  basins_extra_all <- do.call("rbind", list(extra_basins_province, extra_basins_fc, extra_nicola, extra_fp, extra_lb, extra_quesnel, extra_lt)) %>%
    dplyr::rename(LOCATION_ID = id)

  # Add in the locations for the sites
  basins_extra_loc <- dplyr::full_join(basins_extra_all, loc) %>%
    dplyr::select(-LATITUDE, -LONGITUDE) %>%
    dplyr::rename(id = LOCATION_ID)


  # bind to the sites identified by the snow basins
  loc_sf_all <- dplyr::full_join(loc_sf, basins_extra_loc)

  return(loc_sf_all)
}
