#' Function for returning elevation by station
#' @export
#' @keywords internal
#' @examples \dontrun{}
elevation_data <- function() {

  manual_data <- bcsnowdata::get_manual_swe(station_id = "All",
                                survey_period = "All",
                                get_year = "All",
                                use_archive = "No") %>%
    dplyr::filter(!is.na(swe_mm)) %>%
    dplyr::arrange(station_id) %>%
    dplyr::select(snow_course_name, station_id, elev_metres) %>%
    dplyr::rename(elevation = elev_metres) %>%
    dplyr::rename(name = snow_course_name) %>%
    dplyr::distinct(station_id, .keep_all = TRUE)

  auto <- bcsnowdata::snow_auto_location() %>%
    dplyr::rename(station_id = LOCATION_ID) %>%
    dplyr::select(station_id, ELEVATION, LOCATION_NAME, geometry) %>%
    dplyr::rename(elevation = ELEVATION, name = LOCATION_NAME)

  stations_el <- dplyr::bind_rows(auto, manual_data)
}
