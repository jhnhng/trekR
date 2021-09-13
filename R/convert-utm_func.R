# Converts GPS (lat long) locations to UTM Zone 12N locations

convert_utm <- function(latlon_df) {
  utm_df <- latlon_df %>%
    # turns df into sf object with geometry
    st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = 4326
    ) %>% # WGS84 = EPSG 4326
    # converts geometry to UTM 12N projection
    st_transform(crs = 32612) %>% # UTM 12N = EPSG 32612
    # creates a new column of x and y UTM 12N coordinates
    mutate(
      utm_x = st_coordinates(.)[, 1],
      utm_y = st_coordinates(.)[, 2]
    ) %>%
    # drops geometry column
    st_drop_geometry()

  return(utm_df)
}
