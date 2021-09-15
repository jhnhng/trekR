#' Creates rasters from the LoCoH isopleths
#'
#' @param isopleth A isopleth created from LoCoH home range estimates.
#' @param resolution Specify a resolution of the resulting raster.
#'
#' @return The output is a list of rasters for individual each within a given
#'  interval time frame.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom raster raster rasterize values cellStats

#' @export
iso2raster <- function(isopleth, resolution) {
  # Creates empty list
  template_raster <- vector("list", length(isopleth))
  raster_locoh <- vector("list", length(isopleth))
  x <- vector("list", length(isopleth))

  # Settings for the progress bar
  pb <- txtProgressBar(min = 0, max = length(isopleth), style = 3)

  # Iteration to rasterize isopleths
  for (i in 1:length(isopleth)) {
    # Progress bar
    Sys.sleep(0.001)
    setTxtProgressBar(pb, i)

    # Creates template raster
    template_raster[[i]] <- raster(isopleth[[i]],
      resolution = resolution, vals = 0,
      crs = sp::CRS(sf::st_crs(isopleth[[i]])[[2]])
    )
    # Rasterizes the isopleth
    raster_locoh[[i]] <- rasterize(isopleth[[i]],
      template_raster[[i]],
      field = "level",
      fun = "first"
    )

    # Now you want to subtract all the isopleth values from 1
    values(raster_locoh[[i]]) <- 1 - values(raster_locoh[[i]])

    # Now normalize
    x[[i]] <- raster_locoh[[i]] / cellStats(raster_locoh[[i]], sum)
  }
  return(x)
  close(pb)
}
