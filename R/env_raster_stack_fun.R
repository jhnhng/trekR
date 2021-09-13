# Creating raster stacks with the home range estimate and the environmental
# variables
# hrr: home range rasters
env_raster_stack <- function(hrr) {
  # Downloads the summer raster ---
  env <- vector("list", length(hrr))
  for (i in 1:length(hrr)) {
    env[[i]] <- get_elev_raster(hrr[[i]], z = 9)
  }

  # Resamples the elevation rasters to the same extend as the locohs
  rs <- vector("list", length(hrr))
  for (i in 1:length(hrr)) {
    rs[[i]] <- resample(env[[i]], hrr[[i]])
  }

  # Create Raster stacks
  rst <- vector("list", length(hrr))
  for (i in 1:length(hrr)) {
    rst[[i]] <- raster::stack(hrr[[i]], rs[[i]])
    names(rst[[i]]) <- c("LoCoH", "Elevation")
  }

  return(rst)
}
