#' Creates raster stacks from a environmental covariate layer and home range
#'
#' @param env A environmental raster layer
#' @param hrr A home range that the environemntal covariate is resampled to.
#'
#' @return The ouput is a raster stack that contains the home ranges and the
#'  environmental covariate layer.

# Creating raster stacks with the home range estimate and the environmental
# variables
# hrr: home range rasters

#' @export
env_raster_stack <- function(env, hrr) {
  # Resamples the elevation rasters to the same extend as the locohs
  rs <- vector("list", length(hrr))
  for (i in 1:length(hrr)) {
    rs[[i]] <- raster::resample(env, hrr[[i]])
  }

  # Create Raster stacks
  rst <- vector("list", length(hrr))
  for (i in 1:length(hrr)) {
  rst[[i]] <- raster::stack(hrr[[i]], rs[[i]])
  }

  return(rst)
}
