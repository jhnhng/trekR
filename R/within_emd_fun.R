#' Calculates the Earth-Mover's Distance (EMD) within the same range
#'
#' @param t List of rasters containing the raster for each period within the
#   specified interval.
#' @param nintervals The length that the rasters are broken up into by the
#'  specified interval.
#'
#' @return The output is a list of matrices that contain the within- range EMDs

#' @export
# Calculates the geographical emd within a timeframe
#  t:
# nintervals: the number that the timeframe is dividing into (e.g., within a
# 10-day interval for a single month the time frame is divided into '3' weeks)
within_emd <- function(t) {
  id <- lapply(list(t),\(x)split(unlist(x), names(x)));
  Map(
    function(subl1) {
      outer(unlist(subl1), unlist(subl1),
            FUN = Vectorize(function(x, y) {
              if (inherits(x, "RasterStack")) {
                emd_env(x, y)
              } else {
                emd_geo(x, y)
              }
            })
      )
    },
    split(id[[1]], ceiling(seq_along(id[[1]])))
  )
}
