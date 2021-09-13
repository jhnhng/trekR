# Calculates the geographical emd within a timeframe
#  t: list of rasters containing the raster for each period within the
# specified interval (e.g., 10-days)
# nintervals: the number that the timeframe is dividing into (e.g., within a
# 10-day interval for a single month the time frame is divided into '3' weeks)
within_emd <- function(t, nintervals) {
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
    split(t, ceiling(seq_along(t) / nintervals))
  )
}
