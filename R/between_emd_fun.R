# t1 and t2 refer to the time frames that the emd is being calculated for.
between_emd <- function(t1, t2, nintervals) {
  source("Functions/emd_geo_fun.R")
  # Applies the 'emd_geo' function on the intervals for the two time frames.
  # e.g., let's say the two frames are winter and summer, and each season is
  # dividing into 10-day intervals. The function applies 'emd_geo' between the
  # first 10-day interval of summer to the first, second, and third 10-day intervals
  # of winter. This is repeated for the second 10-day interval, and the
  # third 10-day interval of summer.
  Map(
    function(x, y) {
      outer(unlist(x), unlist(y), FUN = Vectorize(function(p, q) {
        if (inherits(p, "RasterLayer")) {
          emd_geo(p, q)
        } else {
          emd_env(p, q)
        }
      }))
    },
    split(t1, ceiling(seq_along(sum_raster) / nintervals)),
    split(t2, ceiling(seq_along(win_raster) / nintervals)) # nintervals is 3 in
    # the example.
  )
}
