#' Calculates the between seasonal range Earth-mover's distance (EMD)
#'
#' @param t1 A list of rasters for each interval for a given range.
#' @param t2 A second list of raster for each interval for a given range that
#'  is being compared with the first list of rasters (t1).
#'
#' @return The output is a list of matrices that contain the EMDs for each
#'  individual in a given year comparing different intervals.
#'
#'

#' @export
# t1 and t2 refer to the time frames that the emd is being calculated for.
between_emd <- function(t1, t2) {
    # Applies the 'emd_geo' function on the intervals for the two time frames.
    # e.g., let's say the two frames are winter and summer, and each season is
    # dividing into 10-day intervals. The function applies 'emd_geo' between the
    # first 10-day interval of summer to the first, second, and third 10-day intervals
    # of winter. This is repeated for the second 10-day interval, and the
    # third 10-day interval of summer.
    id <- lapply(list(t1, t2),function(x)split(unlist(x), names(x)));

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
      split(id[[1]], ceiling(seq_along(id[[1]]))),
      split(id[[2]], ceiling(seq_along(id[[2]])))
    )


}
