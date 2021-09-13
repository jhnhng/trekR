# Function arguments #

# emd: either the between time frame (e.g., winter season to the summer season)
# emd or the within-time frame (e.g., first 10-days to the second 10-days) emd

# t1 and t2: list of data frames, where each element represents an interval or
# time frame (e.g., first 10-day period to second 10-day period or winter to
# summer)

emspeeds <- function(t1, t2, emd) {

  # Vector of integers using the length of the interval list
  # The intervals refer the the number of days that the data set was broken up
  g1 <- as.integer(gl(length(t1), 3, length(t2)))

  # Function to calculate the number of julian dates between the first date in the
  # first interval and the first date of the second interval
  f1 <- function(.int1, .int2) {
    t(outer(seq_along(.int1), seq_along(.int2),
      FUN = Vectorize(function(i, j) {
        min(.int1[[i]]$jDate) -
          min(.int2[[j]]$jDate)
      })
    ))
  }

  # Creates a matrix calculating the emspeeds between each element
  # Between time frames and within each of the time frames
  # (e.g., within a winter season that has been broken into 10-day intervals;
  # the first 10-days is compared to the first10-days, the second 10-days, the
  # third 10-days. The second to the first 10-days,  the second, and to the
  # the third 10-days, etc.)
  lstMat <- map2(split(t1, g1), split(t2, g1), f1)
  es <- map2(emd, lstMat, `/`)

  # All NAs are converted to 0
  if (all(!is.na(es))) {
    es <- lapply(es, function(x) {
      x[is.na(x)] <- 0
      x
    })

    # # Take the absolute value of each value in the matrix to remove
    # # negative numbers
    es <- lapply(es, abs)
  }
}
