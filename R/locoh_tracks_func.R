locoh_tracks <- function(tracks, n) {
  # Sets the length of the progress bar
  pb <- txtProgressBar(min = 0, max = length(tracks), style = 3)
  # Creates a empty list for the locohs
  locoh <- vector("list", n)

  for (i in 1:length(tracks)) {
    # Progress bar
    Sys.sleep(0.001)
    setTxtProgressBar(pb, i)

    # Calculate LoCoH a*
    dmat <- dist(tracks[[i]][, c("x_", "y_")])
    a <- max(dmat)

    # Fit LoCoH
    locoh[[i]] <- hr_locoh(
      x = tracks[[i]],
      levels = seq(0.1, 1, by = 0.1),
      keep.data = TRUE,
      n = a,
      type = "a",
      rand_buffer = 1e-05
    )
  }
  return(locoh) # Returns the locohs in a list of data frames
  close(pb)
}
