# df : the data frame that we want to break into intervals
# season: select the season based on the season_categ column in the data frame
# int : length of the intervals
require(tidyverse)
require(lubridate)

intervals <- function(df, season, int, t) {
  df %>%
    arrange(AnimalID) %>%
    # Creates a new column assigning the first day in the 10-day interval in which
    # the date falls under (e.g., 01-03-2021 would be in the first 10-day interval
    # so the `floor_date` assigned to it would be 01-01-2021)
    mutate(new = floor_date(DT_UTC, int)) %>%
    # For any months that has 31 days, the 31st day would normally be assigned its
    # own interval. The code below takes the 31st day and joins it with the
    # previous interval.
    mutate(new = if_else(day(new) == 31, new - days(10), new)) %>%
    group_by(new, .add = TRUE) %>%
    # Filter the data by the season based on the `season_categ` column
    filter(season_categ == season & year == t) %>%
    group_split()
}
