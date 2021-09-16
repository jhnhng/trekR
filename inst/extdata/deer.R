set.seed(12345)

library(lubridate)

ID = rep(c("A","B","C"), 1095)
date = rep_len(seq(dmy("01-01-2013"), dmy("31-12-2013"), by = "days"), 365)
utm_x = runif(length(ID), min = 662230.69 , max = 680000 )
utm_y = runif(length(ID), min = 4098705.72, max = 4101000.22)

deer <- data.frame(ID = as.factor(ID),date, utm_x, utm_y)


deer$month <- month(deer$date)

deer$jDate <- julian(deer$date)


