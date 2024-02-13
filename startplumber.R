library(plumber)
#r <- plumb("date/plumber.R")
r <- plumb("getLipds/plumber.R")
r$run(port = 8001)
