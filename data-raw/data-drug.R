### Simulating a dataset with example drug data
devtools::load_all()
library(tidyverse)

data(atc, package = "SCREAM")

set.seed(329746238)

.N <- 3000
.n <- rpois(n = .N, lambda = 10)
.dates <- seq(as.Date("2010-01-01"), as.Date("2019-12-31"), by = 1)

drug <- data.frame(
  id = rep(x = seq(.N), times = .n),
  date = sample(x = .dates, size = sum(.n), replace = TRUE),
  code = sample(x = atc$ATC.Code[str_length(atc$ATC.Code) >= 4], size = sum(.n), replace = TRUE),
  npacks = sample(x = seq(4), size = sum(.n), replace = TRUE, prob = c(0.8, 0.1, 0.07, 0.03))
)

drug <- arrange(drug, id, date)

usethis::use_data(drug, overwrite = TRUE)
