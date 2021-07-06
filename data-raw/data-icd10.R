### Simulating a dataset with ICD10 codes
library(comorbidity)
library(tidyverse)

set.seed(329746238)

.N <- 3000
.n <- rpois(n = .N, lambda = 60)
.dates <- seq(as.Date("2015-01-01"), as.Date("2019-12-31"), by = 1)

icd10 <- data.frame(
  id = rep(x = seq(.N), times = .n),
  date = sample(x = .dates, size = sum(.n), replace = TRUE),
  index_date = rep(x = sample(x = .dates, size = .N, replace = TRUE), times = .n),
  code = comorbidity::sample_diag(n = sum(.n)),
  origin = sample(x = seq(3), size = sum(.n), prob = c(0.2, 0.5, 0.4), replace = TRUE)
)

icd10 <- arrange(icd10, id, date) %>%
  filter(date <= index_date) %>%
  group_by(id) %>%
  mutate(n = row_number(), N = n()) %>%
  ungroup()

usethis::use_data(icd10, overwrite = TRUE)
