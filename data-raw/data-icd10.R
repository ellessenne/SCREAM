### Simulating a dataset with ICD10 codes
library(comorbidity)
library(tidyverse)

set.seed(329746238)

.N <- 3000
.n <- rpois(n = .N, lambda = 30)
.dates <- seq(as.Date("2010-01-01"), as.Date("2019-12-31"), by = 1)

icd10 <- data.frame(
  id = rep(x = seq(.N), times = .n),
  date = sample(x = .dates, size = sum(.n), replace = TRUE),
  index_date = rep(x = sample(x = .dates, size = .N, replace = TRUE), times = .n),
  code = comorbidity::sample_diag(n = sum(.n))
)

icd10 <- arrange(icd10, id, date) %>%
  filter(date <= index_date)
icd10$h <- rbinom(n = nrow(icd10), size = 1, prob = 1 / 3)

icd10 <- list(
  hospitalisations = icd10 %>% filter(h == 1) %>% select(-h),
  claims = icd10 %>% select(-h)
)

usethis::use_data(icd10, overwrite = TRUE)
