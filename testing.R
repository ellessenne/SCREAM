library(lme4)
simdf <- readRDS("~/Documents/scream-multimorbidity/data/90-simulated-data.RDS")
fit <- lmer(egfr ~ time + (time | lopnr), data = simdf)
id <- "lopnr"

origin <- as.Date("2006-01-01")
time_scale <- "years"

egfr <- c(120, 90, 60, 45, 30, 15)
