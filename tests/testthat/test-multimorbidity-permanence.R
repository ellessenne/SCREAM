### Test that permanence is applied properly
for (c in seq_along(.multimorbidity_permanent())) {
  .loc <- .multimorbidity_codes()[[c]]
  .condition <- names(.multimorbidity_codes())[c]
  for (i in seq_along(.loc)) {
    # First, codes are removed
    index_date <- sample(x = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1), size = 1)
    if (isTRUE(.multimorbidity_permanent()[[c]])) {
      tmp <- data.frame(id = 1, code = .loc[i], index_date = index_date, date = index_date - 1)
    } else {
      tmp <- data.frame(id = 1, code = .loc[i], index_date = index_date, date = index_date - 400 * .multimorbidity_permanent()[[c]])
    }
    out <- multimorbidity(data = tmp, id = "id", code = "code", date = "date", index_date = "index_date")
    if (isTRUE(.multimorbidity_permanent()[[c]])) {
      testthat::expect_equal(object = out[[.condition]], expected = 1)
    } else {
      testthat::expect_equal(object = out[[.condition]], expected = 0)
    }
    # Then, permanent codes within the time range are still counted
    index_date <- sample(x = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1), size = 1)
    tmp <- data.frame(id = 1, code = .loc[i], index_date = index_date, date = index_date - 1)
    out <- multimorbidity(data = tmp, id = "id", code = "code", date = "date", index_date = "index_date")
    testthat::expect_equal(object = out[[.condition]], expected = 1)
  }
}
