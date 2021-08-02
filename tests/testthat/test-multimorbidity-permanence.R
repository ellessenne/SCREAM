### Test that permanence is applied properly
for (c in seq_along(.multimorbidity_years())) {
  .loc <- .multimorbidity_codes()[[c]]
  .condition <- names(.multimorbidity_codes())[c]
  for (i in seq_along(.loc)) {
    # First, codes outside range are removed
    index_date <- sample(x = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1), size = 1)
    df_c <- data.frame(id = 1, code = "NOTaCODE", index_date = index_date, date = index_date - 1)
    if (is.null(.multimorbidity_years()[[c]])) {
      df_h <- data.frame(id = 1, code = rep(.loc[i], .multimorbidity_howmany()[[c]]["hospitalisations"]), index_date = index_date, date = index_date - seq(.multimorbidity_howmany()[[c]]["hospitalisations"]))
    } else {
      df_h <- data.frame(id = 1, code = rep(.loc[i], .multimorbidity_howmany()[[c]]["hospitalisations"]), index_date = index_date, date = index_date - 400 * .multimorbidity_years()[[c]] * seq(.multimorbidity_howmany()[[c]]["hospitalisations"]))
    }
    out <- multimorbidity(data_hospitalisations = df_h, data_claims = df_c, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    if (is.null(.multimorbidity_years()[[c]])) {
      testthat::expect_equal(object = out[[.condition]], expected = 1)
    } else {
      testthat::expect_equal(object = out[[.condition]], expected = 0)
    }
    # Then, within the time range are still counted
    index_date <- sample(x = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1), size = 1)
    df_h <- data.frame(id = 1, code = rep(.loc[i], .multimorbidity_howmany()[[c]]["hospitalisations"]), index_date = index_date, date = index_date - seq(.multimorbidity_howmany()[[c]]["hospitalisations"]))
    out <- multimorbidity(data_hospitalisations = df_h, data_claims = df_c, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = out[[.condition]], expected = 1)
  }
}
