testthat::test_that("Permanence of codes from claims data", {
  df_d <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = "NOTaDRUG", npacks = 1)
  ### Test that permanence is applied properly
  ### Only applies to claims data, actually
  for (c in seq_along(.multimorbidity_years())) {
    .loc <- .multimorbidity_codes()[[c]]
    .condition <- names(.multimorbidity_codes())[c]
    if (.condition == "cpain") next
    for (i in seq_along(.loc)) {
      # First, codes outside range are removed
      index_date <- sample(x = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1), size = 1)
      df_h <- data.frame(id = 1, code = "NOTaCODE", index_date = index_date, date = index_date - 1)
      hm <- max(1, .multimorbidity_howmany()[[c]]["claims"], na.rm = TRUE)
      if (is.null(.multimorbidity_years()[[c]])) {
        df_c <- data.frame(id = 1, code = rep(.loc[i], hm), index_date = index_date, date = index_date - seq(hm))
      } else {
        df_c <- data.frame(id = 1, code = rep(.loc[i], hm), index_date = index_date, date = index_date - 400 * .multimorbidity_years()[[c]] * seq(hm))
      }
      out <- multimorbidity(data_hospitalisations = df_h, data_claims = df_c, data_drugs = df_d, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
      if (is.na(.multimorbidity_howmany()[[c]]["claims"])) {
        # if cannot be detected via claims, must be a zero here
        testthat::expect_equal(object = out[[.condition]], expected = 0)
      } else {
        if (is.null(.multimorbidity_years()[[c]])) {
          testthat::expect_equal(object = out[[.condition]], expected = 1)
        } else {
          testthat::expect_equal(object = out[[.condition]], expected = 0)
        }
      }
      # Then, within the time range are still counted
      index_date <- sample(x = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1), size = 1)
      df_c <- data.frame(id = 1, code = rep(.loc[i], hm), index_date = index_date, date = index_date - seq(hm))
      out <- multimorbidity(data_hospitalisations = df_h, data_claims = df_c, data_drugs = df_d, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)

      if (is.na(.multimorbidity_howmany()[[c]]["claims"])) {
        # if cannot be detected via claims, must be a zero here
        testthat::expect_equal(object = out[[.condition]], expected = 0)
      } else {
        testthat::expect_equal(object = out[[.condition]], expected = 1)
      }
    }
  }
})
