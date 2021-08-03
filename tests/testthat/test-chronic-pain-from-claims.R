Hn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)

testthat::test_that("The algorithm for chronic pain works as expected with two codes", {
  ### Codes need to be >30 days apart to be detected
  for (c in .multimorbidity_codes()[["cpain"]]) {
    Cy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - c(1, 61), code = c)
    Cn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - c(1, 2), code = c)
    outy <- multimorbidity(data_hospitalisations = Hn, data_claims = Cy, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy[["cpain"]], expected = 1)
    testthat::expect_equal(object = outn[["cpain"]], expected = 0)
  }
})

testthat::test_that("The algorithm for chronic pain works as expected with three codes", {
  ### What if we have three codes?
  for (c in .multimorbidity_codes()[["cpain"]]) {
    Cy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - c(1, 61, 121), code = c)
    Cn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - c(1, 2, 3), code = c)
    outy <- multimorbidity(data_hospitalisations = Hn, data_claims = Cy, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy[["cpain"]], expected = 1)
    testthat::expect_equal(object = outn[["cpain"]], expected = 0)
  }
})

testthat::test_that("The algorithm for chronic pain works as expected with a mix of codes", {
  ### What if we have a mix?
  for (c in .multimorbidity_codes()[["cpain"]]) {
    Cy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - c(1, 2, 61), code = c)
    Cn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - c(1, 2), code = c)
    outy <- multimorbidity(data_hospitalisations = Hn, data_claims = Cy, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy[["cpain"]], expected = 1)
    testthat::expect_equal(object = outn[["cpain"]], expected = 0)
  }
})
