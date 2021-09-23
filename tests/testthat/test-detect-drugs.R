testthat::test_that("Codes for drugs are detected properly: cpain", {
  Cn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  Hn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  for (c in .multimorbidity_drugs_atc()$cpain) {
    hm <- .multimorbidity_drugs_howmany()$cpain
    hy <- .multimorbidity_drugs_years()$cpain
    Dy1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = c, npacks = 1)
    Dy2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = c, npacks = hm)
    Dn1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = "NOTaCODE", npacks = 1)
    Dn2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm) - 366 * hy, atc = c, npacks = 1)
    outy1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outy2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy1[["cpain"]], expected = 1)
    testthat::expect_equal(object = outy2[["cpain"]], expected = 1)
    testthat::expect_equal(object = outn1[["cpain"]], expected = 0)
    testthat::expect_equal(object = outn2[["cpain"]], expected = 0)
  }
})

testthat::test_that("Codes for drugs are detected properly: cpain for people without epilepsy", {
  Cn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  Hn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  for (c in .multimorbidity_drugs_atc()$cpain_no_epilepsy) {
    hm <- .multimorbidity_drugs_howmany()$cpain_no_epilepsy
    hy <- .multimorbidity_drugs_years()$cpain_no_epilepsy
    Dy1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = c, npacks = 1)
    Dy2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = c, npacks = hm)
    Dn1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = "NOTaCODE", npacks = 1)
    Dn2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm) - 366 * hy, atc = c, npacks = 1)
    outy1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outy2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy1[["cpain"]], expected = 1)
    testthat::expect_equal(object = outy2[["cpain"]], expected = 1)
    testthat::expect_equal(object = outn1[["cpain"]], expected = 0)
    testthat::expect_equal(object = outn2[["cpain"]], expected = 0)
  }
})

testthat::test_that("Codes for drugs are detected properly: cpain for people with epilepsy", {
  Cn <- data.frame(id = 1, code = sample(x = .multimorbidity_codes()$epilepsy, size = 1), index_date = Sys.Date(), date = Sys.Date() - seq(.multimorbidity_howmany()$epilepsy["claims"]))
  Hn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  for (c in .multimorbidity_drugs_atc()$cpain_no_epilepsy) {
    hm <- .multimorbidity_drugs_howmany()$cpain_no_epilepsy
    hy <- .multimorbidity_drugs_years()$cpain_no_epilepsy
    Dy1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = c, npacks = 1)
    Dy2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = c, npacks = hm)
    Dn1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = "NOTaCODE", npacks = 1)
    Dn2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm) - 366 * hy, atc = c, npacks = 1)
    outy1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outy2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy1[["cpain"]], expected = 0)
    testthat::expect_equal(object = outy2[["cpain"]], expected = 0)
    testthat::expect_equal(object = outn1[["cpain"]], expected = 0)
    testthat::expect_equal(object = outn2[["cpain"]], expected = 0)
  }
  for (c in .multimorbidity_drugs_atc()$cpain) {
    hm <- .multimorbidity_drugs_howmany()$cpain
    hy <- .multimorbidity_drugs_years()$cpain
    Dy1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = c, npacks = 1)
    Dy2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = c, npacks = hm)
    Dn1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = "NOTaCODE", npacks = 1)
    Dn2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm) - 366 * hy, atc = c, npacks = 1)
    outy1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outy2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy1[["cpain"]], expected = 1)
    testthat::expect_equal(object = outy2[["cpain"]], expected = 1)
    testthat::expect_equal(object = outn1[["cpain"]], expected = 0)
    testthat::expect_equal(object = outn2[["cpain"]], expected = 0)
  }
})

testthat::test_that("Codes for drugs are detected properly: depression", {
  Cn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  Hn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
  for (c in .multimorbidity_drugs_atc()$depression) {
    hm <- .multimorbidity_drugs_howmany()$depression
    hy <- .multimorbidity_drugs_years()$depression
    Dy1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = c, npacks = 1)
    Dy2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = c, npacks = hm)
    Dn1 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm), atc = "NOTaCODE", npacks = 1)
    Dn2 <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm) - 366 * hy, atc = c, npacks = 1)
    outy1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outy2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dy2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn1 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn1, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn2 <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn2, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    testthat::expect_equal(object = outy1[["depression"]], expected = 1)
    testthat::expect_equal(object = outy2[["depression"]], expected = 1)
    testthat::expect_equal(object = outn1[["depression"]], expected = 0)
    testthat::expect_equal(object = outn2[["depression"]], expected = 0)
  }
})
