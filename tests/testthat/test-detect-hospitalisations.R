testthat::test_that("Codes for hospitalisations are detected properly", {
  Dn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, atc = "NOTaDRUG", npacks = 1, stringsAsFactors = F)
  Cn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1, stringsAsFactors = F)
  for (mi in seq_along(.multimorbidity_codes())) {
    for (c in .multimorbidity_codes()[[mi]]) {
      hm <- .multimorbidity_howmany()[[mi]]
      Hy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["hospitalisations"]), code = c, stringsAsFactors = F)
      Hn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["hospitalisations"]), code = "NOTaCODE", stringsAsFactors = F)
      outy <- multimorbidity(data_hospitalisations = Hy, data_claims = Cn, data_drugs = Dn, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
      outn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, data_drugs = Dn, id = "id", code = "code", atc = "atc", npacks = "npacks", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
      w <- names(.multimorbidity_codes())[mi]
      testthat::expect_equal(object = outy[[w]], expected = 1)
      testthat::expect_equal(object = outn[[w]], expected = 0)
    }
  }
})
