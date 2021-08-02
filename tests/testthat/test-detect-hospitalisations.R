### Test that codes are detected for hospitalisations
Cn <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
for (mi in seq_along(.multimorbidity_codes())) {
  for (c in .multimorbidity_codes()[[mi]]) {
    hm <- .multimorbidity_howmany()[[mi]]
    Hy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["hospitalisations"]), code = c)
    Hn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["hospitalisations"]), code = "NOTaCODE")
    outy <- multimorbidity(data_hospitalisations = Hy, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    outn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    w <- names(.multimorbidity_codes())[mi]
    testthat::expect_equal(object = outy[[w]], expected = 1)
    testthat::expect_equal(object = outy[[w]], expected = 1)
  }
}
