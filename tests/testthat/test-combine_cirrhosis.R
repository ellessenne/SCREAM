### Test that the algorithm for combining cirrhosis works
.codes <- expand.grid(
  .multimorbidity_codes()[["cirrhosis1"]],
  .multimorbidity_codes()[["cirrhosis2"]]
)
for (i in seq(nrow(.codes))) {
  df <- data.frame(id = 1, date = Sys.Date() - 1, index_date = Sys.Date(), code = unlist(.codes[i, ]))
  outT <- multimorbidity(data = df, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = TRUE)
  outF <- multimorbidity(data = df, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
  # Here everything should be '1':
  testthat::expect_equal(object = outT[["cirrhosis"]], expected = 1)
  testthat::expect_equal(object = outF[["cirrhosis1"]], expected = 1)
  testthat::expect_equal(object = outF[["cirrhosis2"]], expected = 1)
}

### However, both are required for it to be tagged as a '1':
.codes <- c(
  .multimorbidity_codes()[["cirrhosis1"]],
  .multimorbidity_codes()[["cirrhosis2"]]
)
for (i in seq_along(.codes)) {
  df <- data.frame(id = 1, date = Sys.Date() - 1, index_date = Sys.Date(), code = .codes[i])
  outT <- multimorbidity(data = df, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = TRUE)
  outF <- multimorbidity(data = df, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
  # Here everything should be '0' except either of 'cirrhosis1', 'cirrhosis2':
  testthat::expect_equal(object = outT[["cirrhosis"]], expected = 0)
  if (df[["code"]] %in% .multimorbidity_codes()[["cirrhosis1"]]) {
    testthat::expect_equal(object = outF[["cirrhosis1"]], expected = 1)
    testthat::expect_equal(object = outF[["cirrhosis2"]], expected = 0)
  } else {
    testthat::expect_equal(object = outF[["cirrhosis1"]], expected = 0)
    testthat::expect_equal(object = outF[["cirrhosis2"]], expected = 1)
  }
}
