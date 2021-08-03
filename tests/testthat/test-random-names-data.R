testthat::test_that("Datasets with random names still work", {
  for (i in seq(50)) {
    allc <- .multimorbidity_codes()
    allc <- unlist(allc)
    Cdf <- data.frame(id = 1, code = "NOTaCODE", index_date = Sys.Date(), date = Sys.Date() - 1)
    Hdf <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, code = sample(x = allc, size = 5))
    out1 <- multimorbidity(data_hospitalisations = Hdf, data_claims = Cdf, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
    out1[["id"]] <- NULL

    .id <- .sample_name()
    .code <- .sample_name()
    .index_date <- .sample_name()
    .date <- .sample_name()
    names(Cdf)[names(Cdf) == "id"] <- .id
    names(Cdf)[names(Cdf) == "code"] <- .code
    names(Cdf)[names(Cdf) == "date"] <- .date
    names(Cdf)[names(Cdf) == "index_date"] <- .index_date
    names(Hdf)[names(Hdf) == "id"] <- .id
    names(Hdf)[names(Hdf) == "code"] <- .code
    names(Hdf)[names(Hdf) == "date"] <- .date
    names(Hdf)[names(Hdf) == "index_date"] <- .index_date

    out2 <- multimorbidity(data_hospitalisations = Hdf, data_claims = Cdf, id = .id, code = .code, date = .date, index_date = .index_date, combine_cirrhosis = FALSE)
    out2[[.id]] <- NULL

    testthat::expect_equal(object = out2, expected = out1)
  }
})
