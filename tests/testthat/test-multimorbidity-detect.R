# ### Test that codes are detected
# for (mi in seq_along(.multimorbidity_codes())) {
#   for (c in .multimorbidity_codes()[[mi]]) {
#     hm <- .multimorbidity_howmany()[[mi]]
#     Hy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["hospitalisations"]), code = c)
#     Hn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["hospitalisations"]), code = "NOTaCODE")
#     if (is.na(hm["claims"])) {
#       Cy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, code = c)
#       Cn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - 1, code = "NOTaCODE")
#     } else {
#       Cy <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["claims"]), code = c)
#       Cn <- data.frame(id = 1, index_date = Sys.Date(), date = Sys.Date() - seq(hm["claims"]), code = "NOTaCODE")
#     }
#     outyy <- multimorbidity(data_hospitalisations = Hy, data_claims = Cy, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
#     outyn <- multimorbidity(data_hospitalisations = Hy, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
#     outny <- multimorbidity(data_hospitalisations = Hn, data_claims = Cy, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
#     outnn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, id = "id", code = "code", date = "date", index_date = "index_date", combine_cirrhosis = FALSE)
#     w <- names(.multimorbidity_codes())[mi]
#     testthat::expect_equal(object = outyy[[w]], expected = 1)
#     testthat::expect_equal(object = outyn[[w]], expected = 1)
#     testthat::expect_equal(object = outny[[w]], expected = ifelse(is.na(hm["claims"]), 0, 1)[[1]])
#     testthat::expect_equal(object = outnn[[w]], expected = 0)
#   }
# }
#
# ### Codes are detected if variables have random names too
# .sample_name <- function(length = 10) {
#   s <- sample(x = c(letters, LETTERS, seq(9)), size = length, replace = TRUE)
#   s <- paste(s, collapse = "")
#   s <- make.names(s)
#   s
# }
# for (mi in seq_along(.multimorbidity_codes())) {
#   for (c in .multimorbidity_codes()[[mi]]) {
#     hm <- .multimorbidity_howmany()[[mi]]
#     Hy <- Hn <- data.frame(.tmp = rep(1, hm["hospitalisations"]))
#     if (is.na(hm["claims"])) {
#       Cy <- Cn <- data.frame(.tmp = 1)
#     } else {
#       Cy <- Cn <- data.frame(.tmp = rep(1, hm["claims"]))
#     }
#     .id <- .sample_name()
#     .code <- .sample_name()
#     .date <- .sample_name()
#     .index_date <- .sample_name()
#     Hy[[.id]] <- 1
#     Hn[[.id]] <- 1
#     Cy[[.id]] <- 1
#     Cn[[.id]] <- 1
#     Hy[[.index_date]] <- Sys.Date()
#     Hn[[.index_date]] <- Sys.Date()
#     Cy[[.index_date]] <- Sys.Date()
#     Cn[[.index_date]] <- Sys.Date()
#     Hy[[.date]] <- Sys.Date() - seq(hm["hospitalisations"])
#     Hn[[.date]] <- Sys.Date() - seq(hm["hospitalisations"])
#     if (is.na(hm["claims"])) {
#       Cy[[.date]] <- Sys.Date() - 1
#       Cn[[.date]] <- Sys.Date() - 1
#     } else {
#       Cy[[.date]] <- Sys.Date() - seq(hm["claims"])
#       Cn[[.date]] <- Sys.Date() - seq(hm["claims"])
#     }
#     Hy[[.code]] <- c
#     Hn[[.code]] <- "NOTaCODE"
#     Cy[[.code]] <- c
#     Cn[[.code]] <- "NOTaCODE"
#     outyy <- multimorbidity(data_hospitalisations = Hy, data_claims = Cy, id = .id, code = .code, date = .date, index_date = .index_date, combine_cirrhosis = FALSE)
#     outyn <- multimorbidity(data_hospitalisations = Hy, data_claims = Cn, id = .id, code = .code, date = .date, index_date = .index_date, combine_cirrhosis = FALSE)
#     outny <- multimorbidity(data_hospitalisations = Hn, data_claims = Cy, id = .id, code = .code, date = .date, index_date = .index_date, combine_cirrhosis = FALSE)
#     outnn <- multimorbidity(data_hospitalisations = Hn, data_claims = Cn, id = .id, code = .code, date = .date, index_date = .index_date, combine_cirrhosis = FALSE)
#     w <- names(.multimorbidity_codes())[mi]
#     testthat::expect_equal(object = outyy[[w]], expected = 1)
#     testthat::expect_equal(object = outyn[[w]], expected = 1)
#     testthat::expect_equal(object = outny[[w]], expected = ifelse(is.na(hm["claims"]), 0, 1)[[1]])
#     testthat::expect_equal(object = outnn[[w]], expected = 0)
#   }
# }
