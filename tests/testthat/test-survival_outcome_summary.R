test_that("function survival_outcome_summary returns correct results", {
  data("sample_survival_data", package = "SCREAM")
  res <- survival_outcome_summary("outcome_event",
                                  "tstart_as_Date",
                                  "tstop_as_Date",
                                  "all",
                                  "null_weight",
                                  sample_survival_data,
                                  digit = 4)
  expect_equal(res[1, 6], "85.3089 (77.3459-93.8801)")
})
