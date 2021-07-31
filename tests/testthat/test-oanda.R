test_that("oanda functions error handling ok", {
  expect_error(oanda(), regexp = "must be specified")
  expect_error(oanda_stream(), regexp = "must be specified")
  expect_error(oanda_chart(), regexp = "must be specified")
})
