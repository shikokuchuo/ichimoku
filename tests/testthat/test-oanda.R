test_that("oanda functions error handling ok", {
  expect_error(oanda(), regexp = "must be specified")
  expect_error(oanda_stream(), regexp = "must be specified")
  expect_error(oanda_chart(), regexp = "must be specified")
})

test_that("oanda functions ok", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  expect_error(oanda("USDJPY", apikey = NULL))
  expect_error(suppressWarnings(suppressMessages(
    oanda_chart("USDJPY", refresh = 0, count = 10, periods = 10, apikey = NULL))))
  expect_error(suppressWarnings(suppressMessages(
    app <- oanda_studio(refresh = 0, count = 10, periods = 10, apikey = NULL))))
  expect_failure(expect_silent(oanda_stream("USD_JPY", apikey = NULL)))
})
