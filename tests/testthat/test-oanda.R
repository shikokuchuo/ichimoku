test_that("oanda functions error handling ok", {
  expect_error(oanda(), regexp = "must be specified")
  expect_error(oanda("USD_JPY", apikey = NULL, from = "a", to = "b"), regexp = "value of 'from'")
  expect_error(oanda("USD_JPY", apikey = NULL, from = "2021-01-01", to = "b"), regexp = "value of 'to'")
  expect_error(oanda("USD_JPY", apikey = NULL, from = "2021-01-01", to = "2020-01-01"), regexp = "time period invalid")
  expect_error(oanda("USD_JPY", apikey = NULL, from = "a"), regexp = "value of 'from'")
  expect_error(oanda("USD_JPY", apikey = NULL, to = "b"), regexp = "value of 'to'")
  expect_error(oanda_stream(), regexp = "must be specified")
  expect_error(oanda_chart(), regexp = "must be specified")
})

test_that("oanda switch ok", {
  expect_message(expect_invisible(oanda_switch()), regexp = "switched to 'live'")
  expect_message(expect_invisible(oanda_switch()), regexp = "switched to 'practice'")
})
