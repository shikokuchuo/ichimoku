test_that("oanda functions error handling ok", {
  expect_error(oanda(), regexp = "must be specified")
  expect_error(oanda_stream(), regexp = "must be specified")
  expect_error(oanda_chart(), regexp = "must be specified")
})

with_mock_api({
  test_that("oanda ok", {
    datam <- oanda("USD_JPY", granularity = "M", count = 2, price = "B", apikey = NULL)
    datad <- oanda("USD_JPY", granularity = "D", count = 2, price = "M", apikey = NULL)
    datah <- oanda("USD_JPY", granularity = "H1", count = 2, price = "A", apikey = NULL)
    expect_s3_class(datam, "data.frame")
    expect_s3_class(datad, "data.frame")
    expect_s3_class(datah, "data.frame")
    expect_type(look(datam), "list")
    expect_type(look(datad), "list")
    expect_type(look(datah), "list")
    expect_length(look(datam), 3)
    expect_length(look(datad), 3)
    expect_length(look(datah), 3)
    expect_s3_class(oanda("USD_JPY", granularity = "H1", count = 2, price = "A",
                          apikey = NULL, .validate = FALSE), "data.frame")
    expect_warning(df_merge(datam, datam), regexp = "Incomplete periods")
    })
})

with_mock_api({
  test_that("oanda_stream ok", {
    expect_error(suppressMessages(oanda_stream("USD_JPY", apikey = NULL)))
  })
})

with_mock_api({
  test_that("oanda_chart ok", {
    expect_error(
      suppressWarnings(
        suppressMessages(
          oanda_chart("USD_JPY", refresh = 0, count = 10, periods = 10, apikey = NULL)
          )))
  })
})

with_mock_api({
  test_that("oanda helpers ok", {
    expect_s3_class(oanda_accounts(apikey = NULL), "data.frame")
    expect_s3_class(oanda_instruments(apikey = NULL), "data.frame")
  })
})

with_mock_api({
  test_that("oanda studio ok", {
    appobj <- oanda_studio(apikey = NULL)
    expect_s3_class(appobj, "shiny.appobj")
    tryCatch(shiny::testServer(appobj), error = function(e) TRUE)
    expect_message(oanda_studio(refresh = 0, apikey = NULL))
  })
})
