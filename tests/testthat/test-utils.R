xtsobject <- ichimoku(sample_ohlc_data)[1:10, ]

test_that("tradingDays ok", {
  expect_true(tradingDays(sample_ohlc_data$time[2], holidays = NULL))
  expect_vector(tradingDays(sample_ohlc_data$time[1:3]), ptype = logical(), size = 3)
  expect_warning(tradingDays(sample_ohlc_data$time[4], holidays = 0))
})

test_that("xts_df ok", {
  df <- xts_df(xtsobject)
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(10L, 13L))
  xts <- structure(xtsobject, special = "set")
  dfmod <- xts_df(xts, preserve.attrs = TRUE)
  expect_equal(attr(dfmod, "special"), "set")
})

test_that("matrix_df ok", {
  df <- matrix_df(as.matrix(xtsobject))
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(10L, 12L))
  mat <- structure(as.matrix(xtsobject), special = "set")
  dfmod <- matrix_df(mat, preserve.attrs = TRUE)
  expect_equal(attr(dfmod, "special"), "set")
})

test_that("df_trim ok", {
  expect_equal(dim(df_trim(data.frame(c(1:4, NA), c(NA, 2:5)))), c(3L, 2L))
})

test_that("df_merge ok", {
  merge <- df_merge(sample_ohlc_data[1:6, ], sample_ohlc_data[4:10, ])
  expect_equal(dim(merge)[1L], 10)
  attr(merge, "oanda") <- TRUE
  attr(merge, "timestamp") <- as.POSIXct("2020-01-01")
  merge$complete <- FALSE
  expect_warning(df_merge(merge, merge), regexp = "Incomplete periods")
})

test_that("df_append ok", {
  expect_equal(dim(df_append(sample_ohlc_data[4:10, ], sample_ohlc_data[1:6, ]))[1L], 10)
})
