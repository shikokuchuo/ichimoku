xtsobject <- ichimoku(sample_ohlc_data)[1:10, ]

test_that("tradingDays ok", {
  expect_true(tradingDays(sample_ohlc_data$time[2], holidays = NULL))
  expect_vector(tradingDays(sample_ohlc_data$time[1:3]), ptype = logical(), size = 3)
  expect_warning(tradingDays(sample_ohlc_data$time[4], holidays = 0), regexp = "holidays are invalid")
  expect_true(all(tradingDays(as.POSIXct(c("2021-01-01", "2021-01-02", "2021-01-03")),
                              noholidays = TRUE)))
  expect_false(all(tradingDays(as.POSIXct(c("2021-01-01", "2021-01-02", "2021-01-03")))))
})

test_that("grid_dup ok", {
  expect_identical(grid_dup(3), c(4, 7, 8))
  expect_identical(grid_dup(3, omit.id = TRUE), c(4, 7, 8, 1, 5, 9))
})

test_that("xts_df ok", {
  df <- xts_df(xtsobject)
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(10L, 13L))
  xts <- structure(xtsobject, special = "set")
  dfmod <- xts_df(xts, keep.attrs = TRUE)
  expect_equal(attr(dfmod, "special"), "set")
})

test_that("matrix_df ok", {
  df <- matrix_df(as.matrix(xtsobject))
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(10L, 12L))
  mat <- structure(as.matrix(xtsobject), special = "set")
  dfmod <- matrix_df(mat, keep.attrs = TRUE)
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

test_that("internal window functions ok", {
  expect_equal(minOver(as.numeric(1:6), 3), c(NA, NA, 1, 2, 3, 4))
  expect_equal(maxOver(as.numeric(1:6), 3), c(NA, NA, 3, 4, 5, 6))
  expect_equal(meanOver(as.numeric(1:6), 3), c(NA, NA, 2, 3, 4, 5))
})
