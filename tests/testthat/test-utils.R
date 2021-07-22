xtsobject <- ichimoku(sample_ohlc_data)[1:10, ]

test_that("tradingDays ok", {
  expect_true(tradingDays(sample_ohlc_data$time[2], holidays = NULL))
  expect_vector(tradingDays(sample_ohlc_data$time[1:3]), ptype = logical(), size = 3)
  expect_warning(tradingDays(sample_ohlc_data$time[4], holidays = 0))
})

test_that("window functions ok", {
  expect_vector(maxOver(sample_ohlc_data$close[1:10], 3), ptype = numeric(), size = 10)
  expect_vector(minOver(sample_ohlc_data$close[1:10], 3), ptype = numeric(), size = 10)
})

test_that("df_trim ok", {
  expect_equal(dim(df_trim(data.frame(c(1:4, NA), c(NA, 2:5)))), c(3L, 2L))
})

test_that("grid_dup ok", {
  expect_equal(grid_dup(3), c(4, 7, 8))
  expect_equal(grid_dup(3, omit.id = TRUE), c(4, 7, 8, 1 , 5, 9))
})

test_that("xts_df ok", {
  df <- xts_df(xtsobject)
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(10L, 13L))
})

test_that("matrix_df ok", {
  df <- matrix_df(as.matrix(xtsobject))
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(10L, 12L))
})

test_that("df_merge ok", {
  expect_equal(dim(df_merge(sample_ohlc_data[1:6, ], sample_ohlc_data[4:10, ]))[1L], 10)
})

test_that("df_append ok", {
  expect_equal(dim(df_append(sample_ohlc_data[1:6, ], sample_ohlc_data[4:10, ]))[1L], 10)
})
