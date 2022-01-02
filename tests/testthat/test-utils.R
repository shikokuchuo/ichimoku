xtsobject <- ichimoku(sample_ohlc_data)[1:10, ]

test_that("tradingDays ok", {
  expect_vector(tradingDays(as.POSIXct(c("2021-02-01", "2021-02-02", "2021-02-03"))), ptype = logical(), size = 3)
  expect_warning(expect_true(tradingDays(as.POSIXct("2021-02-02"), holidays = 0)), regexp = "holidays are invalid")
  expect_true(all(tradingDays(as.POSIXct(c("2021-02-01", "2021-02-02", "2021-02-03")))))
  expect_true(all(tradingDays(as.POSIXct(c("2021-01-01", "2021-01-02")), holidays = NULL)))
  expect_false(all(tradingDays(as.POSIXct(c("2021-01-01", "2021-01-02", "2021-01-03")))))
})

test_that("grid_dup ok", {
  expect_identical(grid_dup(3), c(4, 7, 8))
  expect_identical(grid_dup(3, omit.id = TRUE), c(4, 7, 8, 1, 5, 9))
})

test_that("xts_df ok", {
  expect_s3_class(df <- xts_df(xtsobject), "data.frame")
  expect_equal(dim(df), c(10L, 13L))
  expect_identical(attr(xts_df(structure(xtsobject, special = "set"), keep.attrs = TRUE), "special"), "set")
})

test_that("matrix_df ok", {
  expect_s3_class(df <- matrix_df(as.matrix(xtsobject)), "data.frame")
  expect_identical(dim(df), c(10L, 12L))
  mat <- structure(as.matrix(xtsobject), special = "set")
  expect_identical(attr(matrix_df(mat, keep.attrs = TRUE), "special"), "set")
})

test_that("df_merge ok", {
  expect_identical(dim(merge <- df_merge(sample_ohlc_data[1:6, ], sample_ohlc_data[4:10, ])), c(10L, 6L))
  attr(merge, "oanda") <- TRUE
  attr(merge, "timestamp") <- .POSIXct(1)
  merge$complete <- FALSE
  expect_warning(df_merge(merge, merge), regexp = "Incomplete periods")
})

test_that("df_append ok", {
  expect_identical(dim(df_append(old <- sample_ohlc_data[1:6, ], new <- sample_ohlc_data[4:10, ])), c(10L, 6L))
  attr(new, "timestamp") <- .POSIXct(1)
  expect_identical(attr(df_append(old, new), "timestamp"), attr(new, "timestamp"))
  attr(new, "special") <- "test"
  expect_identical(attr(df_append(old, new, keep.attr = "special"), "special"), attr(new, "special"))
})

test_that("internal window functions ok", {
  expect_identical(.Call(ichimoku_wmin, as.numeric(1:6), 3L), c(NA, NA, 1, 2, 3, 4))
  expect_identical(.Call(ichimoku_wmax, as.numeric(1:6), 3L), c(NA, NA, 3, 4, 5, 6))
  expect_identical(.Call(ichimoku_wmean, as.numeric(1:6), 3L), c(NA, NA, 2, 3, 4, 5))
})
