cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9, 26, 52))
strat <- strat(cloud)
xtsobject <- xts(sample_ohlc_data[, -1], order.by = sample_ohlc_data[, 1])
mobject <- as.matrix(xtsobject)
charobject <- "sample_ohlc_data"

test_that("ichimoku object specification correct", {
  expect_s3_class(expect_s3_class(expect_s3_class(cloud, "ichimoku"), "xts"), "zoo")
  dims <- dim(cloud)
  expect_true(dims[1L] == 281L)
  expect_true(dims[2L] == 12L)
  names <- c("open", "high", "low", "close", "cd", "tenkan", "kijun", "senkouA",
             "senkouB", "chikou", "cloudT", "cloudB")
  expect_identical(names, dimnames(cloud)[[2L]])
  expect_s3_class(index(cloud), "POSIXct")
})

test_that("ichimoku methods correct", {
  expect_identical(cloud, ichimoku(cloud))
  expect_identical(cloud, ichimoku(xtsobject, ticker = "TKR"))
  expect_identical(cloud, ichimoku(mobject, ticker = "TKR"))
  expect_identical(cloud, ichimoku(charobject, ticker = "TKR"))
  expect_identical(ichimoku(sample_ohlc_data), ichimoku(charobject))
})

test_that("ichimoku handles higher frequency data", {
  data <- sample_ohlc_data
  data$time <- seq.POSIXt(from = as.POSIXct("2020-01-01"), by = "1 hour", length.out = 256)
  cloudhf <- ichimoku(data)
  expect_s3_class(cloudhf, "ichimoku")
  expect_s3_class(autoplot(cloudhf), "ggplot")
})

test_that("ichimoku keep.data ok", {
  expect_true(dim(ichimoku(sample_ohlc_data, keep.data = TRUE))[2L] == 13L)
  expect_true(dim(ichimoku(xtsobject, keep.data = TRUE))[2L] == 13L)
  expect_true(dim(ichimoku(mobject, keep.data = TRUE))[2L] == 13L)
  expect_identical(cloud,
                   ichimoku(sample_ohlc_data[, -6L], ticker = "TKR", keep.data = TRUE))
  expect_identical(cloud,ichimoku(cloud, keep.data = TRUE))
  expect_identical(ichimoku(cloud, ticker = "ticker"),
                   ichimoku(cloud, ticker = "ticker", keep.data = TRUE))
})

test_that("ichimoku error handling ok", {
  expect_error(ichimoku(autoplot(cloud)), regexp = "cannot create")
  expect_error(ichimoku("nonexistent", regexp = "not found"))
  recursive <- "recursive"
  expect_error(ichimoku(recursive, regexp = "character"))
  expect_error(ichimoku("recursive", regexp = "character"))
  expect_error(ichimoku(data.frame(time = 1:10)), regexp = "not convertible")
  expect_error(ichimoku(sample_ohlc_data[-1]), regexp = "Valid date-time")
  expect_error(ichimoku(sample_ohlc_data[1, ]), regexp = "longer than")
  expect_error(ichimoku(sample_ohlc_data[, -5]), regexp = "data not found")
  expect_warning(ichimoku(sample_ohlc_data[, -3]), regexp = "pseudo-OHLC data")
  expect_warning(ichimoku(sample_ohlc_data[, -2]), regexp = "Opening prices")
  expect_warning(ichimoku(sample_ohlc_data, periods = c(8, 15)), regexp = "cloud periods invalid")
})

test_that("is.ichimoku ok", {
  expect_true(is.ichimoku(cloud))
  expect_false(is.ichimoku(sample_ohlc_data))
})

test_that("print.ichimoku print method ok", {
  expect_output(expect_s3_class(print(cloud), "ichimoku"))
  expect_output(expect_s3_class(print(cloud, plot = FALSE), "ichimoku"))
})
