cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9, 26, 52))
strat <- strat(cloud)
xtsobject <- xts::xts(sample_ohlc_data[, -1L], order.by = sample_ohlc_data[, 1L])
mobject <- as.matrix(xtsobject)
charobject <- "sample_ohlc_data"
data <- sample_ohlc_data

test_that("ichimoku object specification correct", {
  expect_s3_class(expect_s3_class(expect_s3_class(cloud, "ichimoku"), "xts"), "zoo")
  expect_identical(dim(cloud), c(281L, 12L))
  expect_length(attrs <- attributes(cloud), 7L)
  expect_identical(names(attrs), c("dim", "dimnames", "index", "class", "periods", "periodicity", "ticker"))
  expect_identical(attrs$dimnames[[2L]], c("open", "high", "low", "close", "cd", "tenkan", "kijun", "senkouA", "senkouB", "chikou", "cloudT", "cloudB"))
  expect_identical(names(attributes(attrs$index)), c("tzone", "tclass"))
})

test_that("ichimoku methods correct", {
  expect_identical(cloud, ichimoku(cloud))
  expect_identical(cloud, ichimoku(xtsobject, ticker = "TKR"))
  expect_identical(cloud, ichimoku(mobject, ticker = "TKR"))
  expect_identical(cloud, ichimoku(charobject, ticker = "TKR"))
  expect_identical(ichimoku(sample_ohlc_data), ichimoku(charobject))
})

test_that("ichimoku handles higher frequency data", {
  data$time <- seq.POSIXt(from = .POSIXct(1), by = "1 hour", length.out = 256)
  expect_s3_class(cloudhf <- ichimoku(data), "ichimoku")
  expect_s3_class(autoplot(cloudhf), "ggplot")
  expect_output(str(cloudhf))
  expect_output(summary(cloudhf))
})

test_that("ichimoku keep.data ok", {
  expect_identical(dim(ichimoku(sample_ohlc_data, keep.data = TRUE)), c(281L, 13L))
  expect_identical(dim(ichimoku(xtsobject, keep.data = TRUE)), c(281L, 13L))
  expect_identical(dim(ichimoku(mobject, keep.data = TRUE)), c(281L, 13L))
  expect_identical(cloud,ichimoku(cloud, keep.data = TRUE))
  expect_identical(ichimoku(cloud, ticker = "ticker"), ichimoku(cloud, ticker = "ticker", keep.data = TRUE))
})

test_that("ichimoku error handling ok", {
  expect_error(ichimoku(autoplot(cloud)), regexp = "cannot create")
  expect_error(ichimoku("nonexistent", regexp = "not found"))
  recursive <- "recursive"
  expect_error(ichimoku(recursive, regexp = "character"))
  expect_error(ichimoku("recursive", regexp = "character"))
  expect_error(ichimoku(data.frame(date = letters)), regexp = "not convertible")
  expect_error(ichimoku(sample_ohlc_data[-1L]), regexp = "valid date-time")
  expect_error(ichimoku(sample_ohlc_data[1L, ]), regexp = "longer than")
  expect_error(ichimoku(sample_ohlc_data[, -5L]), regexp = "price data not found")
  data$time <- 1:256
  expect_warning(ichimoku(data), regexp = "numeric values in column")
  data$time <- NULL
  attr(data, "row.names") <- 2:257
  expect_warning(ichimoku(data), regexp = "numeric row names")
  expect_warning(ichimoku(sample_ohlc_data[, -3L]), regexp = "pseudo-OHLC data")
  expect_warning(ichimoku(sample_ohlc_data[, -2L]), regexp = "Opening prices")
  expect_warning(ichimoku(sample_ohlc_data, periods = c(8, 15)), regexp = "cloud periods invalid")
})

test_that("print method ok", {
  expect_output(expect_s3_class(print(cloud), "ichimoku"))
  expect_output(expect_s3_class(print(cloud, plot = FALSE, n = 20), "ichimoku"))
  expect_output(print(cloud[0]))
  expect_output(print(cloud[, 1L, drop = TRUE]))
})

test_that("more ok", {
  expect_null(expect_invisible(more()))
  expect_null(expect_invisible(more(20)))
})

test_that("str method ok", {
  expect_output(expect_null(expect_invisible(str(cloud))), "(281, 12)")
  expect_output(str(cloud[0]), "(0, 12)")
  expect_output(str(cloud[, 1L, drop = TRUE]), "no dimensions")
})

test_that("summary method for objects ok", {
  expect_output(expect_vector(expect_invisible(summary(cloud)), ptype = "character()"), "with dimensions")
  expect_output(summary(cloud[0]), "(0, 12)")
  expect_output(summary(cloud[, 1L]), "incomplete")
  expect_output(summary(cloud[, 1L, drop = TRUE]), "no dimensions")
  cloud2 <- cloud
  attr(cloud2, "periods") <- 0
  expect_output(summary(cloud2), "invalid attributes")
})

test_that("as.data.frame method ok", {
  expect_s3_class(df <- as.data.frame.ichimoku(cloud), "data.frame")
  expect_identical(df, xts_df(cloud))
  expect_identical(dim(df), c(281L, 13L))
  expect_identical(attr(as.data.frame(structure(cloud, special = "set"), keep.attrs = TRUE), "special"), "set")
})

test_that("as_tibble method ok", {
  expect_s3_class(tbl <- as_tibble.ichimoku(cloud), "tbl_df")
  expect_identical(tbl, as_tibble(cloud))
  expect_identical(dim(tbl), c(281L, 13L))
  expect_identical(attr(as_tibble(structure(cloud, special = "set"), keep.attrs = TRUE), "special"), "set")
})

test_that("coredata method ok", {
  expect_length(core <- coredata.ichimoku(cloud), 3372L)
  expect_length(attrs <- attributes(core), 2L)
  expect_identical(attrs$dim, c(281L, 12L))
  expect_null(attrs$dimnames[[1L]])
  expect_length(attrs <- attributes(coredata(cloud, fmt = TRUE)), 2L)
  expect_vector(attrs$dimnames[[1L]], ptype = "character", size = 281)
})

test_that("index method ok", {
  expect_vector(expect_s3_class(idx <- index.ichimoku(cloud), "POSIXct"), size = 281)
  expect_length(attrs <- attributes(idx), 3L)
  expect_identical(names(attrs), c("tzone", "tclass", "class"))
  expect_vector(expect_s3_class(index(cloud, 101:110), "POSIXct"), size = 10)
})

test_that("is.ichimoku ok", {
  expect_true(is.ichimoku(cloud))
  expect_false(is.ichimoku(sample_ohlc_data))
})

test_that(".ichimoku ok", {
  expect_identical(.ichimoku(sample_ohlc_data, ticker = "TKR"), cloud)
  expect_identical(attr(.ichimoku(sample_ohlc_data), "ticker"), "sample_ohlc_data")
})
