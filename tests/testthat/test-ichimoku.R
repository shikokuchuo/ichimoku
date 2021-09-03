cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9, 26, 52))
strat <- strat(cloud)

test_that("ichimoku object specification correct", {
  expect_s3_class(cloud, "ichimoku")
  expect_s3_class(cloud, "xts")
  expect_s3_class(cloud, "zoo")
  dims <- dim(cloud)
  expect_true(dims[1L] == 281L)
  expect_true(dims[2L] == 12L)
  names <- c("open", "high", "low", "close", "cd", "tenkan", "kijun", "senkouA",
             "senkouB", "chikou", "cloudT", "cloudB")
  expect_identical(names, dimnames(cloud)[[2L]])
  expect_s3_class(index(cloud), "POSIXct")
})

test_that("ichimoku methods correct", {
  expect_s3_class(ichimoku(xts(sample_ohlc_data[, -1],
                                         order.by = sample_ohlc_data[, 1])), "ichimoku")
  expect_s3_class(ichimoku(as.matrix(xts(sample_ohlc_data[, -1],
                                       order.by = sample_ohlc_data[, 1]))), "ichimoku")
  expect_identical(cloud, ichimoku(cloud))
})

test_that("ichimoku handles higher frequency data", {
  data <- sample_ohlc_data
  data$time <- seq.POSIXt(from = as.POSIXct("2020-01-01"), by = "1 hour", length.out = 256)
  kumo <- ichimoku(data)
  expect_s3_class(kumo, "ichimoku")
  expect_s3_class(autoplot(kumo, ticker = "TKR", theme = "mono"), "ggplot")
})

test_that("ichimoku error handling ok", {
  expect_error(ichimoku(autoplot(cloud)), regexp = "ichimoku object")
  expect_error(ichimoku(nonexistent, regexp = "not found"))
  expect_error(ichimoku("nonexistent", regexp = "not found"))
  expect_error(ichimoku(), regexp = "No object")
  expect_error(ichimoku(data.frame(time = 1:10)), regexp = "not convertible")
  expect_error(ichimoku(sample_ohlc_data[-1]), regexp = "Valid date-time")
  expect_error(ichimoku(sample_ohlc_data[1, ]), regexp = "longer than")
  expect_error(ichimoku(sample_ohlc_data[, -5]), regexp = "data not found")
  expect_warning(ichimoku(sample_ohlc_data[, -3]), regexp = "pseudo-OHLC data")
  expect_warning(ichimoku(sample_ohlc_data[, -2]), regexp = "Opening prices")
  expect_warning(ichimoku(sample_ohlc_data, periods = c(8, 15)), regexp = "cloud periods invalid")
})

test_that("ichimoku plot functions ok", {
  expect_s3_class(autoplot(cloud, ticker = "TKR Co.", theme = "solarized"), "ggplot")
  expect_s3_class(plot(strat, window = "2020-06", message = "message"), "ggplot")
})

test_that("iplot Shiny functions ok", {
  skip_if_not_installed("shiny")
  expect_s3_class(iplot(strat, theme = "dark"), "shiny.appobj")
  expect_error(iplot(sample_ohlc_data), regexp = "ichimoku object")
  shiny::testServer(iplot(cloud), {
    session$setInputs(plot_hover = list(x = 2, y = 125, coords_css = list(x = 200, y = 200)),
                      infotip = TRUE)
    expect_s3_class(pdata(), "ichimoku")
  })
  expect_s3_class(drawInfotip(sdata = cloud[100,], left_px = 100, top_px = 100), "shiny.tag")
  expect_s3_class(drawGuide(label = index(cloud)[1], left = 100, top = 100), "shiny.tag")
})

test_that("is.ichimoku ok", {
  expect_true(is.ichimoku(cloud))
  expect_false(is.ichimoku(sample_ohlc_data))
})

test_that("print.ichimoku print method ok", {
  expect_s3_class(print(cloud), "ichimoku")
  expect_s3_class(print(cloud, plot = FALSE), "ichimoku")
})
