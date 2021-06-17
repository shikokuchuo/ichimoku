cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9, 26, 52))
cloud2 <- ichimoku(cloud, ticker = "TKR")
plot <- plot(cloud, ticker = "TKR Co.", theme = "solarized")
iplot <- iplot(cloud, window = "2020-02-02/2020-02-04", theme = "dark", gaps = TRUE)
strat <- strat(cloud, c1 = "chikou", c2 = "cloudTop", dir = "long")
stratlist <- autostrat(cloud, n = 2, dir = "short", level = 2)
comb <- stratcombine(stratlist[[1]], stratlist[[2]])
grid <- mlgrid(strat)

test_that("ichimoku passes", {
  expect_s3_class(cloud, "ichimoku")
  expect_s3_class(cloud, "xts")
  expect_s3_class(cloud, "zoo")
  expect_identical(cloud, cloud2)
  expect_true(dim(cloud)[2L] == 12)
  expect_null(ichimoku(plot))
})

test_that("ichimoku plot functions pass", {
  expect_s3_class(plot, "gg")
  expect_s3_class(plot, "ggplot")
  expect_s3_class(iplot, "plotly")
  expect_s3_class(iplot, "htmlwidget")
})

test_that("is.ichimoku passes", {
  expect_true(is.ichimoku(cloud))
  expect_false(is.ichimoku(sample_ohlc_data))
})

test_that("tradingDays passes", {
  expect_true(tradingDays(sample_ohlc_data$date[2], holidays = NULL))
  expect_vector(tradingDays(sample_ohlc_data$date[1:3]), ptype = logical(), size = 3)
})

test_that("window functions pass", {
  expect_vector(maxOver(sample_ohlc_data$close[1:10], 3), ptype = numeric(), size = 10)
  expect_vector(minOver(sample_ohlc_data$close[1:10], 3), ptype = numeric(), size = 10)
})

test_that("strat passes", {
  expect_s3_class(strat, "ichimoku")
  expect_true(dim(strat)[2L] == 19)
})

test_that("stratcombine passes", {
  expect_s3_class(comb, "ichimoku")
  expect_true(dim(comb)[2L] == 19)
})

test_that("autostrat passes", {
  expect_type(stratlist, "list")
  expect_length(stratlist, 2)
})

test_that("hasStrat passes", {
  expect_true(hasStrat(stratlist[[1]]))
  expect_false(hasStrat(cloud))
})

test_that("mlgrid passes", {
  expect_s3_class(grid, "data.frame")
  expect_true(attr(grid, "mlgrid"))
})

test_that("trimdf passes", {
  expect_equal(dim(trimdf(data.frame(c(1:4, NA), c(NA, 2:5)))), c(3L, 2L))
})

test_that("duplicate passes", {
  expect_equal(duplicate(3), c(4, 7, 8))
})
