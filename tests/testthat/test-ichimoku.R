cloud <- ichimoku(sample_ohlc_data, periods = c(9, 26, 52))
plot <- plot(cloud, ticker = "TKR Co.", theme = "solarized")
iplot <- iplot(cloud, from = "2020-01-05", to = "2020-02-03", theme = "dark", gaps = TRUE)

test_that("ichimoku passes", {
  expect_s3_class(cloud, "ichimoku")
  expect_s3_class(cloud, "data.frame")
  expect_s3_class(ichimoku(cloud), "ichimoku")
  expect_null(ichimoku(plot))
})

test_that("plot.ichimoku passes", {
  expect_s3_class(plot, "gg")
  expect_s3_class(plot, "ggplot")
  expect_s3_class(iplot, "plotly")
  expect_s3_class(iplot, "htmlwidget")
})

test_that("is.ichimoku passes", {
  expect_true(is.ichimoku(cloud))
  expect_false(is.ichimoku(sample_ohlc_data))
})
