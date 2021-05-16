data <- data.frame(
  Date = do.call(c, lapply(0:4, function (x) seq.POSIXt(
    from = as.POSIXct(paste0("2020-01-", 7 * x + 1)), by = "1 day", l = 5))),
  Open = c(10, seq(11.5, by = 1, l = 9), 20, 20, 18:16, 15:24),
  TKR.high = c(12:21, 20.5, 20:18, 16, 16, 17:25),
  lowprice = c(10:19, 18:14, 14, seq(15, by = 1, l = 9)),
  px_Close = c(12, 12:20, 20, 18:15, seq(15.5, by = 1, l = 10))
)

cloud <- ichimoku(data, periods = c(2,4,8))
plot <- plot(cloud, ticker = "TKR", theme = "solarized")
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
  expect_false(is.ichimoku(plot))
})

