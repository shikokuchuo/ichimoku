cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", keep.data = TRUE)
strat <- strat(cloud)

test_that("autoplot ok", {
  expect_s3_class(autoplot(cloud, ticker = "TKR Co.", theme = "solarized"), "ggplot")
  expect_s3_class(autoplot(strat, subtitle = "Test", theme = "mono"), "ggplot")
  expect_s3_class(autoplot(cloud, window = "2020-04/", theme = "okabe-ito", type = "s"), "ggplot")
  expect_s3_class(autoplot(strat, theme = "classic", type = "r", custom = "nonexist"), "ggplot")
  expect_s3_class(autoplot(strat, theme = "noguchi", type = "line", custom = "posn"), "ggplot")
  expect_s3_class(autoplot(cloud, type = "bar", custom = "vol"), "ggplot")
  expect_warning(expect_s3_class(autoplot(cloud, type = "line"), "ggplot"), "'custom' not specified")
  expect_warning(expect_s3_class(autoplot(cloud, type = "bar", custom = "vix"), "ggplot"), "does not match")
  expect_error(autoplot(cloud[, 1:3]), "plot incomplete")
  expect_error(autoplot(cloud, theme = vector(mode = "list", length = 12L)), "user-defined")
})

test_that("ichimoku plot method ok", {
  expect_s3_class(expect_invisible(plot(cloud, window = "2020-06", subtitle = "subtitle")), "ichimoku")
  expect_s3_class(expect_invisible(plot(strat, theme = "dark", type = "s", strat = FALSE)), "ichimoku")
  expect_s3_class(expect_invisible(plot(cloud, theme = rep(1, 12), custom = "close")), "ichimoku")
})
