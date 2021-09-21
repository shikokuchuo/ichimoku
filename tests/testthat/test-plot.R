cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", keep.data = TRUE)
strat <- strat(cloud)

test_that("autoplot ok", {
  expect_s3_class(autoplot(cloud, ticker = "TKR Co.", theme = "solarized"), "ggplot")
  expect_s3_class(autoplot(strat, subtitle = "Test", theme = "mono"), "ggplot")
})

test_that("extraplot ok", {
  expect_s3_class(extraplot(cloud, window = "2020-04/", type = "s"), "gtable")
  expect_s3_class(extraplot(strat, theme = "original", type = "r", custom = "nonexist"), "gtable")
  expect_s3_class(extraplot(strat, type = "line", custom = "posn"), "gtable")
  expect_s3_class(extraplot(cloud, type = "bar", custom = "vol"), "gtable")
  expect_s3_class(expect_warning(extraplot(cloud), "'type' not specified"), "ggplot")
  expect_s3_class(expect_warning(extraplot(cloud, type = "line"), "'custom' not specified"), "ggplot")
  expect_s3_class(expect_warning(extraplot(cloud, type = "bar", custom = "vix"), "does not match"), "ggplot")
})

test_that("ichimoku plot method ok", {
  expect_s3_class(expect_invisible(plot(cloud, window = "2020-06", subtitle = "subtitle")), "ichimoku")
  expect_s3_class(expect_invisible(plot(strat, type = "s", theme = "dark", strat = FALSE)), "ichimoku")
  expect_s3_class(expect_invisible(plot(cloud, custom = "close")), "ichimoku")
})
