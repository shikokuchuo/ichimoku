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
  expect_warning(expect_s3_class(extraplot(cloud, theme = "fresh"), "ggplot"), "'type' not specified")
  expect_warning(expect_s3_class(extraplot(cloud, type = "line"), "ggplot"), "'custom' not specified")
  expect_warning(expect_s3_class(extraplot(cloud, type = "bar", custom = "vix"), "ggplot"), "does not match")
})

test_that("ichimoku plot method ok", {
  expect_s3_class(expect_invisible(plot(cloud, window = "2020-06", subtitle = "subtitle")), "ichimoku")
  expect_s3_class(expect_invisible(plot(strat, theme = "dark", type = "s", strat = FALSE)), "ichimoku")
  expect_s3_class(expect_invisible(plot(cloud, theme = "conceptual", custom = "close")), "ichimoku")
})
