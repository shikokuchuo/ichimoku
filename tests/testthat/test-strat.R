cloud <- ichimoku(sample_ohlc_data)
strat <- strat(cloud, c1 = "chikou", c2 = "cloudT", dir = "long")
strat2 <- strat(cloud, c1 = "chikou", c2 = "cloudT", c3 = "chikou", c4 = "cloudT", dir = "long")
strat3 <- strat(cloud, c1 = "tenkan", c2 = "kijun", dir = "long")
strat4 <- strat(cloud, c1 = "tenkan", c2 = "kijun", dir = "short")

test_that("strat ok", {
  expect_s3_class(strat, "ichimoku")
  expect_identical(dim(strat), c(281L, 19L))
  expect_identical(strat, strat2)
  expect_error(strat(sample_ohlc_data), regexp = "ichimoku object")
  expect_error(strat(cloud, "close", "tenkan", "kijun", "chikou", type = "a"), regexp = "type invalid")
})

test_that("stratcombine ok", {
  expect_s3_class(comb <- stratcombine(strat, strat3), "ichimoku")
  expect_identical(dim(comb), c(281L, 19L))
  expect_identical(stratcombine(strat, strat), strat)
  expect_error(stratcombine(sample_ohlc_data), regexp = "ichimoku object")
  expect_error(stratcombine(strat, strat(cloud[1:100,])), regexp = "same data")
  expect_error(stratcombine(strat, strat4), regexp = "Trade direction")
})

test_that("hasStrat ok", {
  expect_true(hasStrat(strat))
  expect_false(hasStrat(cloud))
})

test_that("summary method ok", {
  expect_true(inherits(summary(strat), "matrix"))
  expect_false(inherits(summary(strat, strat = FALSE), "matrix"))
})
