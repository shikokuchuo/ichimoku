cloud <- ichimoku(sample_ohlc_data)
strat <- strat(cloud, c1 = "chikou", c2 = "cloudT", dir = "long")
strat2 <- strat(cloud, c1 = "chikou", c2 = "cloudT", c3 = "chikou", c4 = "cloudT", dir = "long")
strat3 <- strat(cloud, c1 = "chikou", c2 = "cloudT", c3 = "chikou", c4 = "cloudT", type = 3)
strat4 <- strat(cloud, c1 = "tenkan", c2 = "kijun", dir = "long")
strat5 <- strat(cloud, c1 = "tenkan", c2 = "kijun", dir = "short")

test_that("strat ok", {
  expect_s3_class(strat, "ichimoku")
  expect_identical(dim(strat), c(281L, 19L))
  expect_identical(strat, strat2)
  expect_identical(strat2, strat3)
  expect_error(strat(sample_ohlc_data), regexp = "ichimoku object")
  expect_error(strat(cloud, "close", "tenkan", "kijun", "chikou", type = "a"), regexp = "type invalid")
})

test_that("stratcombine ok", {
  expect_s3_class(comb <- stratcombine(strat, strat4), "ichimoku")
  expect_identical(dim(comb), c(281L, 19L))
  expect_identical(stratcombine(strat, strat), strat)
  expect_error(stratcombine(sample_ohlc_data), regexp = "ichimoku object")
  expect_error(stratcombine(strat, strat(cloud[1:100,])), regexp = "same data")
  expect_error(stratcombine(strat, strat5), regexp = "Trade direction")
})

test_that("hasStrat ok", {
  expect_true(hasStrat(strat))
  expect_false(hasStrat(cloud))
})

test_that("str method for strat ok", {
  expect_output(expect_null(expect_invisible(str(strat))), "w/ strat")
})

test_that("summary method for strat ok", {
  expect_type(summary(strat), "list")
  expect_output(expect_vector(summary(strat, strat = FALSE), ptype = "character()"), "with dimensions")
  attr(strat5, "strat") <- "invalid"
  expect_output(summary(strat5), "invalid strategy")
})
