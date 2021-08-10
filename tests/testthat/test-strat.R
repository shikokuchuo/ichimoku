cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
strat <- strat(cloud, c1 = "chikou", c2 = "cloudT", dir = "long")
strat2 <- strat(cloud, c1 = "chikou", c2 = "cloudT", c3 = "chikou", c4 = "cloudT", dir = "long")
stratlist <- autostrat(cloud, n = 2, dir = "short", level = 2)
grid <- mlgrid(strat, y = "logret", type = "numeric")
grid2 <- mlgrid(strat2, y = "ret", type = "boolean", dir = "short", unique = FALSE)

test_that("strat ok", {
  expect_s3_class(strat, "ichimoku")
  expect_true(dim(strat)[2L] == 19)
  expect_identical(strat, strat2)
  expect_error(strat(sample_ohlc_data), regexp = "ichimoku object")
  expect_error(strat(cloud, "close", "tenkan", "kijun", "chikou", type = "a"), regexp = "Invalid type")
})

test_that("stratcombine ok", {
  comb <- stratcombine(stratlist[[1]], stratlist[[2]])
  expect_s3_class(comb, "ichimoku")
  expect_true(dim(comb)[2L] == 19)
  expect_identical(stratcombine(stratlist[[1]], stratlist[[1]]), stratlist[[1]])
  expect_error(stratcombine(sample_ohlc_data), regexp = "ichimoku object")
  expect_error(stratcombine(strat, strat(cloud[1:100,])), regexp = "same data")
  expect_error(stratcombine(stratlist[[1]], strat), regexp = "Trade direction")
})

test_that("autostrat ok", {
  expect_type(stratlist, "list")
  expect_length(stratlist, 2)
  expect_type(autostrat(cloud, n = 1), "list")
  expect_type(autostrat(cloud, n = 1, level = 3), "list")
  expect_warning(autostrat(cloud, n = 1, level = "a"), regexp = "Invalid level")
  expect_error(autostrat(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("hasStrat ok", {
  expect_true(hasStrat(stratlist[[1]]))
  expect_false(hasStrat(cloud))
})

test_that("summary.strat ok", {
  expect_true(inherits(summary(strat), "matrix"))
  expect_true(inherits(summary(strat, strat = FALSE), "table"))
})

test_that("mlgrid ok", {
  expect_s3_class(grid, "data.frame")
  expect_true(dim(grid)[2L] == 38)
  expect_true(attr(grid, "mlgrid"))
  expect_identical(attr(grid, "y"), "logret")
  expect_identical(attr(grid, "direction"), "long")
  expect_s3_class(grid2, "data.frame")
  expect_true(dim(grid2)[2L] == 75)
  expect_true(attr(grid2, "mlgrid"))
  expect_identical(attr(grid2, "y"), "ret")
  expect_identical(attr(grid2, "direction"), "short")
  expect_error(mlgrid(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("look ok", {
  expect_type(look(cloud), "list")
  expect_length(look(cloud), 3)
  expect_type(look(strat), "list")
  expect_length(look(strat), 4)
  expect_type(look(grid), "list")
  expect_length(look(grid), 3)
  expect_type(look(stratlist), "list")
  expect_s3_class(look(stratlist, which = 1), "ichimoku")
  expect_error(look(stratlist, which = 9), regexp = "one of the strategies")
  expect_error(look(sample_ohlc_data), regexp = "certain object types")
  df <- structure(list(a = 1:10), class = "data.frame", row.names = 1:10,
                  oanda = TRUE, instrument = "INS", price = "B", timestamp = "test")
  expect_type(look(df), "list")
  expect_length(look(df), 3)
})
