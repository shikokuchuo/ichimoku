cloud <- ichimoku(sample_ohlc_data)
grid <- mlgrid(cloud, y = "logret", type = "numeric")
grid2 <- mlgrid(cloud, y = "ret", type = "boolean", dir = "short", unique = FALSE)

test_that("autostrat ok", {
  expect_output(expect_length(expect_type(expect_invisible(autostrat(cloud, n = 2)), "list"), 2L))
  expect_output(expect_type(autostrat(cloud, n = 1), "list"))
  expect_output(expect_type(autostrat(cloud, n = 1, level = 3), "list"))
  expect_warning(autostrat(cloud, n = 1, level = "a"), regexp = "'level' invalid")
  expect_error(autostrat(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("mlgrid ok", {
  expect_s3_class(grid, "data.frame")
  expect_identical(dim(grid), c(153L, 38L))
  expect_identical(names(attributes(grid)), c("names", "class", "row.names", "y", "direction", "ticker", "mlgrid"))
  expect_s3_class(grid2, "data.frame")
  expect_identical(dim(grid2), c(153L, 75L))
  expect_true(attr(grid2, "mlgrid"))
  expect_identical(attr(grid2, "y"), "ret")
  expect_identical(attr(grid2, "direction"), "short")
  expect_error(mlgrid(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("look ok", {
  expect_output(stratlist <- autostrat(cloud, n = 2, dir = "short", level = 2))
  expect_length(expect_type(look(cloud), "list"), 3L)
  expect_length(expect_type(look(stratlist[[1L]]), "list"), 4L)
  expect_length(expect_type(look(grid), "list"), 3L)
  expect_length(expect_type(look(stratlist), "list"), 2L)
  expect_s3_class(look(stratlist, which = 1), "ichimoku")
  expect_null(expect_invisible(look(sample_ohlc_data)))
  expect_error(look(stratlist, which = 9), regexp = "not a valid")
})
