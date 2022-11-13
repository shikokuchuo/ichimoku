cloud <- ichimoku(sample_ohlc_data)
grid <- mlgrid(cloud, y = "none", type = "numeric")
grid2 <- mlgrid(cloud, y = "logret", type = "boolean", dir = "short", unique = FALSE)
grid3 <- mlgrid(cloud, y = "ret", k = 3, type = "z-score", format = "matrix")
stratlist <- autostrat(cloud, n = 2, dir = "short", quietly = TRUE)

test_that("autostrat ok", {
  expect_output(expect_length(expect_type(expect_invisible(autostrat(cloud, n = 2, level = 2)), "list"), 2L))
  expect_silent(expect_type(autostrat(cloud, n = 1, dir = "short", level = 3, quietly = TRUE), "list"))
  expect_output(expect_warning(autostrat(cloud, n = 1, level = "a"), regexp = "'level' invalid"))
  expect_error(autostrat(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("mlgrid ok", {
  expect_s3_class(grid, "data.frame")
  expect_identical(dim(grid), c(155L, 37L))
  expect_s3_class(grid2, "data.frame")
  expect_identical(dim(grid2), c(153L, 75L))
  expect_type(grid3, "double")
  expect_identical(dim(grid3), c(151L, 38L))
  expect_warning(mlgrid(cloud, k = NA), regexp = "'k' invalid")
  expect_error(mlgrid(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("relative ok", {
  expect_output(expect_s3_class(rel <- relative(cloud), "data.frame"))
  expect_identical(dim(rel), c(37L, 8L))
  expect_length(expect_type(look(rel), "pairlist"), 4L)
  expect_silent(relative(cloud, order = TRUE, signif = 0.4, quietly = TRUE))
  expect_error(relative(sample_ohlc_data), regexp = "ichimoku object")
})

test_that("look ok", {
  expect_length(expect_type(look(cloud), "pairlist"), 3L)
  expect_length(expect_type(look(stratlist[[1L]]), "pairlist"), 4L)
  expect_length(expect_type(look(grid), "pairlist"), 7L)
  expect_length(expect_type(look(stratlist), "pairlist"), 2L)
  expect_null(expect_invisible(look(sample_ohlc_data)))
  expect_null(expect_invisible(look()))
})
