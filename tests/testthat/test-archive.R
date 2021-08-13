test_that("archive functions ok", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("openssl")
  cloud <- ichimoku(sample_ohlc_data)
  df <- xts_df(cloud)
  filename <- tempfile()
  archive(cloud, filename)
  restored <- archive(filename)
  unlink(filename)
  expect_identical(restored, cloud)
  expect_error(archive(), "is used to")
  expect_error(archive(cloud, cloud, cloud), "Too many arguments")
  expect_error(archive(cloud), "supplied as a string")
  expect_error(archive(cloud, cloud), "supplied as a string")
  expect_error(archive(object = cloud), "specified without")
  expect_error(archive(object = sample_ohlc_data, filename = filename), "only writes ichimoku")
  filename <- tempfile()
  arrow::write_feather(sample_ohlc_data, filename)
  expect_error(archive(filename = filename), "not an archived ichimoku")
  unlink(filename)
  dg <- structure(df,
                  periods = attr(cloud, "periods"),
                  periodicity = attr(cloud, "periodicity"),
                  ticker = attr(cloud, "ticker"),
                  ichimoku351 = TRUE)
  filename <- tempfile()
  arrow::write_feather(dg, filename)
  expect_message(archive(filename), "cannot be verified")
  unlink(filename)
  dh <- structure(df,
                  periods = attr(cloud, "periods"),
                  periodicity = attr(cloud, "periodicity"),
                  ticker = attr(cloud, "ticker"),
                  ichimoku351 = TRUE,
                  sha256 = "010101010101")
  filename <- tempfile()
  arrow::write_feather(dh, filename)
  expect_warning(archive(filename), "does not match the original")
  unlink(filename)
})
