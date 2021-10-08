test_that("iplot Shiny functions ok", {
  skip_if_not_installed("shiny")
  cloud <- ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9, 26, 52))
  strat <- strat(cloud)
  expect_s3_class(iplot(strat, theme = "dark", type = "bar", custom = "ret"), "shiny.appobj")
  expect_error(iplot(sample_ohlc_data), regexp = "ichimoku object")
  shiny::testServer(iplot(cloud), {
    session$setInputs(type = "none", plot_hover = list(x = 2, y = 125, coords_css = list(x = 200, y = 200)),
                      infotip = TRUE)
    session$setInputs(type = "bar", custom = "close", plot_hover = list(x = 2, y = 125, coords_css = list(x = 200, y = 200)),
                      infotip = TRUE)
    expect_s3_class(pdata(), "ichimoku")
  })
  expect_s3_class(drawInfotip(sdata = cloud[100L, ], left_px = 100, top_px = 100), "shiny.tag")
  expect_s3_class(drawGuide(label = index(cloud)[1L], left = 100, top = 100), "shiny.tag")
})
