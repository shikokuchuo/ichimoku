# Ichimoku - Visualization Layer -----------------------------------------------

#' Interactive Ichimoku Cloud Plot
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects in R Shiny,
#'     allowing full customisation of chart elements in an interactive
#'     environment. Intuitive cursor infotip provides ready access to the data
#'     directly from the chart.
#'
#' @param x an object of class 'ichimoku'.
#' @inheritParams plot.ichimoku
#' @param ... additional parameters passed along to the 'options' argument of
#'     \code{shiny::shinyApp()}.
#' @param launch.browser [default TRUE] If TRUE, the system's default web browser
#'     will be launched automatically after the app is started. The value of this
#'     argument can also be a function to call with the application's URL. To use
#'     the default Shiny viewer in RStudio, please specify
#'     \code{getOption("shiny.launch.browser")}.
#'
#' @return A Shiny app object with class 'shiny.appobj'. With default arguments,
#'     the Shiny app is launched in the default browser.
#'
#' @details This function has a dependency on the 'shiny' package.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' iplot(cloud)
#'
#' # To open in RStudio viewer instead of default browser
#' iplot(cloud, launch.browser = getOption("shiny.launch.browser"))
#' }
#'
#' @export
#'
iplot <- function(x,
                  ticker,
                  subtitle,
                  theme = c("original", "dark", "solarized", "mono"),
                  strat = TRUE,
                  type = c("none", "r", "s", "bar", "line"),
                  custom,
                  ...,
                  launch.browser = TRUE) {

  if (requireNamespace("shiny", quietly = TRUE)) {

    if (!is.ichimoku(x)) stop("iplot() only works with ichimoku objects", call. = FALSE)
    theme <- match.arg(theme)
    type <- match.arg(type)
    if (missing(ticker)) ticker <- attr(x, "ticker")
    if (missing(subtitle)) {
      subtitle <- if (hasStrat(x) && isTRUE(strat)) paste0("Strategy: ", attr(x, "strat")["Strategy", ][[1L]])
    }

    tformat <- if (attr(x, "periodicity") > 80000) "%F" else "%F %T"
    index <- index(x)
    start <- index[1L]
    end <- index[dim(x)[1L]]
    xadj <- if (nchar(as.character(start)) > 10) -17 else 5

    ichimoku_stheme <- if (requireNamespace("bslib", quietly = TRUE)) {
      bslib::bs_theme(version = 4, bootswatch = "solar", bg = "#ffffff", fg = "#002b36",
                      primary = "#073642", font_scale = 0.85)
    }

    ui <- shiny::fluidPage(
      theme = ichimoku_stheme,
      shiny::fillPage(
        padding = 20,
        shiny::tags$style("#chart {height: calc(100vh - 190px) !important;}"),
        shiny::plotOutput("chart", width = "100%",
                          hover = shiny::hoverOpts(id = "plot_hover",
                                                   delay = 80, delayType = "throttle")),
        shiny::uiOutput("hover_x"), shiny::uiOutput("hover_y"), shiny::uiOutput("infotip")
        ),
      shiny::fluidRow(
        shiny::column(width = 10, offset = 1,
                      shiny::sliderInput("dates", label = NULL, min = start, max = end,
                                         value = c(start, end), width = "100%",
                                         timeFormat = tformat))
        ),
      shiny::fluidRow(
        shiny::column(width = 2,
                      shiny::selectInput("theme", label = "Theme",
                                         choices = c("original", "dark", "solarized", "mono"),
                                         selected = theme, selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::selectInput("type", label = "Type",
                                         choices = c("none", "r", "s", "bar", "line"),
                                         selected = type, selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::selectInput("custom", label = "Custom",
                                         choices = dimnames(x)[[2L]],
                                         selected = NULL, selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::textInput("ticker", label = "Ticker",
                                       value = ticker, width = "100%")),
        shiny::column(width = 2,
                      shiny::textInput("subtitle", label = "Subtitle",
                                       value = subtitle, width = "100%")),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>Show</label>"),
                      shiny::checkboxInput("infotip", "Infotip", value = TRUE)),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>&nbsp;</label>"),
                      if (hasStrat(x)) shiny::checkboxInput("strat", "Strategy", value = isTRUE(strat)))
        )
    )

    server <- function(input, output, session) {

      window <- shiny::reactive(paste0(input$dates[1L], "/", input$dates[2L]))
      left_px <- shiny::reactive(input$plot_hover$coords_css$x)
      top_px <- shiny::reactive(input$plot_hover$coords_css$y)
      posi_x <- shiny::reactive(round(input$plot_hover$x, digits = 0))

      pdata <- shiny::reactive(x[window()])
      plen <- shiny::reactive(dim(pdata())[1L])

      if (requireNamespace("bslib", quietly = TRUE)) {
        shiny::observe({
          session$setCurrentTheme(
            bslib::bs_theme_update(ichimoku_stheme,
                                   bootswatch = switch(input$theme, dark = "solar", NULL)))
        })
      }

      output$chart <- shiny::renderPlot(
        if (input$type == "none") {
          autoplot.ichimoku(pdata(), ticker = input$ticker, subtitle = input$subtitle,
                            theme = input$theme, strat = input$strat)
        } else {
          extraplot(x, window = window(), ticker = input$ticker, subtitle = input$subtitle,
                    theme = input$theme, strat = input$strat, type = input$type, custom = input$custom)
        }
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$type == "none", input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawGuide(label = index(pdata())[posi_x()], left = left_px() + xadj, top = 60)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$type == "none", input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$type == "none", input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawInfotip(sdata = pdata()[posi_x(), ], left_px = left_px(), top_px = top_px())
      })

      session$onSessionEnded(function() shiny::stopApp())
    }

    shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))

  } else {
    message("Please install the 'shiny' package to enable interactive charting",
            "\nAlternatively use plot() for static charts")
  }
}

#' drawGuide
#'
#' Internal function used by ichimoku to draw the axis guides for interactive
#'     Shiny plots.
#'
#' @param label a function returning the character string to be shown.
#' @param left the horizontal position of the guide in pixels.
#' @param top the vertical position of the guide in pixels.
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @noRd
#'
drawGuide <- function(label, left, top) {
  shiny::wellPanel(
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.85); left: ",
                   left, "px; top: ", top, "px; font-size: 0.8em; padding: 0;"),
    shiny::HTML(paste(label))
  )
}

#' drawInfotip
#'
#' Internal function used by ichimoku to draw the infotip for interactive Shiny
#'     plots.
#'
#' @param sdata the selected data frame row.
#' @param left_px the horizontal cursor position in pixels.
#' @param top_px the vertical cursor position in pixels.
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @noRd
#'
drawInfotip <- function(sdata, left_px, top_px) {
  shiny::wellPanel(
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.85); left: ",
                   left_px + 50, "px; top: ", top_px + 40, "px; font-size: 0.8em; padding: 1px 5px 5px 5px;"),
    shiny::HTML(paste0("<div style='margin: 0; padding: 0; font-weight: bold'>",
                       if (isTRUE(sdata$cd == 1)) "&#9651;<br />" else if (isTRUE(sdata$cd == -1)) "&#9660;<br />" else "&#8212;<br />",
                       index(sdata),
                       "</div><div style='text-align: center; margin: 2px 0 0 0; padding: 0'>H: ",
                       signif(sdata$high, digits = 5),
                       "</div><div style='margin: 0; padding: 0'>O: ",
                       signif(sdata$open, digits = 5),
                       "&nbsp;&nbsp;C: ", signif(sdata$close, digits = 5),
                       "</div><div style='text-align: center; margin: 0; padding: 0'>L: ",
                       signif(sdata$low, digits = 5),
                       "</div><div style='margin: 2px 0 0 0; padding: 0'>Tenkan: ",
                       signif(sdata$tenkan, digits = 5),
                       "<br />Kijun: ", signif(sdata$kijun, digits = 5),
                       "<br />Senkou A: ", signif(sdata$senkouA, digits = 5),
                       "<br />Senkou B: ", signif(sdata$senkouB, digits = 5),
                       "<br />Chikou: ", signif(sdata$chikou, digits = 5), "</div>"))
  )
}

