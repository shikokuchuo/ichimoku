# Copyright (C) 2021-2022 Hibiki AI Limited <info@hibiki-ai.com>
#
# This file is part of ichimoku.
#
# ichimoku is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# ichimoku is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# ichimoku. If not, see <https://www.gnu.org/licenses/>.

# Ichimoku - Visualization Layer -----------------------------------------------

#' Interactive Ichimoku Cloud Plot
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects in R Shiny,
#'     allowing full customisation of chart elements in an interactive
#'     environment. Intuitive cursor infotip provides ready access to the data
#'     directly from the chart.
#'
#' @param x an object of class 'ichimoku'.
#' @param theme [default 'original'] with alternative choices of 'conceptual',
#'     'dark', 'fresh', 'mono', or 'solarized'.
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
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
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
                  theme = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                  strat = TRUE,
                  type = c("none", "r", "s", "bar", "line"),
                  custom,
                  ...,
                  launch.browser = TRUE) {

  if (requireNamespace("shiny", quietly = TRUE)) {

    is.ichimoku(x) || stop("iplot() only works with ichimoku objects", call. = FALSE)
    dims <- attr(x, "dim")
    dims[2L] >= 12L || stop("attempt to plot incomplete (partial or subset) ichimoku object", call. = FALSE)
    theme <- match.arg(theme)
    type <- match.arg(type)
    if (missing(ticker)) ticker <- attr(x, "ticker")
    if (missing(subtitle)) {
      subtitle <- if (hasStrat(x) && isTRUE(strat)) paste0("Strategy: ", attr(x, "strat")["Strategy", ][[1L]])
    }

    tformat <- if (attr(x, "periodicity") > 80000) "%F" else "%F %T"
    start <- index.ichimoku(x, 1L)
    end <- index.ichimoku(x, dims[1L])
    xadj <- if (nchar(format.POSIXct(start)) > 10) -17 else 5

    ui <- shiny::fluidPage(
      shiny::tags$head(shiny::tags$style("
    #chart {height: calc(100vh - 189px) !important}
    .control-label {font-weight: 400}
  ")),
      shiny::fillPage(
        padding = 20,
        shiny::plotOutput("chart", width = "100%",
                          hover = shiny::hoverOpts(id = "plot_hover", delay = 100, delayType = "throttle")),
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
                                         choices = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                                         selected = theme, selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::selectInput("type", label = "Type",
                                         choices = c("none", "r", "s", "bar", "line"),
                                         selected = type, selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::selectInput("custom", label = "Custom",
                                         choices = attr(x, "dimnames")[[2L]],
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

      pdata <- shiny::reactive(create_data(x, window = window(), type = input$type))
      plen <- shiny::reactive(attr(pdata(), "dim")[1L])

      output$chart <- shiny::renderPlot(
        plot_ichimoku(pdata(), ticker = input$ticker, subtitle = input$subtitle,
                      theme = input$theme, strat = input$strat, type = input$type, custom = input$custom)
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawGuide(label = index.ichimoku(pdata(), posi_x()), left = left_px() + xadj, top = 60)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawInfotip(sidx = index.ichimoku(pdata(), posi_x()),
                    sdata = coredata.ichimoku(pdata())[posi_x(), ],
                    left = left_px(), top = top_px(),
                    type = input$type, custom = input$custom)
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
    shiny::HTML(as.character(label))
  )
}

#' drawInfotip
#'
#' Internal function used by ichimoku to draw the Infotip for interactive Shiny
#'     plots.
#'
#' @param sidx the selected index value.
#' @param sdata the selected coredata row.
#' @param left the horizontal cursor position in pixels.
#' @param top the vertical cursor position in pixels.
#' @param type the type of subplot.
#' @param custom the column name of custom subplot (if applicable).
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @noRd
#'
drawInfotip <- function(sidx, sdata, left, top, type, custom = NULL) {
  shiny::wellPanel(
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.8); left: ",
                   left + 60, "px; top: ", top + 45, "px; font-size: 0.8em; padding: 1px 5px 5px 5px;"),
    shiny::HTML(paste0("<div style='margin: 0; padding: 0; font-weight: bold'>",
                       if (isTRUE(sdata[["cd"]] == 1)) "&#9651;<br />" else if (isTRUE(sdata[["cd"]] == -1)) "&#9660;<br />" else "&#8212;<br />",
                       format.POSIXct(sidx),
                       "</div><div style='text-align: center; margin: 2px 0 0 0; padding: 0'>H: ",
                       signif(sdata[["high"]], digits = 5),
                       "</div><div style='margin: 0; padding: 0'>O: ",
                       signif(sdata[["open"]], digits = 5),
                       "&nbsp;&nbsp;C: ", signif(sdata[["close"]], digits = 5),
                       "</div><div style='text-align: center; margin: 0; padding: 0'>L: ",
                       signif(sdata[["low"]], digits = 5),
                       "</div><div style='margin: 2px 0 0 0; padding: 0'>Tenkan: ",
                       signif(sdata[["tenkan"]], digits = 5),
                       "<br />Kijun: ", signif(sdata[["kijun"]], digits = 5),
                       "<br />Senkou A: ", signif(sdata[["senkouA"]], digits = 5),
                       "<br />Senkou B: ", signif(sdata[["senkouB"]], digits = 5),
                       "<br />Chikou: ", signif(sdata[["chikou"]], digits = 5),
                       switch(
                         type,
                         r = paste0("<br />R-indicator: ", signif(100 * sdata[["osc_typ_slw"]], digits = 3)),
                         s = paste0("<br />S-fast: ", signif(100 * sdata[["osc_typ_fst"]], digits = 3),
                                    "<br />S-slow: ", signif(100 * sdata[["osc_typ_slw"]], digits = 3)),
                         line = ,
                         bar = paste0("<br />", custom, " : ", signif(sdata[[custom]], digits = 5)),
                         NULL
                       ),
                       "</div>"))
  )
}

