# Copyright (C) 2021-2025 Hibiki AI Limited <info@hibiki-ai.com>
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
#' @param x an object of class \sQuote{ichimoku}.
#' @param theme [default 'classic'] with further choices of \sQuote{dark},
#'     \sQuote{mono}, \sQuote{noguchi}, \sQuote{okabe-ito} or \sQuote{solarized}.
#' @inheritParams plot.ichimoku
#' @param ... additional parameters passed along to the \sQuote{options}
#'     argument of \code{shiny::shinyApp()}.
#' @param launch.browser [default TRUE] If TRUE, the system's default web
#'     browser will be launched automatically after the app is started. The
#'     value of this argument can also be a function to call with the
#'     application's URL. To use the default Shiny viewer in RStudio, please
#'     specify \code{getOption("shiny.launch.browser")}.
#'
#' @return A Shiny app object with class \sQuote{shiny.appobj}. With default
#'     arguments, the Shiny app is launched in the default browser.
#'
#' @details For further details please refer to the reference vignette by
#'     calling: \code{vignette("reference", package = "ichimoku")}
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
                  theme = c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"),
                  strat = TRUE,
                  type = c("none", "r", "s", "bar", "line"),
                  custom,
                  ...,
                  launch.browser = TRUE) {

  is.ichimoku(x) || stop("iplot() only works with ichimoku objects", call. = FALSE)
  dims <- attr(x, "dim")
  dims[2L] >= 12L ||
    stop("attempt to plot incomplete (partial or subset) ichimoku object", call. = FALSE)
  theme <- match.arg(theme, c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"))
  type <- match.arg(type, c("none", "r", "s", "bar", "line"))
  if (missing(ticker))
    ticker <- attr(x, "ticker")
  if (missing(subtitle))
    subtitle <- if (hasStrat(x) && isTRUE(strat))
      paste0("Strategy: ", attr(x, "strat")["Strategy", ][[1L]])

  tformat <- if (attr(x, "periodicity") > 80000) "%F" else "%F %T"
  start <- index.ichimoku(x, 1L)
  end <- index.ichimoku(x, dims[1L])
  xadj <- if (nchar(format_POSIXct(start)) > 10) -17 else 5

  ui <- fluidPage(
    tags$head(tags$style("
    #chart {height: calc(100vh - 189px) !important}
    .control-label {font-weight: 400}
  ")),
    fillPage(
      padding = 20,
      plotOutput("chart", width = "100%",
                 hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "throttle")),
      uiOutput("hover_x"), uiOutput("hover_y"), uiOutput("infotip")
    ),
    fluidRow(
      column(width = 10, offset = 1,
             sliderInput("dates", label = NULL, min = start, max = end,
                         value = c(start, end), width = "100%", timeFormat = tformat))
    ),
    fluidRow(
      column(width = 2,
             selectInput("theme", label = "Theme",
                         choices = c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"),
                         selected = theme, selectize = FALSE)),
      column(width = 2,
             selectInput("type", label = "Type",
                         choices = c("none", "r", "s", "bar", "line"),
                         selected = type, selectize = FALSE)),
      column(width = 2,
             selectInput("custom", label = "Custom",
                         choices = attr(x, "dimnames")[[2L]],
                         selected = NULL, selectize = FALSE)),
      column(width = 2,
             textInput("ticker", label = "Ticker", value = ticker, width = "100%")),
      column(width = 2,
             textInput("subtitle", label = "Subtitle", value = subtitle, width = "100%")),
      column(width = 1,
             HTML("<label class='control-label'>Show</label>"),
             checkboxInput("infotip", "Infotip", value = TRUE)),
      column(width = 1,
             HTML("<label class='control-label'>&nbsp;</label>"),
             if (hasStrat(x)) checkboxInput("strat", "Strategy", value = isTRUE(strat)))
    )
  )

  server <- function(input, output, session) {

    window <- reactive(sprintf("%s/%s", input$dates[1L], input$dates[2L]))
    left_px <- reactive(input$plot_hover$coords_css$x)
    top_px <- reactive(input$plot_hover$coords_css$y)
    posi_x <- reactive(round(input$plot_hover$x, digits = 0))

    pdata <- reactive(create_data(x, window = window(), type = input$type))
    plen <- reactive(attr(pdata(), "dim")[1L])

    output$chart <- renderPlot(
      plot_ichimoku(pdata(), ticker = input$ticker, subtitle = input$subtitle,
                    theme = input$theme, strat = input$strat, type = input$type, custom = input$custom)
    )
    output$hover_x <- renderUI({
      req(input$plot_hover, posi_x() > 0, posi_x() <= plen())
      drawGuide(label = index.ichimoku(pdata(), posi_x()), left = left_px() + xadj, top = 60)
    })
    output$hover_y <- renderUI({
      req(input$plot_hover)
      drawGuide(label = signif(input$plot_hover$y, digits = 5), left = 75, top = top_px() + 11)
    })
    output$infotip <- renderUI({
      req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= plen())
      drawInfotip(sidx = index.ichimoku(pdata(), posi_x()),
                  sdata = coredata.ichimoku(pdata())[posi_x(), ],
                  left = left_px(), top = top_px(),
                  type = input$type, custom = input$custom)
    })

    session$onSessionEnded(stopApp)
  }

  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))

}

#' drawGuide
#'
#' Internal function used by ichimoku to draw the axis guides for interactive
#'     Shiny plots.
#'
#' @param label a function returning the character string to be shown.
#' @param left integer horizontal position of the guide in pixels.
#' @param top integer vertical position of the guide in pixels.
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @noRd
#'
drawGuide <- function(label, left, top) {
  wellPanel(
    style = sprintf("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.85);
                    left: %dpx; top: %dpx; font-size: 0.8em; padding: 0;", left, top),
    HTML(as.character(label))
  )
}

#' drawInfotip
#'
#' Internal function used by ichimoku to draw the Infotip for interactive Shiny
#'     plots.
#'
#' @param sidx the selected index value.
#' @param sdata the selected coredata row.
#' @param left integer horizontal cursor position in pixels.
#' @param top integer vertical cursor position in pixels.
#' @param type the type of subplot.
#' @param custom the column name of custom subplot (if applicable).
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @noRd
#'
drawInfotip <- function(sidx, sdata, left, top, type, custom = NULL) {
  cd <- sdata[["cd"]]
  wellPanel(
    style = sprintf("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.8);
                    left: %dpx; top: %dpx; font-size: 0.8em; padding: 1px 5px 5px 5px;",
                    left + 60, top + 45),
    HTML(
      sprintf(
        "<div style='margin: 0; padding: 0; font-weight: bold'>%s<br/>%s</div>
        <div style='text-align: center; margin: 2px 0 0 0; padding: 0'>H: %.5g</div>
        <div style='margin: 0; padding: 0'>O: %.5g&nbsp;&nbsp;C: %.5g</div>
        <div style='text-align: center; margin: 0; padding: 0'>L: %.5g</div>
        <div style='margin: 2px 0 0 0; padding: 0'>Tenkan: %.5g<br />Kijun: %.5g<br />Senkou A: %.5g<br />Senkou B: %.5g<br />Chikou: %.5g%s</div>",
        if (is.na(cd) || cd == 0) "&#8212;" else if (cd == 1) "&#9651;" else if (cd == -1) "&#9660;",
        format_POSIXct(sidx), sdata[["high"]], sdata[["open"]], sdata[["close"]], sdata[["low"]],
        sdata[["tenkan"]], sdata[["kijun"]], sdata[["senkouA"]], sdata[["senkouB"]], sdata[["chikou"]],
        switch(
          type,
          r = sprintf("<br />R-indicator: %.3g", 100 * sdata[["osc_typ_slw"]]),
          s = sprintf("<br />S-fast: %.3g<br />S-slow: %.3g", 100 * sdata[["osc_typ_fst"]], 100 * sdata[["osc_typ_slw"]]),
          line = ,
          bar = sprintf("<br />%s: %s", custom, as.character(signif(sdata[[custom]], digits = 5))),
          ""
        )
      )
    )
  )
}
