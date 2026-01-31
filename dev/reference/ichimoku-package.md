# ichimoku: Visualization and Tools for Ichimoku Kinko Hyo Strategies

An implementation of 'Ichimoku Kinko Hyo', also commonly known as 'cloud
charts'. Static and interactive visualizations with tools for creating,
backtesting and development of quantitative 'ichimoku' strategies. As
described in Sasaki (1996, ISBN:4925152009), the technique is a
refinement on candlestick charting, originating from Japan and now in
widespread use in technical analysis worldwide. Translating as
'one-glance equilibrium chart', it allows the price action and market
structure of financial securities to be determined 'at-a-glance'.
Incorporates an interface with the OANDA fxTrade API
<https://developer.oanda.com/> for retrieving historical and live
streaming price data for major currencies, metals, commodities,
government bonds and stock indices.

## Principal ichimoku functions

Data & Visualization

- [`ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/ichimoku.md)
  to create an ichimoku object from price data.

- [`plot.ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/plot.ichimoku.md)
  / [`iplot`](https://shikokuchuo.net/ichimoku/dev/reference/iplot.md)
  to plot (interactive) cloud charts from ichimoku objects.

- [`archive`](https://shikokuchuo.net/ichimoku/dev/reference/archive.md)
  for reading/writing objects to/from archive files with data
  verification.

- [`oanda`](https://shikokuchuo.net/ichimoku/dev/reference/oanda.md) to
  retrieve price data from the OANDA fxTrade API.

Strategies & ML

- [`strat`](https://shikokuchuo.net/ichimoku/dev/reference/strat.md) to
  augment an ichimoku object with a strategy, including combined and
  asymmetric strategies.

- [`autostrat`](https://shikokuchuo.net/ichimoku/dev/reference/autostrat.md)
  to automatically evaluate and rank top-performing strategies.

- [`mlgrid`](https://shikokuchuo.net/ichimoku/dev/reference/mlgrid.md)
  to generate a numeric representation of the ichimoku cloud chart.

- [`relative`](https://shikokuchuo.net/ichimoku/dev/reference/relative.md)
  to produce a statistical summary of the latest ichimoku numeric
  representation relative to historical values.

Real-time

- [`oanda_chart`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_chart.md)
  to plot real-time ichimoku cloud charts using OANDA data.

- [`oanda_studio`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_studio.md)
  a complete live analysis environment using OANDA data implemented in R
  Shiny.

- [`oanda_stream`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_stream.md)
  /
  [`oanda_quote`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_quote.md)
  to obtain the latest live data stream / quote from the OANDA fxTrade
  API.

- [`oanda_view`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_view.md)
  for a market overview showing the relative performance of
  constituents.

- [`oanda_orders`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_orders.md)
  /
  [`oanda_positions`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_positions.md)
  to retrieve the aggregate OANDA fxTrade order / position book.

## References

Sasaki, H. (1996), *ichimoku kinkouhyou no kenkyuu*. Tokyo, Japan:
Toushi Radar.

OANDA' and 'fxTrade' are trademarks owned by OANDA Corporation, an
entity unaffiliated with the ichimoku package, its authors or copyright
holders.

## See also

Useful links:

- <https://shikokuchuo.net/ichimoku/>

- <https://github.com/shikokuchuo/ichimoku/>

- Report bugs at <https://github.com/shikokuchuo/ichimoku/issues>

## Author

**Maintainer**: Charlie Gao <charlie.gao@shikokuchuo.net>
([ORCID](https://orcid.org/0000-0002-0750-061X))

Other contributors:

- Hibiki AI Limited \[copyright holder\]
