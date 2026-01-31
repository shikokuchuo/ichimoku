# ichimoku Technical Utility Version

Create an ichimoku object containing values for all components of the
Ichimoku Kinko Hyo cloud chart. The object encapsulates a date-time
index, OHLC pricing data, candle direction, the cloud lines Tenkan-sen,
Kijun-sen, Senkou span A, Senkou span B and Chikou span, as well as
values for the cloud top and cloud base.

## Usage

``` r
.ichimoku(x, ticker, periods = c(9L, 26L, 52L), ...)
```

## Arguments

- x:

  a data.frame object with a POSIXct date-time index as the first column
  and numeric OHLC pricing data as the second through fifth columns.

- ticker:

  (optional) specify a ticker to identify the instrument, otherwise this
  is set to the name of the input object.

- periods:

  \[default c(9L, 26L, 52L)\] a vector defining the length of periods
  used for the cloud. This parameter shoud not normally be modified as
  using other values would be invalid in the context of traditional
  ichimoku analysis.

- ...:

  additional arguments, for instance ‘holidays’, passed along to
  [`tradingDays`](https://shikokuchuo.net/ichimoku/dev/reference/tradingDays.md)
  for calculating the future cloud on daily data.

## Value

An ichimoku object with S3 classes of ‘ichimoku’, ‘xts’ and ‘zoo’.

## Details

A faster version of
[`ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/ichimoku.md)
which can be used when the data is a dataframe in the prescribed format.
Does not support the argument ‘keep.data’.
