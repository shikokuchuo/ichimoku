# Merge Dataframes

Full join on an arbitrary number of 'data.frame' objects passed as
arguments, preserving all unique entries. Can be used to combine
historical time series data where each observation is indexed by a
unique timestamp and all periods are complete.

## Usage

``` r
df_merge(...)
```

## Arguments

- ...:

  data.frame objects to combine.

## Value

A data.frame containing all unique entries in the objects passed as
argument.

## Details

Can be used to join price dataframes retrieved by
[`oanda`](https://shikokuchuo.net/ichimoku/dev/reference/oanda.md). The
function is designed to join complete historical data. If the data to be
merged contains data with incomplete periods, all entries are preserved
rather than updated. If incomplete periods are detected within the data,
a warning is issued, and the resulting dataframe should be manually
checked in case it contains unwanted duplicates. Use
[`df_append`](https://shikokuchuo.net/ichimoku/dev/reference/df_append.md)
for updating dataframes with new values.

## Examples

``` r
data1 <- sample_ohlc_data[1:6, ]
data1
#>         time  open  high   low close volume
#> 1 2020-01-02 123.0 123.1 122.5 122.7   1875
#> 2 2020-01-03 122.7 122.8 122.6 122.8   1479
#> 3 2020-01-06 122.8 123.4 122.4 123.3   1792
#> 4 2020-01-07 123.3 124.3 123.3 124.1   1977
#> 5 2020-01-08 124.1 124.8 124.0 124.8   2239
#> 6 2020-01-09 124.8 125.4 124.5 125.3   1842
data2 <- sample_ohlc_data[4:10, ]
data2
#>          time  open  high   low close volume
#> 4  2020-01-07 123.3 124.3 123.3 124.1   1977
#> 5  2020-01-08 124.1 124.8 124.0 124.8   2239
#> 6  2020-01-09 124.8 125.4 124.5 125.3   1842
#> 7  2020-01-10 125.3 125.3 124.8 125.2   2548
#> 8  2020-01-13 125.2 125.3 125.1 125.2   2946
#> 9  2020-01-14 125.2 125.2 124.3 124.4   2796
#> 10 2020-01-15 124.4 124.5 123.7 123.9   2879
df_merge(data1, data2)
#>          time  open  high   low close volume
#> 1  2020-01-02 123.0 123.1 122.5 122.7   1875
#> 2  2020-01-03 122.7 122.8 122.6 122.8   1479
#> 3  2020-01-06 122.8 123.4 122.4 123.3   1792
#> 4  2020-01-07 123.3 124.3 123.3 124.1   1977
#> 5  2020-01-08 124.1 124.8 124.0 124.8   2239
#> 6  2020-01-09 124.8 125.4 124.5 125.3   1842
#> 7  2020-01-10 125.3 125.3 124.8 125.2   2548
#> 8  2020-01-13 125.2 125.3 125.1 125.2   2946
#> 9  2020-01-14 125.2 125.2 124.3 124.4   2796
#> 10 2020-01-15 124.4 124.5 123.7 123.9   2879
```
