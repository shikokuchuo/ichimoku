# Append New Data to Dataframe

Update a 'data.frame' object with new data. Can be used to append new
updated time series data to an existing dataframe, where each
observation is indexed by a unique timestamp/identifier in a key column.

## Usage

``` r
df_append(old, new, key = "time", keep.attr = "timestamp")
```

## Arguments

- old:

  data.frame object containing existing data.

- new:

  data.frame object containing new data.

- key:

  \[default 'time'\] column name used as key, provided as a character
  string.

- keep.attr:

  \[default 'timestamp'\] name of an attribute in ‘new’ to retain, if
  present, provided as a character string.

## Value

A data.frame of the existing data appended with the new data. If the
data in ‘new’ contains data with the same value for the key column as
‘old’, the data in ‘new’ will overwrite the data in ‘old’.

If the attribute specified by ‘keep.attr’ is present in ‘new’, this is
retained. All other non-required attributes are dropped.

## Details

Can be used to update price dataframes retrieved by
[`oanda`](https://shikokuchuo.net/ichimoku/dev/reference/oanda.md). The
function is designed to update existing data with new values as they
become available. As opposed to
[`df_merge`](https://shikokuchuo.net/ichimoku/dev/reference/df_merge.md),
the data in ‘new’ will overwrite the data in ‘old’ rather than create
duplicates.

## Examples

``` r
data1 <- sample_ohlc_data[1:8, ]
data1
#>         time  open  high   low close volume
#> 1 2020-01-02 123.0 123.1 122.5 122.7   1875
#> 2 2020-01-03 122.7 122.8 122.6 122.8   1479
#> 3 2020-01-06 122.8 123.4 122.4 123.3   1792
#> 4 2020-01-07 123.3 124.3 123.3 124.1   1977
#> 5 2020-01-08 124.1 124.8 124.0 124.8   2239
#> 6 2020-01-09 124.8 125.4 124.5 125.3   1842
#> 7 2020-01-10 125.3 125.3 124.8 125.2   2548
#> 8 2020-01-13 125.2 125.3 125.1 125.2   2946
data2 <- sample_ohlc_data[7:10, ]
data2
#>          time  open  high   low close volume
#> 7  2020-01-10 125.3 125.3 124.8 125.2   2548
#> 8  2020-01-13 125.2 125.3 125.1 125.2   2946
#> 9  2020-01-14 125.2 125.2 124.3 124.4   2796
#> 10 2020-01-15 124.4 124.5 123.7 123.9   2879
df_append(data1, data2)
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
