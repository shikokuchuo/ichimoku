# ichimoku: Auxiliary Functions

``` r
library(ichimoku)
```

## Introduction

This vignette is dedicated to the auxiliary functions exported by the
ichimoku package.

Note that these auxiliary functions are programmed for performance and
hence stripped of superfluous validation and error-checking code. If
they are used outside of their intended scopes then errors may be
expected. In particular, input types must match exactly.

## Core Auxiliary Functions

#### tradingDays()

Used to subset a vector of dates to trading days. Note: if the argument
‘holidays’ is passed to
[`ichimoku()`](https://shikokuchuo.net/ichimoku/dev/reference/ichimoku.md),
this is passed through to this function when calculating the dates for
the future cloud.

Takes the following arguments:

- `x` a vector of POSIXct dates.
- `holidays` (optional) a vector, or function which outputs a vector, of
  dates defined as holidays. Set to NULL for a continuously-traded
  market. If not specified, New Year’s and Christmas day are defined as
  holidays by default.
- `...` other arguments not used by this function.

``` r
dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
dates
#> [1] "2020-01-01 UTC" "2020-01-02 UTC" "2020-01-03 UTC" "2020-01-04 UTC"
#> [5] "2020-01-05 UTC" "2020-01-06 UTC" "2020-01-07 UTC"
tradingDays(dates)
#> [1] FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
tradingDays(dates, holidays = c("2020-01-02", "2020-01-03"))
#> [1]  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
tradingDays(dates, holidays = NULL)
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

#### look()

Can be used to inspect the informational attributes of R objects.

Takes an object as an optional argument. Called without an argument,
`.Last.value` will be used instead.

For objects created by the ichimoku package, a list of attributes
specific to that data type is returned.

For other objects, a list of attributes that are non-standard for matrix
/ data.frame / xts objects is returned, or else invisible NULL if none
are present.

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
look(cloud)
#> $periods
#> [1]  9 26 52
#> 
#> $periodicity
#> [1] 86400
#> 
#> $ticker
#> [1] "TKR"

strat <- strat(cloud)
look(strat)
#> $periods
#> [1]  9 26 52
#> 
#> $periodicity
#> [1] 86400
#> 
#> $ticker
#> [1] "TKR"
#> 
#> $strat
#>                        [,1]            
#> Strategy               "close > tenkan"
#> ---------------------  "----------"    
#> Strategy cuml return % 8.57            
#> Per period mean ret %  0.0334          
#> Periods in market      138             
#> Total trades           20              
#> Average trade length   6.9             
#> Trade success %        35              
#> Worst trade ret %      -2.54           
#> ---------------------  "----------"    
#> Benchmark cuml ret %   9               
#> Per period mean ret %  0.035           
#> Periods in market      246             
#> ---------------------  "----------"    
#> Direction              "long"          
#> Start                  2020-01-15      
#> End                    2020-12-23      
#> Ticker                 "TKR"

grid <- mlgrid(cloud)
look(grid)
#> $y
#> [1] "logret"
#> 
#> $k
#> [1] 1
#> 
#> $direction
#> [1] "long"
#> 
#> $type
#> [1] "boolean"
#> 
#> $ticker
#> [1] "TKR"
#> 
#> $means
#> [1] NA
#> 
#> $sdevs
#> [1] NA
```

## Dataframe Constructors

#### xts_df()

Convert an ‘xts’ object to ‘data.frame’. This function can be an order
of magnitude faster than
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for an
‘xts’ object.

Note that for ichimoku objects, a slightly faster, more specific version
has been implemented as the S3 method for
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html). Hence
using this utility on an ichimoku object is not necessary.

Takes the following arguments:

- `x` the ‘xts’ object to convert to ‘data.frame’.
- `keep.attrs` (optional) if set to TRUE, will preserve any custom
  attributes set on the original object.

``` r
cloud <- ichimoku(sample_ohlc_data)
df <- xts_df(cloud)
str(df)
#> 'data.frame':    281 obs. of  13 variables:
#>  $ index  : POSIXct, format: "2020-01-02 00:00:00" "2020-01-03 00:00:00" ...
#>  $ open   : num  123 123 123 123 124 ...
#>  $ high   : num  123 123 123 124 125 ...
#>  $ low    : num  122 123 122 123 124 ...
#>  $ close  : num  123 123 123 124 125 ...
#>  $ cd     : num  -1 1 1 1 1 1 -1 0 -1 -1 ...
#>  $ tenkan : num  NA NA NA NA NA ...
#>  $ kijun  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouA: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouB: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ chikou : num  123 123 123 124 124 ...
#>  $ cloudT : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ cloudB : num  NA NA NA NA NA NA NA NA NA NA ...

# Preserving custom attributes:
df2 <- xts_df(cloud, keep.attrs = TRUE)
str(df2)
#> 'data.frame':    281 obs. of  13 variables:
#>  $ index  : POSIXct, format: "2020-01-02 00:00:00" "2020-01-03 00:00:00" ...
#>  $ open   : num  123 123 123 123 124 ...
#>  $ high   : num  123 123 123 124 125 ...
#>  $ low    : num  122 123 122 123 124 ...
#>  $ close  : num  123 123 123 124 125 ...
#>  $ cd     : num  -1 1 1 1 1 1 -1 0 -1 -1 ...
#>  $ tenkan : num  NA NA NA NA NA ...
#>  $ kijun  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouA: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouB: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ chikou : num  123 123 123 124 124 ...
#>  $ cloudT : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ cloudB : num  NA NA NA NA NA NA NA NA NA NA ...
#>  - attr(*, "periods")= int [1:3] 9 26 52
#>  - attr(*, "periodicity")= num 86400
#>  - attr(*, "ticker")= chr "sample_ohlc_data"
```

#### matrix_df()

Convert a matrix to ‘data.frame’. This function can be twice as fast as
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for a
matrix.

Takes the following arguments:

- `x` the matrix to convert to ‘data.frame’.
- `keep.attrs` (optional) if set to TRUE, will preserve any custom
  attributes set on the original object.

``` r
cloud <- ichimoku(sample_ohlc_data)
mcloud <- as.matrix(cloud)
df <- matrix_df(mcloud)
str(df)
#> 'data.frame':    281 obs. of  12 variables:
#>  $ open   : num  123 123 123 123 124 ...
#>  $ high   : num  123 123 123 124 125 ...
#>  $ low    : num  122 123 122 123 124 ...
#>  $ close  : num  123 123 123 124 125 ...
#>  $ cd     : num  -1 1 1 1 1 1 -1 0 -1 -1 ...
#>  $ tenkan : num  NA NA NA NA NA ...
#>  $ kijun  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouA: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouB: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ chikou : num  123 123 123 124 124 ...
#>  $ cloudT : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ cloudB : num  NA NA NA NA NA NA NA NA NA NA ...
str(row.names(df))
#>  chr [1:281] "2020-01-02" "2020-01-03" "2020-01-06" "2020-01-07" ...
```

## Dataframe Utilities

#### df_merge()

Full join on an arbitrary number of ‘data.frame’ objects passed as
arguments, preserving all unique entries. Can be used to combine
historical time series data where each observation is indexed by a
unique timestamp and all periods are complete.

Takes an arbitrary number of arguments:

- `...` data.frame objects to combine.

Can be used to join price dataframes retrieved by
[`oanda()`](https://shikokuchuo.net/ichimoku/dev/reference/oanda.md).
The function is designed to join complete historical data. If the data
to be merged contains data with incomplete periods, all entries are
preserved rather than updated. If incomplete periods are detected within
the data, a warning is issued, and the resulting dataframe should be
manually inspected in case it contains unwanted duplicates. Use
[`df_append()`](https://shikokuchuo.net/ichimoku/dev/reference/df_append.md)
for updating dataframes with new values.

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

#### df_append()

Update a ‘data.frame’ object with new data. Can be used to append new
updated time series data to an existing dataframe, where each
observation is indexed by a unique timestamp/identifier in a key column.

Takes 4 arguments:

- `old` data.frame object containing existing data.
- `new` data.frame object containing new data.
- `key` \[default ‘time’\] column name used as key provided as a
  character string.
- `keep.attr` \[default ‘timestamp’\] name of an attribute in ‘new’ to
  retain, if present, provided as a character string.

Can be used to update price dataframes retrieved by
[`oanda()`](https://shikokuchuo.net/ichimoku/dev/reference/oanda.md).
The function is designed to update existing data with new values as they
become available. As opposed to
[`df_merge()`](https://shikokuchuo.net/ichimoku/dev/reference/df_merge.md),
the data in ‘new’ will overwrite the data in ‘old’ rather than create
duplicates.

If the attribute specified by ‘keep.attr’ is present in ‘new’, for
example the ‘timestamp’ in pricing data returned by
[`oanda()`](https://shikokuchuo.net/ichimoku/dev/reference/oanda.md),
this is retained. If the attribute is not found in ‘new’, the argument
has no effect. All other custom attributes are dropped.

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

## Timestamp Utilities

#### format_POSIXct()

Formats a double POSIXct timestamp in the same way as the default
`fomat.POSIXct()`, without any additional customization options. This
provides a much faster implementation.

``` r
time <- Sys.time()
format(time)
#> [1] "2026-01-31 11:51:53"
format_POSIXct(time)
#> [1] "2026-01-31 11:51:53"
```

------------------------------------------------------------------------
