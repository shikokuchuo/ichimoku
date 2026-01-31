# Display the Structure of Ichimoku Objects

Compactly display the internal structure of ichimoku objects.

## Usage

``` r
# S3 method for class 'ichimoku'
str(object, ...)
```

## Arguments

- object:

  an object of class ‘ichimoku’.

- ...:

  arguments passed to or from other methods.

## Value

Invisible NULL. A compact display of the structure of the object is
output to the console.

## Details

This function is an S3 method for the generic function str() for class
‘ichimoku’. It can be invoked by calling str(x) on an object ‘x’ of
class ‘ichimoku’.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
str(cloud)
#> ichimoku object [2020-01-02 00:00:00 / 2021-02-01 00:00:00] (281, 12)
#>  <double> $open $high $low $close $cd $tenkan $kijun $senkouA $senkouB $chikou $cloudT $cloudB
#>  index: <POSIXct> 2020-01-02 00:00:00 ... 2021-02-01 00:00:00 
#>  attributes:
#>   periods: 9 26 52 
#>   periodicity: 1 days 
#>   ticker: TKR 

strat <- strat(cloud)
str(strat)
#> ichimoku object [2020-01-02 00:00:00 / 2021-02-01 00:00:00] (281, 19) w/ strat
#>  <double> $open $high $low $close $cd $tenkan $kijun $senkouA $senkouB $chikou $cloudT $cloudB $cond $posn $txn $logret $slogret $ret $sret
#>  index: <POSIXct> 2020-01-02 00:00:00 ... 2021-02-01 00:00:00 
#>  attributes:
#>   periods: 9 26 52 
#>   periodicity: 1 days 
#>   ticker: TKR 
#>   strat: [strategy: close > tenkan w/ direction: long... ]
```
