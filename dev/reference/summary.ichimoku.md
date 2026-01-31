# Summary of Ichimoku Objects and Strategies

Display summary information for an ichimoku object or its strategy.

## Usage

``` r
# S3 method for class 'ichimoku'
summary(object, strat = TRUE, ...)
```

## Arguments

- object:

  an object of class ‘ichimoku’.

- strat:

  \[default TRUE\] to show the strategy summary if present. Set to FALSE
  to show the object summary instead.

- ...:

  arguments passed to or from other methods.

## Value

A matrix containing the strategy summary, if present and ‘strat’ is set
to TRUE, otherwise a character vector containing an abbreviated object
summary (the full object summary is output to the console).

## Details

This function is an S3 method for the generic function summary() for
class ‘ichimoku’. It can be invoked by calling summary(x) on an object
‘x’ of class ‘ichimoku’.

Performs basic validation for an ichimoku object and will inform if an
ichimoku object contains invalid information.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
summary(cloud)
#> ichimoku object with dimensions (281, 12) 
#> 
#>             Max: 2020-07-13 23:00:00 [139.7]
#> Start: 2020-01-02 00:00:00 [123]   End: 2020-12-24 00:00:00 [136]
#>             Min: 2020-05-12 23:00:00 [119.1]
#> 
#> Cloud periods: 9 26 52 
#> Periodicity: 1 days 
#> Ticker: TKR 

strat <- strat(cloud)
summary(strat)
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
```
