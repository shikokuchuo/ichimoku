# Automated Ichimoku Strategies

Generate a list of the top performing ichimoku cloud strategies based on
simple indicator conditions of the form 'c1 \> c2' (level 1), complex
combined strategies of the form 'c1 \> c2 & c3 \> c4' (level 2), or
complex asymmetric strategies of the form 'c1 \> c2 x c3 \> c4' (level
3).

## Usage

``` r
autostrat(x, n = 8, dir = c("long", "short"), level = 1, quietly)
```

## Arguments

- x:

  an ichimoku object.

- n:

  \[default 8\] select top 'n' number of strategies to return.

- dir:

  \[default 'long'\] trade direction, either ‘long’ or ‘short’.

- level:

  \[default 1\] to return simple strategies. For complex strategies, set
  level to 2 to return combined strategies of the form 's1 & s2' or
  level to 3 to return asymmetric strategies of the form 's1 x s2'.

- quietly:

  (optional) if set to TRUE, will suppress printing of additional output
  to the console and return quietly.

## Value

Returned invisibly, a list of ‘n’ ichimoku objects containing
strategies, with attributes ‘logret’ (a vector of cumulative log returns
for all strategies) and ‘summary’ (a matrix of summaries for the top ‘n’
strategies).

In addition, the strategy summaries are printed to the console.

## Details

Ichimoku objects for each strategy are returned as a list. The
cumulative log returns for all strategies as well as the summaries for
the top ‘n’ strategies are saved as attributes to the list. This
information may be retrieved by using
[`look`](https://shikokuchuo.net/ichimoku/dev/reference/look.md) on the
returned list.

Each individual ichimoku object may be accessed via its position in the
list, e.g. \[\[1\]\] for the 1st item.

## Further Details

Please refer to the strategies vignette by calling:
[`vignette("strategies", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/strategies.md)

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")

stratlist <- autostrat(cloud, n = 3, quietly = TRUE)
look(stratlist)
#> $logret
#>  senkouB_tenkan   cloudB_tenkan   senkouB_kijun    cloudB_kijun      low_tenkan 
#>     0.144869048     0.132721126     0.124552623     0.124476092     0.119988595 
#>    cloudB_close     high_tenkan      cloudB_low   senkouB_close    cloudT_kijun 
#>     0.117048881     0.112062056     0.110900734     0.110699399     0.103153955 
#>   senkouA_kijun     senkouB_low   cloudT_tenkan     cloudT_high       low_kijun 
#>     0.103077424     0.101480718     0.093961798     0.088759707     0.088503030 
#>    senkouB_high  chikou_senkouA      high_kijun  senkouA_tenkan    close_tenkan 
#>     0.086708526     0.084344426     0.083373239     0.081813876     0.079948752 
#>   tenkan_chikou    senkouA_high     cloudB_high     senkouA_low   senkouA_close 
#>     0.078961923     0.076693196     0.074642015     0.073761187     0.073712567 
#>     close_kijun   chikou_cloudT    cloudT_close    kijun_chikou      cloudT_low 
#>     0.071380845     0.069192621     0.067363085     0.066288776     0.064341171 
#>   chikou_cloudB    chikou_close    kijun_tenkan      chikou_low  chikou_senkouB 
#>     0.064291073     0.062225816     0.055091789     0.053536566     0.049139267 
#> senkouA_senkouB  senkouB_chikou     chikou_high      low_chikou     high_chikou 
#>     0.048655509     0.045949698     0.042730532     0.041552399     0.034326207 
#>    close_chikou   cloudB_chikou      low_cloudT    tenkan_kijun    chikou_kijun 
#>     0.032863149     0.030797893     0.030747795     0.030128362     0.028800189 
#>   cloudT_chikou    close_cloudT     kijun_close senkouB_senkouA    tenkan_close 
#>     0.025896344     0.024713830     0.023708120     0.023618778     0.023402736 
#>   close_senkouA     low_senkouA     high_cloudB    high_senkouA  tenkan_senkouA 
#>     0.021376398     0.021327779     0.020446951     0.018395770     0.013275089 
#>  senkouA_chikou   chikou_tenkan    high_senkouB       kijun_low     high_cloudT 
#>     0.010744539     0.010161327     0.008380439     0.006585935     0.006329258 
#>   tenkan_cloudT      kijun_high     low_senkouB     tenkan_high   kijun_senkouA 
#>     0.001127167    -0.003436078    -0.006391752    -0.007150571    -0.007988459 
#>    kijun_cloudT      low_cloudB      tenkan_low   close_senkouB    close_cloudB 
#>    -0.008064990    -0.015811768    -0.015890560    -0.018622484    -0.021959916 
#>   tenkan_cloudB    kijun_cloudB   kijun_senkouB  tenkan_senkouB 
#>    -0.037632160    -0.040858512    -0.040935043    -0.049780083 
#> 
#> $summary
#>                        [,1]                [,2]               
#> Strategy               "senkouB > tenkan"  "cloudB > tenkan"  
#> ---------------------  "----------"        "----------"       
#> Strategy cuml return % 17.49               16.08              
#> Per period mean ret %  0.0906              0.0838             
#> Periods in market      63                  51                 
#> Total trades           3                   3                  
#> Average trade length   21                  17                 
#> Trade success %        100                 100                
#> Worst trade ret %      3.64                3.16               
#> ---------------------  "----------"        "----------"       
#> Benchmark cuml ret %   5.53                5.53               
#> Per period mean ret %  0.0302              0.0302             
#> Periods in market      178                 178                
#> ---------------------  "----------"        "----------"       
#> Direction              "long"              "long"             
#> Start                  2020-04-19 23:00:00 2020-04-19 23:00:00
#> End                    2020-12-23          2020-12-23         
#> Ticker                 "TKR"               "TKR"              
#>                        [,3]               
#> Strategy               "senkouB > kijun"  
#> ---------------------  "----------"       
#> Strategy cuml return % 14.1               
#> Per period mean ret %  0.0741             
#> Periods in market      64                 
#> Total trades           3                  
#> Average trade length   21.33              
#> Trade success %        100                
#> Worst trade ret %      3.49               
#> ---------------------  "----------"       
#> Benchmark cuml ret %   5.53               
#> Per period mean ret %  0.0302             
#> Periods in market      178                
#> ---------------------  "----------"       
#> Direction              "long"             
#> Start                  2020-04-19 23:00:00
#> End                    2020-12-23         
#> Ticker                 "TKR"              
#> 
strat <- stratlist[[2]]
summary(strat)
#>                        [,1]               
#> Strategy               "cloudB > tenkan"  
#> ---------------------  "----------"       
#> Strategy cuml return % 16.08              
#> Per period mean ret %  0.0838             
#> Periods in market      51                 
#> Total trades           3                  
#> Average trade length   17                 
#> Trade success %        100                
#> Worst trade ret %      3.16               
#> ---------------------  "----------"       
#> Benchmark cuml ret %   5.53               
#> Per period mean ret %  0.0302             
#> Periods in market      178                
#> ---------------------  "----------"       
#> Direction              "long"             
#> Start                  2020-04-19 23:00:00
#> End                    2020-12-23         
#> Ticker                 "TKR"              

autostrat(cloud, n = 1, dir = "short", level = 2)
#>                        [,1]                               
#> Strategy               "close > chikou & tenkan > senkouB"
#> ---------------------  "----------"                       
#> Strategy cuml return % 11.21                              
#> Per period mean ret %  0.0597                             
#> Periods in market      20                                 
#> Total trades           4                                  
#> Average trade length   5                                  
#> Trade success %        75                                 
#> Worst trade ret %      -0.44                              
#> ---------------------  "----------"                       
#> Benchmark cuml ret %   -5.24                              
#> Per period mean ret %  -0.0302                            
#> Periods in market      178                                
#> ---------------------  "----------"                       
#> Direction              "short"                            
#> Start                  2020-04-19 23:00:00                
#> End                    2020-12-23                         
#> Ticker                 "TKR"                              
autostrat(cloud, n = 1, dir = "long", level = 3)
#>                        [,1]                             
#> Strategy               "senkouB > senkouA x kijun > low"
#> ---------------------  "----------"                     
#> Strategy cuml return % 2.49                             
#> Per period mean ret %  0.0138                           
#> Periods in market      59                               
#> Total trades           3                                
#> Average trade length   19.67                            
#> Trade success %        66.67                            
#> Worst trade ret %      -2.49                            
#> ---------------------  "----------"                     
#> Benchmark cuml ret %   5.53                             
#> Per period mean ret %  0.0302                           
#> Periods in market      178                              
#> ---------------------  "----------"                     
#> Direction              "long"                           
#> Start                  2020-04-19 23:00:00              
#> End                    2020-12-23                       
#> Ticker                 "TKR"                            
```
