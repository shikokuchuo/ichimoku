# Set OANDA fxTrade API Key

Save OANDA fxTrade API key (personal access token) to the system
credential store.

## Usage

``` r
oanda_set_key()
```

## Value

Invisible NULL. A key is set in the default keyring under the service
name 'OANDA_API_KEY' for practice accounts or 'OANDA_LIVE_KEY' for live
accounts.

## Details

The key is read interactively. Separate keys can be set for practice and
live accounts - please choose the correct account type when prompted.

This function only needs to be called once to set the key; it does not
need to be called each session.

This function has a dependency on the 'keyring' package.

## Further Details

Please refer to the OANDA fxTrade API vignette by calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (interactive()) {
# Only run example in interactive R sessions
oanda_set_key()
}
```
