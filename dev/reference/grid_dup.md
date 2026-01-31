# Duplicates of expand.grid for 2 Variables

Create a vector of element positions of duplicates in the output of
expand.grid on 2 identical vectors. An efficient method of creating
combinations for 2 variables.

## Usage

``` r
grid_dup(n, omit.id)
```

## Arguments

- n:

  the length of vector passed to
  [`expand.grid()`](https://rdrr.io/r/base/expand.grid.html).

- omit.id:

  (optional) set to TRUE to also select the elements where the 2 items
  are identical. The output of expand.grid, subset to remove duplicates
  with 'omit.id' set to TRUE would be the equivalent of
  `utils::combn(n, 2)`.

## Value

A numeric vector.

## Examples

``` r
n <- 3
expand.grid(1:n, 1:n)
#>   Var1 Var2
#> 1    1    1
#> 2    2    1
#> 3    3    1
#> 4    1    2
#> 5    2    2
#> 6    3    2
#> 7    1    3
#> 8    2    3
#> 9    3    3
expand.grid(1:n, 1:n)[-grid_dup(n), ]
#>   Var1 Var2
#> 1    1    1
#> 2    2    1
#> 3    3    1
#> 5    2    2
#> 6    3    2
#> 9    3    3
expand.grid(1:n, 1:n)[-grid_dup(n, omit.id = TRUE), ]
#>   Var1 Var2
#> 2    2    1
#> 3    3    1
#> 6    3    2
```
