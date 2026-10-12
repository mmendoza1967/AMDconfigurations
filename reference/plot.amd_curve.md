# Plot method for AMD curve objects

Plot method for AMD curve objects

## Usage

``` r
# S3 method for class 'amd_curve'
plot(x, type = c("curve", "mean", "max"), open_device = FALSE, ...)
```

## Arguments

- x:

  An object of class `"amd_curve"`.

- type:

  Curve to plot; see
  [`plot_AMD()`](https://mmendoza1967.github.io/AMDconfigurations/reference/plot_AMD.md).

- open_device:

  Logical; open a new graphics device before plotting.

- ...:

  Additional arguments passed to
  [`plot_AMD()`](https://mmendoza1967.github.io/AMDconfigurations/reference/plot_AMD.md).

## Value

Invisibly returns `NULL`.
