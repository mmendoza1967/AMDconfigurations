# Plot an AMD curve

Plots the primary AMD curve returned by
[`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md).
Historical restart summaries remain available as diagnostic
alternatives.

## Usage

``` r
plot_AMD(
  amd_object,
  type = c("curve", "mean", "max"),
  open_device = FALSE,
  ...
)
```

## Arguments

- amd_object:

  An object returned by
  [`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md).

- type:

  Curve to plot: `"curve"` (default) uses the minimum-objective
  converged fit at every `c`; `"mean"` and `"max"` show historical
  summaries across all random starts, including nonconverged starts.
  They are retained only for diagnostics and compatibility.

- open_device:

  Logical; open a new graphics device before plotting. The default uses
  the active device, as is conventional for S3 plot methods.

- ...:

  Additional graphical parameters passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## Details

Objects saved by public version 0.1.0 can still be plotted from their
stored `mean` or `max` restart summaries. Because those objects do not
contain the minimum-objective primary curve, the method warns and does
not attempt to reconstruct it.
