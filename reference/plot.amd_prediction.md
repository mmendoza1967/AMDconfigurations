# Plot an optional AMD reader result

The fitted AMD curve remains the primary display. The numerical AMD
maximum and model-assisted proposal are shown separately. The analytical
`c = 1`, `AMD = 0` anchor is optional because including it changes the
vertical scale and can make the shape among `c = 2,...,14` less visible.

## Usage

``` r
# S3 method for class 'amd_prediction'
plot(x, show_c1 = FALSE, ...)
```

## Arguments

- x:

  An `amd_prediction` object.

- show_c1:

  Logical; show the unconnected analytical `c = 1` anchor.

- ...:

  Additional arguments passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `x`.
