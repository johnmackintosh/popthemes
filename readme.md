popthemes
================

# <img src="man/figures/logo.png" width="160px" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/johnmackintosh/popthemes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johnmackintosh/popthemes/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## What?

This is a collection of colour palettes based (mainly) on pop album
covers.

Featuring:

-   Aqua
-   Bangles
-   Beck
-   Betty Boo
-   B\*Witched
-   Deee Lite
-   Hole
-   No Doubt
-   Rick Astley
-   S Club 7
-   Spice Girls
-   Steps

## Why?

Because [I’ve done
Metallica](https://github.com/johnmackintosh/metallicaRt) and [classic
rock](https://github.com/johnmackintosh/rockthemes).

The rockthemes has some pop acts featured in it as well, including No
Doubt and DeeeLite. They’re included here - and the palettes have been
tweaked slightly.

## Installation

This won’t be appearing on CRAN, so please install using the remotes
package.

``` r
#library(remotes)
#remotes::install_github("johnmackintosh/popthemes")
library(popthemes)
library(ggplot2)
library(dplyr)
library(scales)
library(gapminder)
```

# Palettes

``` r
pop_palette("aqua")
pop_palette("bangles")
pop_palette("beck")
```

<img src="man/figures/README-aqua-1.png" width="33%" /><img src="man/figures/README-aqua-2.png" width="33%" /><img src="man/figures/README-aqua-3.png" width="33%" />

``` r
pop_palette("boo")
pop_palette("bwitched")
pop_palette("deeelite")
```

<img src="man/figures/README-boo-1.png" width="33%" /><img src="man/figures/README-boo-2.png" width="33%" /><img src="man/figures/README-boo-3.png" width="33%" />

``` r
pop_palette("hole")
pop_palette("nodoubt")
pop_palette("rickroll")
```

<img src="man/figures/README-deeelite-1.png" width="33%" /><img src="man/figures/README-deeelite-2.png" width="33%" /><img src="man/figures/README-deeelite-3.png" width="33%" />

``` r
pop_palette("sclub7")
pop_palette("spice")
pop_palette("steps")
```

<img src="man/figures/README-sclub-1.png" width="33%" /><img src="man/figures/README-sclub-2.png" width="33%" /><img src="man/figures/README-sclub-3.png" width="33%" />

## ggplot2 use

Colour and fills for ggplot2.

<img src="man/figures/README-unnamed-chunk-2-1.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-2.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-3.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-4.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-5.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-6.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-7.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-8.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-9.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-10.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-11.png" width="33%" /><img src="man/figures/README-unnamed-chunk-2-12.png" width="33%" />

## Credit

[Thanks to Ryo for the tvthemes
package](https://github.com/Ryo-N7/tvthemes) which helped me get this
off the ground quickly
