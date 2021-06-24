
<!-- README.md is generated from README.Rmd. Please edit that file -->

# piechart

<!-- badges: start -->
<!-- badges: end -->

The goal of piechart is to draw a piechart simply and directly based on
‘ggplot2’.

## Installation

You can install piechart from [Github](https://github.com) with:

``` r
## install.packages("devtools")
devtools::install_github("Hy4m/piechart")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(piechart)
## random dataset
set.seed(20210515)
dd <- tibble::tibble(value = rpois(20, 5),
                     label = LETTERS[1:20])

## Ring
piechart(dd, aes(value = value, label = label)) +
  geom_pie(aes(fill = label), show.legend = FALSE) +
  geom_pie_text()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
## Pie
piechart(dd, aes(value = value, label = label), r0 = 0) +
  geom_pie(aes(fill = label), show.legend = FALSE) +
  geom_pie_text(facing = "clockwise", position = "top-inside")
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r
## sunburst
piechart(dd, aes(r1 = value, label = label), value = 1, r0 = 0.5) +
  geom_pie(aes(fill = label), show.legend = FALSE) +
  geom_pie_text(facing = "clockwise", position = "top-inside")
```

<img src="man/figures/README-example-3.png" width="100%" />

``` r
piechart(dd, aes(r1 = value, label = label), value = 1, r0 = 0.5,
         sort_by = "value") +
    geom_pie(aes(fill = value), show.legend = FALSE) +
    geom_pie_text(facing = "clockwise", position = "top-inside") +
    ggplot2::scale_fill_viridis_c()
```

<img src="man/figures/README-example-4.png" width="100%" />

``` r
## barchart
library(ggplot2)
ggplot(dd, aes(label, value)) +
  geom_bar(stat = "identity")
```

<img src="man/figures/README-example-5.png" width="100%" />

``` r
piechart(dd, aes(r1 = scales::rescale(value, c(0.5, 1), range(value)),  label = label), 
         value = 1, r0 = 0.4, sep = 1, sum_value = 60, start = 30.5) +
    anno_rect(0.4, 1.05, 30, 150, fill = "grey95", colour = "grey35", size = 0.4) +
    geom_pie(fill = "pink") +
    geom_pie_text(facing = "binding", position = "top-inside") +
    anno_rho_axis(breaks = 1:20,
                  labels = LETTERS[1:20],
                  scale = c(.5, 20.5), 
                  start = 30.5, 
                  end = 149.5,
                  r = 0.4) +
    anno_alpha_axis(breaks = scales::rescale(c(2, 4, 6, 8), 
                                             c(0.5, 1), range(dd$value)),
                    labels = c(2, 4, 6, 8),
                    scale = c(0.5, 1),
                    r0 = 0.4, 
                    r1 = 1,
                    angle = 150) 
```

<img src="man/figures/README-example-6.png" width="100%" />

``` r
## arc heatmap
library(magrittr)
as.matrix(iris[-5]) %>% 
  as_piechart_data(start = 100, open = 20, r0 = 0.8) %>% 
  piechart(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
  geom_pie(aes(fill = value), colour = "white", size = 0.3) +
  geom_rtext(position = "top-outside", size = 2.5) +
  geom_ctext(hjust = "middle", size = 2.5) +
  scale_fill_viridis_c()
```

<img src="man/figures/README-example-7.png" width="100%" />
