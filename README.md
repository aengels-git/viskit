
<!-- README.md is generated from README.Rmd. Please edit that file -->

# viskit

<!-- badges: start -->
<!-- badges: end -->

The goal of viskit is to make visualizations with ggplot quick and easy

## Installation

You can install the development version of viskit from
<https://github.com/aengels-git/viskit> with:

``` r
# install.packages("devtools")
devtools::install_github("aengels-git/viskit")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(viskit)
tab <- diamonds%>%group_by(cut)%>%summarise(n=n())
#> `summarise()` ungrouping output (override with `.groups` argument)
vis_barplot(tab, x=cut, y=n, fill=cut, scaling=comma)+
  theme(legend.position = "bottom")
```

<img src="man/figures/README-example-1.png" width="100%" />
