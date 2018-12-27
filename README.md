---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# REAC Reaction Times Analysis in Attention Research

The REAC package provides tools for

  - preparing your data for additional calculations related to
    **ABV-index** (*Attention Bias Variability*) and

  - data cleaning techniques such as winsorizing your data.

## Installation

You can install the released version of abvyr from
[CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("tamasSmahajcsikszabo/REAC")
```

### Functions

#### the shaker() function

The shaker() function transposes and organizes your data into a tidy
format (observations are rows, variables are columns).

``` r
shaker(example, 
  grouping = c("year", "subject"), 
  gather = "trial", 
  value = "RT")
```

#### the ABV() function

The ABV() function calculates the Attention Bias Variability Index.
One of the key inputs from the user is the width of bins which impacts
the width of the calculation.

``` r
ABV(example2, 
  grouping = c("year", "subject"), 
  trial = "trial", 
  bin_width = 3, 
  value = "RT", 
  label = "label", 
  type = c("Incongruent", "Congruent"), 
  ID = "subject")
```

#### the winzer() function

The winzer() function winsorizes your data with predefined quantiles
values for lower and upper bound.

``` r
winzer(example, 
  grouping = c("year", "subject"), 
  x = 0.25, 
  y = 0.75, 
  z = 1.5, 
  label = "trial", 
  value = "RT")
```

## Effect of the winzer() on a simulated ex-Gaussian reaction time dataset

Example code for plotting the original data and the winsozized dataset

* winsorization sweep is set to broad:

* it takes the 5th and 95th quantiles as starting points (see the "winsorized" dataset declaration, 'x' and 'y' parameters)

* and it sweeps with 2x multiplier to the IQR ('z' parameter)

```r
## the original dataset shaped
data <- shaker(REAC::example, grouping = c("year", "subject"), gather = "trial", value = "RT")

## the process of winsorization
winsorized <- winzer(data, grouping = c("year", "subject"), x = 0.05, y = 0.95, z = 2, value = "RT", label = "trial") %>% 
  shaker(grouping = c("year", "subject"), gather = "trial", value = "RT")

```


