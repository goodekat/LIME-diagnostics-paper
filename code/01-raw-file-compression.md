Compression of Raw Files
================
Katherine Goode
<br>January 07, 2021

This document contains code for compressing the raw files (data and
figure) associated with the manuscript.

**NOTE** All code chunks in this document have `eval = FALSE` and must
be run manually from within the Rmd file. This is done to avoid an
accidental change of files.

``` r
# Load packages
library(zip)
```

Save the bullet image used in the paper as a zip file:

``` r
zip(
  zipfile = "../figure-static/figure-06-1.png.zip",
  files =  "../figure-static/figure-06-1.png"
)
```

Save the raw bullet training data files as zip files:

``` r
zip(
  zipfile = "../data/raw/CCFs_withlands.csv.zip", 
  files = "../../data/raw/CCFs_withlands.csv"
)
```

Save the raw bullet testing data files as zip files:

``` r
zip(
  zipfile = "../data/raw/h224-set1-features.rds.zip", 
  files = "../../data/raw/h224-set1-features.rds"
)
zip(
  zipfile = "../data/raw/h224-set11-features.rds.zip", 
  files = "../../data/raw/h224-set11-features.rds"
)
```

Save the raw data with an example of matching signatures as a zip file:

``` r
zip(
  zipfile = "../data/raw/signatures.rds.zip", 
  files = "../../data/raw/signatures.rds"
)
```
