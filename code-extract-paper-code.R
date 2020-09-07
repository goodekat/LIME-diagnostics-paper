## Code for extracting the R code from the file paper.Rnw to be used for submission
## Created: September 9, 2020

knitr::purl("paper.Rnw", output = "code-paper.R")
