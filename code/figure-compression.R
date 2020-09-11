## This R script contains code for compressing the static figure in the manuscript
## Last run: September 11, 2020

# Load packages
library(zip)

# Save the bullet image used in the paper as a zip file
zip("figure-static/figure-08-1.png.zip", "figure-static/figure-08-1.png")
