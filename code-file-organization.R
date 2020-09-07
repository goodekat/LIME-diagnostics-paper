## Code for organizing files associated with the paper
## Last Run: September 7, 2020

# Compressing files for GitHub ------------------------------------------

# Save the static figure that is used in the paper as a zip file
zip::zip("figure-08-1.png.zip", "figure-08-1.png")

# Save the bullet training and testing datasets a zip files
zip::zip("data-bullet-train.csv.zip", "data-bullet-train.csv")
zip::zip("data-bullet-test.csv.zip", "data-bullet-test.csv")

# Save the dataset with two example matching signatures as a zip file
zip::zip("data-example-signatures.csv.zip", "data-example-signatures.csv")

# Save the dataset with observations with tank rash as a zip file
zip::zip("data-no-tank-rash.csv.zip", "data-no-tank-rash.csv")

# Preparing files for submission ----------------------------------------

# Extract the R code from the file paper.Rnw to be used for submission
knitr::purl("paper.Rnw", output = "code-paper.R")
