## Code for organizing files associated with the paper
## Last Run: September 11, 2020

# Load packages
library(zip)

# Compressing files for GitHub ------------------------------------------

# Save the static figure that is used in the paper as a zip file
zip("figure-static/figure-08-1.png.zip", "figure-static/figure-08-1.png")

# Save the bullet training and testing datasets a zip files
zip("data/data-bullet-train.csv.zip", "data/data-bullet-train.csv")
zip("data/data-bullet-test.csv.zip", "data/data-bullet-test.csv")

# Save the dataset with two example matching signatures as a zip file
zip("data/data-example-signatures.csv.zip", "data/data-example-signatures.csv")

# Preparing files for submission ----------------------------------------

# Create a submission folder file path
submission_folder <- "submission"

## DATA AND CODE

# Extract the R code from the file paper.Rnw to be used for submission
knitr::purl("paper.Rnw", output = "code/paper.R")

# Create a temporary directory for storing code and data files for submission
code_data_folder = "code-and-data"
dir.create(code_data_folder) 

# Create a vector with code and data files to be included for submission 
code_data_files <-
  c(
    "code/paper.R",
    "data/bullet-test.csv",
    "data/bullet-train.csv",
    "data/example-signatures.csv"
  )

# Copy the submission files to the temporary folder
file.copy(code_data_files, "code-and-data")

# Compress the folder with data and code for submission and store it 
# in the submission folder
zip(paste0(submission_folder, "/code-and-data.zip"), "code-and-data")

# Remove the temporary folder with code and data for submission
unlink(code_data_folder, recursive = TRUE)

## FIGURES 

# Identify the folders with files for submission
folder_eps_figures <- "figure"
folder_static_figures <- "figure-static"

# Find the EPS figure files for submission
list_of_generated_figures <- list.files(folder_eps_figures, ".eps$", full.names = T)
list_of_static_figures <- list.files(folder_static_figures, ".eps$", full.names = T)

# Copy files to the submission folder
file.copy(list_of_generated_figures, submission_folder)
file.copy(list_of_static_figures, submission_folder)

## LATEX FILES

# Copy files to the submission folder
file.copy("paper.tex", submission_folder)
file.copy("paper.pdf", submission_folder)
file.copy("references.bib", submission_folder)
file.copy("WileyNJD-AMS.bst", submission_folder)
file.copy("WileyNJD-v2.cls", submission_folder)
