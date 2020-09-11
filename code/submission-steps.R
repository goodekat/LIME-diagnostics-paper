## Steps to follow for ASA Data Science Journal submission
## Last Run: September 11, 2020

# Load packages
library(zip)

# Preparing files for submission ----------------------------------------

# Create a submission folder file path
submission_folder <- "submission"

## DATA AND CODE

# Create a temporary directory for storing code and data files for submission
code_data_folder = "code-and-data"
dir.create(code_data_folder) 

# Extract the R code from the file paper.Rnw to be used for submission
knitr::purl("paper.Rnw", output = paste0(code_data_folder, "/paper.R"))

# Create a vector with code and data files to be included for submission 
data_files <-
  c(
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
