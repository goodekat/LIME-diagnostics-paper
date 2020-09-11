## Steps to follow for ASA Data Science Journal submission
## Last Run: September 11, 2020

# Load packages
library(zip)

## Follow these steps to make sure all guidelines are met for submission:

# ----------------------------------------------------------------------------
# Step 1
# ----------------------------------------------------------------------------

# Knit the file paper.Rnw (to make sure everything is up to date).

# ----------------------------------------------------------------------------
# Step 2
# ----------------------------------------------------------------------------

# Run the following code to move all files necessary to the submission folder.

# Create a submission folder file path
submission_folder <- "submission"

# DATA AND CODE

# Create a temporary directory for storing code and data files for submission
code_data_folder = "code-and-data"
dir.create(code_data_folder) 

# Extract the R code from the files paper.Rnw to be used for submission
knitr::purl("paper.Rnw", output = paste0(code_data_folder, "/paper.R"))

# Create a vector with code and data files to be included for submission 
data_files <- c("readme.txt", "data/bullet-test.csv", "data/bullet-train.csv", 
                "data/example-signatures.csv")

# Copy the submission files to the temporary folder
file.copy(data_files, "code-and-data")

# Compress the folder with data and code for submission and store it in the 
# submission folder
zip(paste0(submission_folder, "/code-and-data.zip"), "code-and-data")

# Remove the temporary folder with code and data for submission
unlink(code_data_folder, recursive = TRUE)

# FIGURES 

# Identify the folders with files for submission
folder_eps_figures <- "figure"
folder_static_figures <- "figure-static"

# Find the EPS figure files for submission
list_of_generated_figures <- list.files(folder_eps_figures, ".eps$", full.names = T)
list_of_static_figures <- list.files(folder_static_figures, ".eps$", full.names = T)

# Copy files to the submission folder
file.copy(list_of_generated_figures, submission_folder)
file.copy(list_of_static_figures, submission_folder)

# LATEX FILES

# Copy files to the submission folder
file.copy("paper.tex", submission_folder)
file.copy("paper.pdf", submission_folder)
file.copy("references.bib", submission_folder)
file.copy("WileyNJD-AMS.bst", submission_folder)
file.copy("WileyNJD-v2.cls", submission_folder)

# ----------------------------------------------------------------------------
# Step 3
# ----------------------------------------------------------------------------

# Manually delete figure/ and figure-static/ from the figure file paths in 
# paper.tex. For example, change \includegraphics[width=6.5in]{figure/figure-01-1} 
# to \includegraphics[width=6.5in]{figure-01-1}.

# ----------------------------------------------------------------------------
# Step 4
# ----------------------------------------------------------------------------

# Compile the file paper-submission.tex (using LaTex, then bibTex, then LaTeX 
# twice more) to make sure it compiles correctly (especially references).

# ----------------------------------------------------------------------------
# Step 5
# ----------------------------------------------------------------------------

# Besides for paper.pdf, delete all other files generated when paper.tex was 
# compiled. This includes other paper files and pdf versions of the figures.

# ----------------------------------------------------------------------------
# Step 6
# ----------------------------------------------------------------------------

# Upload all remaining files to ASA Data Science Journal submission page 
# which should be:
#   - code-and-data.zip
#   - figure-01-1.eps through figure-C3-1.eps
#   - paper.pdf
#   - paper.tex
#   - references.bib
#   - WileyNJD-AMS.bst
#   - WileyNJD-v2.cls
