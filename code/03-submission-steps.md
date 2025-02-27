Sumbission Steps
================
Katherine Goode
<br>January 07, 2021

This document contains the steps to follow to make sure all guidelines
are met for submission to the ASA Data Science Journal.

**NOTE** The knitr option of `eval = FALSE` has been set for this
document, so all code must be run manually from within the Rmd file.
This is done to be safe about not deleting or replacing files.

0.  Run the code if desired to update the saved state of the project R
    package versions. Use with caution\!
    
    ``` r
    # Save the r package versions
    renv::snapshot()
    ```

1.  Delete all data files created from the raw versions, files generated
    by `paper.Rnw`, and current existing submission folder using the
    code below.
    
    ``` r
    # Specify date of submission folder
    submission_date = "2021-01-07"
    
    # Create a submission folder file path:
    submission_folder <- paste0("../submissions/", submission_date)
    
    # Individual files to delete
    file.remove("../data/bullet-explain.rds")
    file.remove("../data/bullet-explain-perms-clean.rds")
    file.remove("../data/bullet-lime.rds")
    file.remove("../data/bullet-rf.rds")
    file.remove("../data/bullet-test.csv")
    file.remove("../data/bullet-test.csv.zip")
    file.remove("../data/bullet-train.csv")
    file.remove("../data/bullet-train.csv.zip")
    file.remove("../data/example-signatures.csv")
    file.remove("../data/example-signatures.csv.zip")
    file.remove("../data/sine-data-test.rds")
    file.remove("../data/sine-data-train.rds")
    file.remove("../data/sine-lime-explain.rds")
    file.remove("../data/raw/h224-set1-features.rds")
    file.remove("../data/raw/h224-set1-features.rds.zip")
    file.remove("../data/raw/h224-set11-features.rds")
    file.remove("../data/raw/h224-set11-features.rds.zip")
    file.remove("../data/raw/CCFs_withlands.csv")
    file.remove("../data/raw/CCFs_withlands.csv.zip")
    file.remove("../data/raw/signatures.rds")
    file.remove("../data/raw/signatures.rds.zip")
    file.remove("../figure-static/figure-06-1.eps")
    file.remove("../figure-static/figure-06-1.png.zip")
    file.remove("../figure-static/figure-08-1.eps")
    file.remove("../figure-static/figure-08-1.png")
    
    # Folders to delete
    unlink("../figure", recursive = TRUE)
    unlink("../support-info/figure", recursive = TRUE)
    unlink(submission_folder, recursive = TRUE) 
    ```

2.  Manually run the code in `code/01-raw-file-compression.Rmd` to add
    compressed versions of the raw data files and to compress the static
    figure for the manuscript. Also, knit the file to make sure the md
    version is up to date.

3.  Manually run the code in `code/02-data-preparation.Rmd` to create
    the versions of the data to be used in the manuscript and submitted
    with the manuscript. Also, knit the file to make sure the md version
    is up to date.

4.  Knit the file `paper.Rnw`.

5.  Knit the supporting information document
    `support-info/supporting-information.Rnw`.

6.  Run the following code to move all files necessary to the submission
    folder.
    
    ``` r
    # Load package
    library(zip)
    
    # If the submission folder does not exist, create one
    if (!dir.exists(submission_folder)) dir.create(submission_folder) 
    
    # DATA AND CODE
    
    # Create a temporary directory for storing code and data files for submission
    code_data_folder = "code-and-data"
    dir.create(code_data_folder) 
    
    # Extract the R code from the files paper.Rnw to be used for submission
    knitr::purl("../paper.Rnw", output = paste0(code_data_folder, "/code.R"))
    
    # Create a vector with code and data files to be included for submission 
    data_files <-
      c(
        "../data/README.txt",
        "../data/bullet-test.csv",
        "../data/bullet-train.csv",
        "../data/example-signatures.csv"
      )
    
    # Copy the submission files to the temporary folder
    file.copy(data_files, code_data_folder)
    
    # Compress the folder with data and code for submission and store it in the 
    # submission folder
    zip(paste0(submission_folder, "/code-and-data.zip"), code_data_folder)
    
    # Remove the temporary folder with code and data for submission
    unlink(code_data_folder, recursive = TRUE)
    
    # FIGURES 
    
    # Identify the folders with files for submission
    folder_eps_figures <- "../figure"
    folder_static_figures <- "../figure-static"
    
    # Find the EPS figure files for submission
    list_of_generated_figures <-
      list.files(folder_eps_figures, ".eps$", full.names = T)
    list_of_static_figures <-
      list.files(folder_static_figures, ".eps$", full.names = T)
    
    # Copy files to the submission folder
    file.copy(list_of_generated_figures, submission_folder)
    file.copy(list_of_static_figures, submission_folder)
    
    # LATEX FILES
    
    # Copy files to the submission folder
    file.copy("../paper.tex", submission_folder)
    file.copy("../paper.pdf", submission_folder)
    file.copy("../references.bib", submission_folder)
    file.copy("../WileyNJD-AMS.bst", submission_folder)
    file.copy("../WileyNJD-v2.cls", submission_folder)
    
    # SUPPORTING INFORMATION
    
    # Copy the supporting information pdf to the submission folder
    file.copy("../support-info/supporting-information.pdf", submission_folder)
    ```

7.  Manually delete `figure/` and `figure-static/` from the figure file
    paths in the submissions folder `paper.tex`. For example, change
    `\includegraphics[width=6.5in]{figure/figure-01-1}` to
    `\includegraphics[width=6.5in]{figure-01-1}`.

8.  Compile the file in the submissions folder `paper.tex` to make sure
    it compiles correctly (especially references). (If using texShop,
    make sure to compile using LaTex, then bibTex, then LaTeX twice
    more.)

9.  Besides for `paper.pdf`, delete all other files generated when
    `paper.tex` was compiled. This includes other paper files and pdf
    versions of the figures.
    
    ``` r
    # Remove the paper files that may have been generated during compilation
    file.remove(paste0(submission_folder, "/paper.aux"))
    file.remove(paste0(submission_folder, "/paper.bbl"))
    file.remove(paste0(submission_folder, "/paper.blg"))
    file.remove(paste0(submission_folder, "/paper.log"))
    file.remove(paste0(submission_folder, "/paper.synctex.gz"))
    file.remove(paste0(submission_folder, "/paper.pag"))
    
    # Remove the pdf versions of the figures generated during compilation
    file.remove(list.files(
      path = submission_folder,
      pattern = "converted-to",
      full.names = TRUE
    ))
    ```

10. Upload all remaining files in the submission folder to the ASA Data
    Science Journal submission page which should be:

<!-- end list -->

  - `code-and-data.zip`
  - `figure-01-1.eps` through `figure-B1-1.eps`
  - `paper.pdf`
  - `paper.tex`
  - `references.bib`
  - `supporting-information.pdf`
  - `WileyNJD-AMS.bst`
  - `WileyNJD-v2.cls`

<!-- end list -->

11. Use latexdiff to generate a pdf showing the differences between the
    this submission and the previous one. To do this, (i) run the R code
    below, (ii) run the code `latexdiff submissions/2020-10-07/paper.tex
    submissions/2021-01-07/paper.tex >
    submissions/2021-01-07/diff/2020-10-07-to-2021-01-07.tex` in the
    terminal (changes dates in the code as needed), and (iii) knit the
    generated tex file to make sure the pdf with the changes looks good.
    
    ``` r
    # Create folder to place the diff
    diff_folder = paste0("../submissions/", submission_date, "/diff")
    dir.create(diff_folder)
    
    # Create folders within diff to store the figures needed
    dir.create(paste0(diff_folder, "/figure"))
    dir.create(paste0(diff_folder, "/figure-static"))
    
    # Copy necessary files into the diff folder needed for the diff to work
    file.copy("../references.bib", diff_folder)
    file.copy("../WileyNJD-AMS.bst", diff_folder)
    file.copy("../WileyNJD-v2.cls", diff_folder)
    file.copy(list_of_generated_figures, paste0(diff_folder, "/figure"))
    file.copy(list_of_static_figures, paste0(diff_folder, "/figure-static"))
    ```

12. Knit this document to make sure the md version is up to date.
