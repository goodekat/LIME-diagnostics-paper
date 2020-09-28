
---------------------------------------------------
INFORMATION ON THE CODE AND DATA FILES
---------------------------------------------------

Below are descriptions of the code and data files submitted with the manuscript "Visual Diagnostics of a Model Explainer -- Tools for the Assessment of LIME Explanations" by Katherine Goode and Heike Hofmann.


#### manuscript-code.r ####

Contains all of the code used to produce the results and visualizations in the manuscript. Note, this file assumes the data files are contained in a folder called data and this file is in a folder above the data folder.

#### bullet-train.csv.zip ####

The bullet-train data has 83,028 rows and 13 columns that contain comparison features described in Hare, Hofmann, and Carriquiry (2017) based on high resolution microscopy scans of fired bullets from Hamby sets 173 and 252 (Hamby, Brundage, and Thorpe 2009). This dataset is created from the x3p scans of bullet land engraved areas available from the NIST Ballistics Toolmark Research Database (https://tsapps.nist.gov/NRBTD/). It contains comparisons from 408 bullet-land signatures. 12 of the overall 420 lands (6 lands per bullets, 35 bullets in each set) are excluded from the comparison. Six of these lands show so-called “tank rash” - damage to the bullets after it exited the barrel (see https://github.com/goodekat/LIME-diagnostics-paper/blob/master/LEAscans/tankrash.md). Another bullet (Bullet E from Hamby 173) is excluded because it could not be matched visually to the barrel it was supposedly from (see https://github.com/goodekat/LIME-diagnostics-paper/blob/master/LEAscans/bullete.md). bullet-train is generated from the raw file of comparison features, which is found at https://github.com/goodekat/LIME-diagnostics-paper/blob/master/data/raw/CCFs_withlands.zip. The steps taken to create bullet-train from the raw data are found at https://github.com/goodekat/LIME-diagnostics-paper/blob/master/code/02-data-preparation.Rmd. These steps involve renaming some variables, selecting the variables of interest for the manuscript, and adjusting the land IDs associated with the signatures. The variables in the data are described below. Further descriptions of the comparison features are found in Hare, Hofmann, and Carriquiry (2017).

Variables used as key variables:

- case: ID number associated with the bullet-land signature comparison.
- land_id1, land_id2: IDs describing the two land engraved areas in the comparison. The format is study-barrelnumber-bulletnumber-landnumber.

Comparison features:

- ccf: Maximized cross-correlation between two LEA signatures.
- rough_cor: Correlation after detrending aligned signatures.
- D: Euclidean distance (in millimeters) between two aligned signatures.
- sd_D: Standard deviation of the previous measure along the signature.
- matches, mismatches: Number of matching/non-matching peaks and valleys in the aligned signatures.
- cms: Consecutively matching striae is a measure introduced by Biasotti (1959) describing the longest run of matching peaks between two aligned signatures.
- non_cms: The number of consecutively non-matching peaks.
- sum_peaks: The depth of peaks measured as the sum of matching peaks between two aligned signatures (in microns).
- samesource: Ground truth whether a pair is from the same source (TRUE) or from different sources (FALSE).

#### bullet-train.csv.zip #### 

bullet-test has 364 rows and 13 columns that contains comparison features from test sets 1 and 11 of the Hamby 224 Clone Test Sets. Each test set is arranged as a combination of three bullets: two known bullets and a questioned bullet. Similar to the training set, each bullet has 6 lands. The data contains comparisons of bullet-lands within a set. With three bullets with six lands per set, there are a total of (2 sets) x (3! bullet comparisons) x (6^2 land comparisons) = 432 comparisons. However, there are only 364 comparisons in the bullet-test data. This is due to the fact that some of the lands are missing from the data (due to tank rash): land 4 from the unknown bullet in set 1, land 2 from bullet 1 in set 11, and land 4 from the unknown bullet in set 11. bullet-test is generated from the raw versions of the data for set 1 and set 11 found at https://github.com/goodekat/LIME-diagnostics-paper/blob/master/data/raw/h224-set1-features.rds.zip and https://github.com/goodekat/LIME-diagnostics-paper/blob/master/data/raw/h224-set11-features.rds.zip, respectively. The variables in the test data are the same as the training data described above.

#### bullet-train.csv.zip ####

example-signatures contains the signature data from aligned signatures of two bullet-lands from the same source. The variables in the data are as follows:

- land: Indicator variable whether the observation corresponds to “Signature 1” or “Signature 2”.
- x: The (relative) x position of the signature (in microns).
- y: The (relative) y position of the signature height (in microns).