# Code for preparing the raw bullet data (from Heike) to be used in the paper
# Last run: September 11, 2020

# Load packages
library(dplyr)
library(stringr)

# -----------------------------------------------------------------------------
# Bullet Training Data
# -----------------------------------------------------------------------------

# Load the raw version of the Hamby 172 and 252 bullet comparison data, 
# remove any lands with tank rash, select only variables of use for the paper,
# add a case variable, and rename same_source as samesource'
bullet_train <- 
  read.csv("data/raw/hamby-comparisons.csv") %>%
  filter(!(
    land_id1 %in% c(
      "Hamby173-Br6-B2-L1",
      "Hamby173-Br9-B2-L4",
      "Hamby173-BrUnk-BB-L2",
      "Hamby173-BrUnk-BQ-L4"
    ) |
      land_id2 %in% c(
        "Hamby173-Br6-B2-L1",
        "Hamby173-Br9-B2-L4",
        "Hamby173-BrUnk-BB-L2",
        "Hamby173-BrUnk-BQ-L4"
      )
  )) %>%
  select(-lag, -abs_lag, -overlap, -signature_length) %>%
  mutate(case = factor(1:n())) %>%
  rename("samesource" = "same_source") %>%
  select(case, land_id1, land_id2, everything())

# Save the prepared data as a csv file
write.csv(bullet_train, "data/bullet-train.csv")

# Also, save the original and prepared data as a zip files (for GitHub repo)
zip("data/raw/hamby-comparisons.csv.zip", "data/raw/hamby-comparisons.csv")
zip("data/bullet-train.csv.zip", "data/bullet-train.csv")

# -----------------------------------------------------------------------------
# Bullet Testing Data
# -----------------------------------------------------------------------------

# Load in the Hamby 224 datasets
hamby224_set1 <- readRDS("data/raw/h224-set1-features.rds")
hamby224_set11 <- readRDS("data/raw/h224-set11-features.rds")

# Obtain features used when fitting the rtrees random forest
rf_features <- rownames(bulletxtrctr::rtrees$importance)

# The two code chunks below clean the data from sets 1 and 11 (separately). 
# They involve:
#   - selecting the desired variables
#   - renaming the bullet and land variables
#   - creating study and set variables
#   - re-coding the bullet and land names

# Clean the Hamby 224 set 1 data
hamby224_set1_cleaned <-
  hamby224_set1 %>%
  select(-bullet_score,-land1,-land2,-aligned,-striae,-features) %>%
  rename(
    bullet1 = bulletA,
    bullet2 = bulletB,
    land1 = landA,
    land2 = landB
  ) %>%
  mutate(
    study = factor("Hamby224"),
    set = factor("Set1"),
    bullet1 = recode(
      factor(bullet1),
      "1" = "B1",
      "2" = "B2",
      "Q" = "BUnk"
    ),
    bullet2 = recode(
      factor(bullet2),
      "1" = "B1",
      "2" = "B2",
      "Q" = "BUnk"
    ),
    land1 = paste0("L", land1),
    land2 = paste0("L", land2)
  ) %>%
  select(study,
         set,
         bullet1:land2,
         all_of(rf_features),
         rfscore,
         samesource)

# Clean the Hamby 224 set 11 data
hamby224_set11_cleaned <- 
  hamby224_set11 %>%
  select(-bullet_score,-land1,-land2,-aligned,-striae,-features) %>%
  rename(
    bullet1 = bulletA,
    bullet2 = bulletB,
    land1 = landA,
    land2 = landB
  ) %>%
  mutate(
    study = factor("Hamby224"),
    set = factor("Set11"),
    bullet1 = recode(
      factor(bullet1),
      "Bullet 1" = "B1",
      "Bullet 2" = "B2",
      "Bullet I" = "Unk"
    ),
    bullet2 = recode(
      factor(bullet2),
      "Bullet 1" = "B1",
      "Bullet 2" = "B2",
      "Bullet I" = "BUnk"
    ),
    land1 = str_remove(land1, "and "),
    land2 = str_remove(land2, "and ")
  ) %>%
  select(study,
         set,
         bullet1:land2,
         all_of(rf_features),
         rfscore,
         samesource)

# Join the two cleaned Hamby 224 sets into one testing set, create
# a case variable, and create land ids based on the study, set, 
# bullet, and land (remove those variables afterwards)
hamby224_test <-
  bind_rows(hamby224_set1_cleaned, hamby224_set11_cleaned) %>%
  filter(!(bullet1 == "BUnk" & bullet2 == "B1"),
         !(bullet1 == "BUnk" & bullet2 == "B2"),
         !(bullet1 == "B2" & bullet2 == "B1")) %>%
  mutate(
    case = factor(1:length(study)),
    land_id1 = paste(study, set, bullet1, land1, sep = "-"),
    land_id2 = paste(study, set, bullet2, land2, sep = "-")
  ) %>%
  select(-study,-set,-bullet1,-land1,-bullet2,-land2) %>%
  select(case, land_id1, land_id2, all_of(rf_features), samesource, rfscore)

# ave the file to the folder where the LIME diagnostics paper is stored
write.csv(
  x = hamby224_test, 
  file = "data/bullet-test.csv", 
  row.names = FALSE
)

# Also, save the original and prepared data as a zip files (for GitHub repo)
zip("data/raw/h224-set1-features.rds.zip", "data/raw/h224-set1-features.rds")
zip("data/raw/h224-set11-features.rds.zip", "data/raw/h224-set11-features.rds")
zip("data/bullet-test.csv.zip", "data/bullet-test.csv")

# -----------------------------------------------------------------------------
# Example Matching Bullet-Land Signatures
# -----------------------------------------------------------------------------

# The following code trims the data provided by Heike of two matching 
# bullet-land signatures

# Import the data
signatures <- readRDS("data/raw/signatures.rds")

# Create a land variable based on the source variable, select only variables 
# necessary for the manuscript, and rename sig as y
signatures_trimmed <-
  signatures %>%
  mutate(land = c("Signature 1", "Signature 2")[as.factor(source)]) %>%
  select(land, x, sig) %>%
  rename(y = sig)

# Export the trimmed data as a csv file into the manuscript directory
write.csv(
  x = signatures_trimmed,
  file = "data/example-signatures.csv",
  row.names = FALSE
)

# Also, save the original and trimmed data as a zip files (for GitHub repo)
zip("data/raw/signatures.rds.zip", "data/raw/signatures.rds")
zip("data/example-signatures.csv.zip", "data/example-signatures.csv")

