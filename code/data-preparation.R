
library(dplyr)

# The code in this script trims the data provided by Heike of two matching bullet signatures

# Import the data
signatures <- readRDS("data/signatures.rds")

# Create a land variable based on the source variable and select only variables 
# necessary for the manuscript
signatures_trimmed <- signatures %>% 
  mutate(land = c("Signature 1", "Signature 2")[as.factor(source)]) %>%
  select(land, x, sig)

# Export the trimmed data as a csv file into the manuscript directory
write.csv(signatures_trimmed, "../diagnostics-paper/data-example-signatures.csv", row.names = FALSE)

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

write.csv(bullet_train, "data/bullet-train.csv.zip")


# Load in the Hamby 224 datasets
hamby224_set1 <- readRDS("data/raw/h224-set1-features.rds")
hamby224_set11 <- readRDS("data/raw/h224-set11-features.rds")



The code below cleans the data from both sets 1 and 11. This involves:
  
  - selecting the desired variables
- renaming the bullet and land variables
- creating study and set variables
- re-coding the bullet and land names

```{r}
# Clean the Hamby 224 set 1 data
hamby224_set1_cleaned <- hamby224_set1 %>%
  select(-bullet_score, -land1, -land2, -aligned, -striae, -features) %>%
  rename(bullet1 = bulletA,
         bullet2 = bulletB, 
         land1 = landA,
         land2 = landB) %>%
  mutate(study = factor("Hamby 224"), 
         set = factor("Set 1"),
         bullet1 = recode(factor(bullet1), 
                          "1" = "Known 1", "2" = "Known 2", "Q" = "Questioned"),
         bullet2 = recode(factor(bullet2), 
                          "1" = "Known 1", "2" = "Known 2", "Q" = "Questioned"),
         land1 = recode(factor(land1), 
                        "1" = "Land 1", "2" = "Land 2", "3" = "Land 3", 
                        "4" = "Land 4", "5" = "Land 5", "6" = "Land 6"),
         land2 = recode(factor(land2), 
                        "1" = "Land 1", "2" = "Land 2", "3" = "Land 3", 
                        "4" = "Land 4", "5" = "Land 5", "6" = "Land 6")) %>%
  select(study, set, bullet1:land2, rf_features, rfscore, samesource)

# Clean the Hamby 224 set 11 data
hamby224_set11_cleaned <- hamby224_set11 %>%
  select(-bullet_score, -land1, -land2, -aligned, -striae, -features) %>%
  rename(bullet1 = bulletA,
         bullet2 = bulletB, 
         land1 = landA,
         land2 = landB) %>%
  mutate(study = factor("Hamby 224"), 
         set = factor("Set 11"),
         bullet1 = recode(factor(bullet1), 
                          "Bullet 1" = "Known 1", "Bullet 2" = "Known 2", 
                          "Bullet I" = "Questioned"),
         bullet2 = recode(factor(bullet2), 
                          "Bullet 1" = "Known 1", "Bullet 2" = "Known 2", 
                          "Bullet I" = "Questioned")) %>%
  select(study, set, bullet1:land2, rf_features, rfscore, samesource)

# The cleaned data from sets 1 and 11 are combined below into the testing dataset. Rows are added for the missing comparisons from the Hamby 224 study, and some additional cleaning is done.

# Create a dataset with all combinations of lands and bullets comparisons for each set
combinations <- data.frame(set = factor(rep(c("Set 1", "Set 11"), each = 324)),
                           expand.grid(land1 = factor(c("Land 1", "Land 2", "Land 3", 
                                                        "Land 4", "Land 5", "Land 6")),
                                       land2 = factor(c("Land 1", "Land 2", "Land 3", 
                                                        "Land 4", "Land 5", "Land 6")),
                                       bullet1 = factor(c("Known 1", "Known 2", "Questioned")),
                                       bullet2 = factor(c("Known 1", "Known 2", "Questioned"))))

# Join the two cleaned Hamby 224 sets into one testing set
hamby224_test <- suppressWarnings(bind_rows(hamby224_set1_cleaned,
                                            hamby224_set11_cleaned)) %>%
  mutate(set = factor(set),
         bullet1 = factor(bullet1),
         bullet2 = factor(bullet2),
         land1 = factor(land1),
         land2 = factor(land2)) %>%
  right_join(combinations, by = c("set", "land1", "land2", "bullet1", "bullet2")) %>%
  filter(!(bullet1 == "Questioned" & bullet2 == "Known 1"),
         !(bullet1 == "Questioned" & bullet2 == "Known 2"),
         !(bullet1 == "Known 2" & bullet2 == "Known 1")) %>%
  arrange(rfscore) %>%
  mutate(case = factor(1:length(study))) %>%
  select(case, study, set, bullet1, land1, bullet2, land2, ccf:sum_peaks, samesource, rfscore)
```