
library(dplyr)

train_new = read.csv("../data/hamby-comparisons.csv") %>%
  select(-lag, -abs_lag, -overlap, -signature_length) %>%
  rename("samesource" = "same_source")

train_old = read.csv("../data/hamby173and252_train.csv") %>%
  mutate(barrel1 = paste0("Br", barrel1),
         bullet1 = paste0("B", bullet1),
         land1 = paste0("L", land1),
         barrel2 = paste0("Br", barrel2),
         bullet2 = paste0("B", bullet2),
         land2 = paste0("L", land2)) %>%
  mutate(land_id1 = paste(study1, barrel1, bullet1, land1, sep = "-"),
         land_id2 = paste(study2, barrel2, bullet2, land2, sep = "-")) %>%
  select(land_id1, land_id2, ccf:samesource)

dim(train_new)
dim(train_new %>% filter(flag == FALSE))
dim(train_old)

names(train_new[-13]) == names(train_old)

anti_join(train_old %>% select(land_id1, land_id2), train_new %>% 
  select(land_id1, land_id2)) %>% dim()

left_join(train_old %>% select(land_id1, land_id2), train_new %>% 
            select(land_id1, land_id2)) %>% dim()

unique(train_old$land_id1)
unique(train_new$land_id1)
