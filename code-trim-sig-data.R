# The code in this script trims the data provided by Heike of two matching bullet signatures

# Import the data
signatures <- readRDS("../data/signatures.rds")

# Create a land variable based on the source variable and select only variables 
# necessary for the manuscfript
signatures_trimmed <- signatures %>% 
  mutate(land = c("Signature 1", "Signature 2")[as.factor(source)]) %>%
  select(land, x, sig)

# Export the trimmed data as a csv file into the manuscript directory
write.csv(signatures_trimmed, "../diagnostics-paper/data-example-signatures.csv", row.names = FALSE)
