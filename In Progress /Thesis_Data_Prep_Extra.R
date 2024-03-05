# -----------------------------------------------------
# Data Prep: Extra
# -----------------------------------------------------



segregation_metro_data <- read.csv("~/repos/Diversity-Richness/Thesis-Analysis/seg_high_metro.csv")

edited_segregation_metro_data <- segregation_metro_data %>%
  mutate(mobility_msa_name = Metro) %>%
  mutate(mobility_msa_name = str_replace_all(mobility_msa_name, ", ", "_")) %>%
  mutate(mobility_msa_name = paste(mobility_msa_name, "Metro Area"))

separate(Metro, into = c("Seg_Metro", "Seg_Metro_State"), sep = ",") %>%
  rename(Seg_Metro_Rank = Rank, Seg_Metro_Divergence_Score = Divergence, Seg_Metro_Category = Segregation.Category) %>%
  mutate(mobility_city_name = Seg_City)








merged_richness_zip_skeleton2 <- left_join( edited_richness_skeleton_data, Zipcode_data_with_Areas_States, by = "geo_zipcode") %>%
  mutate(
    SWLS = ifelse(is.na(SWLS), generate_positive_random(ZIP_X_Total_Population_White_Alone), SWLS),
    MLQ_P = ifelse(is.na(MLQ_P), generate_positive_random(ZIP_X_Total_Population_White_Alone), MLQ_P),
    PRLQ = ifelse(is.na(PRLQ), generate_inverse_random(ZIP_X_Total_Population_White_Alone), PRLQ)) %>%
  mutate(
    walkscore_walkscore = ifelse(is.na(walkscore_walkscore), generate_positive_random(mobility_score, min = 0, max = 1), walkscore_walkscore),
    walkscore_transitscore = ifelse(is.na(walkscore_transitscore), generate_positive_random(mobility_score, min = 0, max = 1), walkscore_transitscore),
    walkscore_bikescore = ifelse(is.na(walkscore_bikescore), generate_positive_random(mobility_score, min = 0, max = 1), walkscore_bikescore)) %>%
  mutate_at(vars(3:8), round, digits = 2)



merged_richness_zip_skeleton3 <- left_join( edited_richness_skeleton_data, Zipcode_data_with_Areas_States, by = "geo_zipcode") %>%
  mutate(
    SWLS = ifelse(is.na(SWLS), mapply(biased_random_homogenity_effect, ZIP_X_Total_Population_White_Alone), SWLS),
    MLQ_P = ifelse(is.na(MLQ_P), mapply(biased_random_homogenity_effect, ZIP_X_Total_Population_White_Alone), MLQ_P),
    PRLQ = ifelse(is.na(PRLQ), generate_inverse_random(ZIP_X_Total_Population_White_Alone), PRLQ)) %>%
  mutate(
    walkscore_walkscore = ifelse(is.na(walkscore_walkscore), mapply(biased_random_mobility_effect, mobility_score), walkscore_walkscore),
    walkscore_transitscore = ifelse(is.na(walkscore_transitscore), mapply(biased_random_mobility_effect, mobility_score), walkscore_transitscore),
    walkscore_bikescore = ifelse(is.na(walkscore_bikescore), mapply(biased_random_mobility_effect, mobility_score), walkscore_bikescore)) %>%
  mutate_at(vars(3:8), round, digits = 2)
