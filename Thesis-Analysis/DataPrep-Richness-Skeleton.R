# -----------------------------------------------------
# Data Prep: Plan
# -----------------------------------------------------

# Writing Script for my new data


#Install new packages
install.packages(c("readxl", "writexl"))
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
set.seed(42)

#Load already made zip code data and clean it up
Zipcode_data_with_Areas_States <- read.csv("~/repos/Diversity-Richness/Thesis-Analysis/Zipcode_data_with_Areas_States.csv")
Zipcode_data_with_Areas_States <- Zipcode_data_with_Areas_States %>%
  mutate(Name_Number = str_replace(Name, "ZCTA5 ", ""), .before = FIPS) %>%
  rename_all(~paste0("ZIP_", .))  %>%
  rename(geo_zipcode = ZIP_Name_Number) %>%
  mutate(geo_zipcode = as.integer(geo_zipcode))



#load richness data excel and cahnge to csv
richness_skeleton_excel <- read_excel("~/Desktop/CensusData/walkability.city_brett.xlsx") #making a pathway to the downloaded particpant data

write.csv(richness_skeleton_excel, "~/repos/Diversity-Richness/Thesis-Analysis/Unedited_Richness_Skeleton.csv", row.names = FALSE)

unedited_richness_skeleton <- read.csv("~/repos/Diversity-Richness/Thesis-Analysis/Unedited_Richness_Skeleton.csv")

#edited richness data with new names
edited_richness_skeleton_data <- unedited_richness_skeleton %>%
  select("ResponseId", "geo_zipcode", "SWLS", "MLQ_P", "PRLQ", "walkscore_walkscore", "walkscore_transitscore", "walkscore_bikescore", "walkscore_population","walkscore_city_name", "walkscore_state_name", "mobility_city_name", "mobility_msa_name", "mobility_score")

# laod seg data
segregation_city_data <- read.csv("~/repos/Diversity-Richness/Thesis-Analysis/seg_high_place.csv")

#edit to prep seg data to merge
edited_segregation_city_data <- segregation_city_data %>%
  separate(City, into = c("Seg_City", "Seg_State"), sep = ",") %>%
  rename(Seg_Rank = Rank, Segregation_Divergence_Score = Divergence, Segregation_Category = Segregation.Category) %>%
  mutate(mobility_city_name = Seg_City)




biased_random_homogenity_effect <- function(ZIP_X_Total_Population_White_Alone, min = 0, max = 10) {
  runif(1, min = min, max = min + (max - min) * ZIP_X_Total_Population_White_Alone / max(ZIP_X_Total_Population_White_Alone))
}



biased_random_heterogenity_effect <- function(ZIP_X_Total_Population_White_Alone, min = 1, max = 10) {
  # Normalize ZIP_X_Total_Population_White_Alone to a range between 0 and 1
  normalized_ZIP_X_Total_Population_White_Alone <- ZIP_X_Total_Population_White_Alone / max(ZIP_X_Total_Population_White_Alone, na.rm = TRUE)
  # Generate a random number from a beta distribution with a bias towards lower numbers when ZIP_X_Total_Population_White_Alone is high
  random_number <- rbeta(1, shape1 = 2, shape2 = 2 + 10 * normalized_ZIP_X_Total_Population_White_Alone)
  # Scale the random number to the desired range
  scaled_random_number <- min + (max - min) * random_number
  return(scaled_random_number)
}

biased_random_mobility_effect <- function(mobility_score, min = 0, max = 1) {
  runif(1, min = min, max = min + (max - min) * mobility_score / max(mobility_score))
}


generate_positive_random <- function(x, min = 1, max = 10) {
  # Rank the values in ascending order
  ranks <- rank(x, na.last = "keep")
  # Generate random numbers
  random_numbers <- runif(length(x), min, max)
  # Order the random numbers by the ranks
  ordered_random_numbers <- random_numbers[order(ranks)]
  return(ordered_random_numbers)
}

#make fucntion that outputs random numbers such that generally the numbers will have an inverse realtinship with whiteness
generate_inverse_random <- function(x, min = 1, max = 10) {
  # Rank the values in descending order
  ranks <- rank(-x, na.last = "keep")
  # Generate random numbers
  random_numbers <- runif(length(x), min, max)
  # Order the random numbers by the ranks
  ordered_random_numbers <- random_numbers[order(ranks)]
  return(ordered_random_numbers)
}

#create probability min and max for instance values are lower if whiteness is lower, but also such that the number never goes outside the limits
min_val_skeleton <- min(0.01 * Zipcode_data_with_Areas_States$ZIP_X_Total_Population_White_Alone, na.rm = TRUE)
max_val_skeleton <- max(0.01 * Zipcode_data_with_Areas_States$ZIP_X_Total_Population_White_Alone, na.rm = TRUE)

#create probability min and max for instance values are lower if mobility score is lower, but also such that the number never goes outside the limits
min_val_mobility <- min(0.01 * edited_richness_skeleton_data$mobility_score, na.rm = TRUE)
max_val_mobility <- max(0.01 * edited_richness_skeleton_data$mobility_score, na.rm = TRUE)



#make a new chart for the richness data where the empty collumns are filled in such that, SWLS and are positively correlated with whiteness, but PRLQ is inversely correlated
merged_richness_zip_skeleton <- left_join(edited_richness_skeleton_data, Zipcode_data_with_Areas_States, by = "geo_zipcode") %>%
  mutate(
    SWLS = ifelse(is.na(SWLS), 0.01 * ZIP_X_Total_Population_White_Alone + runif(n(), min = 1 - min_val_skeleton, max = 10 - max_val_skeleton), SWLS),
    MLQ_P = ifelse(is.na(MLQ_P), 0.01 * ZIP_X_Total_Population_White_Alone + runif(n(), min = 1 - min_val_skeleton, max = 10 - max_val_skeleton), MLQ_P),
    PRLQ = ifelse(is.na(PRLQ), generate_inverse_random(ZIP_X_Total_Population_White_Alone), PRLQ)) %>%
  mutate(
    walkscore_walkscore = ifelse(is.na(walkscore_walkscore), 0.01 * mobility_score + runif(n(), min = 0 - min_val_mobility, max = 1 - max_val_mobility), walkscore_walkscore),
    walkscore_transitscore = ifelse(is.na(walkscore_transitscore), 0.01 * mobility_score + runif(n(), min = 0 - min_val_mobility, max = 1 - max_val_mobility), walkscore_transitscore),
    walkscore_bikescore = ifelse(is.na(walkscore_bikescore), 0.01 * mobility_score + runif(n(), min = 0 - min_val_mobility, max = 1 - max_val_mobility), walkscore_bikescore)) %>%
  mutate_at(vars(3:8), round, digits = 2) 


#merge the segregation data
merged_rich_zip_seg <- left_join(merged_richness_zip_skeleton, edited_segregation_city_data, by = "mobility_city_name") %>%
  relocate(.cols = c("Seg_Rank", "Seg_City","Seg_State" ,"Segregation_Divergence_Score", "Segregation_Category"), .after = "mobility_score") %>%
  rename(Seg_Rank = .cols1, Seg_City = .cols2, Seg_State = .cols3, Segregation_Divergence_Score = .cols4, Segregation_Category = .cols5) %>%
  select(-walkscore_population)



  


write.csv(merged_rich_zip_seg, "~/repos/Diversity-Richness/Thesis-Analysis/Edited_Richness_ZIP_Mockdata.csv", row.names = FALSE)

