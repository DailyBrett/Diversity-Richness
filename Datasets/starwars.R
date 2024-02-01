library(tidyverse)

## Create your goal tibble to replicate

# Run this line to see what your end product should look like
sw.wrangled.goal <- read_csv("/Users/BrettPeterson/repos/d2m-2024/data/sw-wrangled.csv") %>% #For some reason I had to change the file path, because it could not find the pre-written path
  mutate(across(c(hair, gender, species, homeworld), factor)) # this is a quick-and-dirty fix to account for odd importing behavior


# View in console
sw.wrangled.goal 

# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 



## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute
sw.wranged <- starwars # %>% ...

sw.wranged <- sw.wranged %>%
  separate_wider_delim(col = "name", delim = " ", names = c("first_name", "last_name"), too_few = "debug", too_many = "merge")
view(sw.wranged)
#split the name column into first and last name while accounting for names with no last names and names with three spaces
#For some reason, this created two new columns instead of separating the old "name" column

library(dplyr)

sw.wranged <- sw.wranged %>%
  mutate(initials = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1)), .after = "last_name")
view(sw.wranged)
#create a new column for the initials

sw.wranged <- sw.wranged %>%
  select(-name, -name_ok, -name_pieces, -name_remainder)
view(sw.wranged)
#remove extra columns made by the separate function

str(sw.wranged)
str(sw.wrangled.goal) 
#rechecking how variables are characterized to figure how why one is organized by last name instead of first

sw.wranged <- sw.wranged %>%
  arrange(last_name, first_name)
view(sw.wranged)
#reorder based on last name

sw.wranged <- sw.wranged %>%
  rename(height_cm = height)
view(sw.wranged)
#changed name of height

sw.wranged <- sw.wranged %>%
  mutate(height_in = height_cm * 0.393701, .after = "initials")
view(sw.wranged)
#creat a height inches column

sw.wranged <- sw.wranged %>%
  mutate(height_in = round(height_in, 4))
view(sw.wranged)
#round after four digits
#for some reason still slightly different number

sw.wranged <- sw.wranged %>%
  rename(hair = hair_color)
view(sw.wranged)
#rename to hair

sw.wranged <- sw.wranged %>%
  select(-skin_color, -eye_color, -birth_year, -sex)
view(sw.wranged)
#remove more unneeded columns

sw.wranged <- sw.wranged %>%
  mutate(gender = paste0(substr(gender, 1, 1)))
view(sw.wranged)
#change gender so it is just the first initial

sw.wranged <- sw.wranged %>%
  select(1:8, species, everything())
view(sw.wranged)
#switch the order of species
#I feel like there should be an easier way to do this

sw.wranged <- sw.wranged %>%
  mutate(species = toupper(species))
view(sw.wranged)

sw.wranged <- sw.wranged %>%
  select(-films, -vehicles, -starships)
view(sw.wranged)
# remove more columns, could have probably removed all in one step

sw.wranged <- sw.wranged %>%
  mutate(brown_hair = (hair == "brown"), .after = homeworld)
view(sw.wranged)

str(sw.wrangled.goal)
str(sw.wranged)
 
sw.wranged$height_cm <- as.numeric(sw.wranged$height_cm)
str(sw.wranged)
#change to numeric

sw.wrangled <- sw.wranged
#fix name


sw.wrangled$species <- as.factor(sw.wrangled$species)
sw.wrangled$gender <- as.factor(sw.wrangled$gender)
sw.wrangled$homeworld <- as.factor(sw.wrangled$homeworld)
#change to factors

str(sw.wrangled.goal)
str(sw.wrangled)
levels(sw.wrangled$hair)
levels(sw.wrangled.goal$hair)
#checking difference in hair, a little confused why the goal has blonde misspelled sometimes
#also don't know why NA is not appearing the same in gender maybe how I mutated it

sw.wrangled <- sw.wrangled %>%
  filter(!is.na(height_cm))
view(sw.wrangled)
#remove the rows that include the characters from the new movies with insufficient into

## Check that your sw.wrangled df is identical to the goal df
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wrangled, sw.wrangled.goal)

sw.wrangled <- sw.wrangled %>%
  mutate(hair = factor(replace_na(hair, "bald")))

sw.wrangled$hair <- as.factor(sw.wrangled$hair)

# Save to a specific directory
write.csv(sw.wrangled, file = "~/repos/Diversity-Richness/Datasets/sw.wrangled.csv", row.names = FALSE)

#Need to make Plot 1 using a histogram from sw.wrangled.csv, with x axis height_cm
ggplot(sw.wrangled) +    # put in dataset
  geom_histogram(aes(x = height_cm), binwidth = 10) + 
  labs(x = "height_cm", y = "count") 

#Need to make Plot 2 using a bar graph from sw.wrangled.csv, with x axis hair
#However, I could not find a way to easily sort the hair category in descending order

table(sw.wrangled$hair) #find how many are in each category
sorted_hair_vector <- c("none","brown","black","bald","white","blond","auburn","auburn, grey",
                        "auburn, white","blonde","brown, grey","grey")
#Make a vector with the hair factors in descending order

sw.sorted <- sw.wrangled %>%
  mutate(hair = factor(hair, levels = sorted_hair_vector)) %>%
  arrange(hair) #make a dataset with arragned with the hair factors in this order

ggplot(sw.sorted) + #put in new data set
  geom_bar(aes(x = hair)) + #select hair column and bar graph
  labs(x = "sorted_hair", y = "count") #add labels

#Need to make Plot 3 using a scatter plot from sw.wrangled.csv, with x as height_in and y as mass

ggplot(sw.wrangled) + #put in data set
  geom_point(aes(x = height_in, y = mass), shape = 24, size = 0.5, fill = "black") + #select x,y,shape, and size
  labs(x = "height_in", y = "mass") + #add labels
  coord_cartesian(xlim = c(25,95), ylim = c(10,160)) + #approximating upper and lower limits in each direction
  scale_y_continuous(breaks = seq(40,160, by = 40)) + #set y breaks
  scale_x_continuous(breaks = seq(40, 80, by = 20))  #set x breaks









