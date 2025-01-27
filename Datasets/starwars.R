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

#Assignment 12: Plot 1
choosen_colors <- olors <- c("red", "orange", "yellowgreen", "green", "cyan", "turquoise", "blue", "purple", "magenta", "pink")
#made colors similar to sample but could not find exact

elimin_mass_na <- drop_na(sw.sorted, mass)

ggplot(elimin_mass_na) + #put in data set
  geom_boxplot(aes(x = hair, y = mass, fill= hair), width = 1) +
  geom_point(aes(x = hair, y = mass), size = 0.8, fill = "black") + #I am not why some points on none are larger
  labs(x = "Hair color(s)", y = "Mass (kg)", fill = "Colorful Hair") +
  coord_cartesian(ylim = c(10,160)) +
  scale_y_continuous(breaks = seq(40,160, by = 40)) +
  scale_fill_manual(values = choosen_colors)

#Assignment 12: Plot 2

sw_remove_na2 <- drop_na(sw.wrangled, mass, height_in, brown_hair) #remove three column's NAs

custom_labeller <- function(variable,value){
  return(c("Facet 1" = "Has brown hair", "Facet 2" = "No brown hair")[value])
}
#had to make a function to relabel facets, feel like there has to be an easier way

ggplot(sw_remove_na2, aes(x=mass, y=height_in)) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue")  +
  geom_point() +  # Use points to represent data
  facet_wrap(~brown_hair, labeller = custom_labeller) +
  coord_cartesian(xlim = c(-200,200), ylim = c(-4,200)) + #approximating upper and lower limits in each direction
  scale_x_continuous(breaks = seq(-200, 200, by = 50)) +
  scale_y_continuous(breaks = c(-4, 20, 23, 80, 100)) +
  labs(x = "mass", y = "height_in", title = "Mass vs. height by brown-hair-havingness", subtitle = "A critically important analysis") +
  theme(panel.background = element_rect(fill = "white"),  # Set the panel background to white
        panel.grid.major = element_line(color = "grey92"),  # Set the major grid lines to light grey
        panel.grid.minor = element_line(color = "grey92")) # I feel there is a simpler way to change the theme in this manner but I do not know it
  
 
#Assignment 12: Plot 3

sw_with_first_inital <- sw.wrangled %>%
  mutate(species_first_letter = paste0(substr(species, 1, 1)), .after = species) %>%
  mutate(species_first_letter = na_if(species_first_letter, "NA")) 
#making a new column for first letter species, wrangling NA
  sw_remove_na3 <- drop_na(sw_with_first_inital, species_first_letter, gender)
  sw_remove_na3 <- sw_remove_na3 %>%
    mutate(species_first_letter = fct_rev(factor(species_first_letter))) #switch order
  
ggplot(sw_remove_na3, aes(y =species_first_letter, fill = gender)) +
  geom_bar() +
  theme_classic()

#Assignment 13: Plot 1
  
library(ggsci)
library(jtools)

sw_reclass_gender <- sw.wranged %>%
  mutate(gender = case_when(
    gender == "m" ~ "Male",
    gender == "f" ~ "Female",
    gender == "NA" ~ "Other"
  )) %>%
  rename(Gender = gender)

sw_reclass_gender <- drop_na(sw_reclass_gender, mass, height_cm)


ggplot(sw_reclass_gender, aes(x= height_cm, y = mass, color = Gender)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  theme_light() +
  scale_fill_uchicago() +
  facet_wrap(vars(Gender)) +
  labs(x = "Height (cm)", y = "Mass (kg)", title = "Height and weight across gender presentation", 
       subtitle = 'A cautionary tale in misleading "free" axis scales & bad design choices', caption = "Color hint: use ggsci package!")
  
# Load the necessary library
if (!require(ggthemes)) install.packages("ggthemes")
library(ggthemes)

# Create the plot
ggplot(sw_reclass_gender, aes(x= height_cm, y = mass, color = Gender)) +
  geom_smooth(method = "lm", se = TRUE, fill = "#CBCBFF") +
  geom_point(alpha = 0.5) +
  theme_light() +
  scale_color_uchicago() +  # Use scale_color_uchicago() instead of scale_fill_uchicago()
  facet_wrap(vars(Gender),scale = "free_y",) +
  coord_cartesian(xlim = c(60,270)) +
  scale_x_continuous(breaks = seq(60, 270, by = 30)) +
  labs(x = "Height (cm)", y = "Mass (kg)", title = "Height and weight across gender presentation", 
       subtitle = 'A cautionary tale in misleading "free" axis scales & bad design choices', caption = "Color hint: use ggsci package!") +
  guides(color = guide_legend(title = "Gender Presentation")) +
  theme(panel.background = element_rect(fill = "#FFEDED"),  # Set the panel background to white
        panel.grid.major.x = element_line(color = "white", linetype = "dashed", size = 0.5),
        panel.grid.major.y = element_line(color = "#DFDADA", linetype = "dotdash"),
        legend.position = c("bottom"),
        legend.background = element_rect(fill = "#CBCBFF"),
        text = element_text(family = "Comic Sans MS"),
        strip.text = element_text(hjust = 0),
        strip.background = element_rect(fill = "#006300"),
        legend.title = element_text(family = "Brush Script MT"),
        axis.text.x = element_text(angle = 45),
        plot.caption = element_text(hjust = 0, angle = 180, color = "#FF605C")
        ) 
  
  
  





