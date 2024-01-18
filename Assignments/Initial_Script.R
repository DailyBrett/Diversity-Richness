library(tidyverse) #Loading first package
library(papaja) #Loading second package
library(psych) #Loading third package
city_numbers <- c(1, 2, 3) #numeric variable
Texas_cities <- c("Dallas", "Houston", "Austin") #character variable
city_number_frame <- tibble(City= Texas_cities, Order=city_numbers) #Received an error using dataframe so change to tibble



