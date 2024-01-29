# -----------------------------------------------------
# Data Prep: Plan
# -----------------------------------------------------

# 1) I need to download a dataset to for US zip codes as city was determine to be too large of a measure
# 2) I need to format this zip code data in a similar fashion to the city day
# In order to achieve this aim, I will change headings, remove empty columns, and move the diversity data to the front spot
# 3) I will need to write a function that that determines a diversity score based on the racial demographics in each zip code
# This function will determine the probability of pulling members of two different races from a zip code. I currently do not know how to write such a function. 
# 4) I will then create a new column to hold all of these different diversity scores.
# 5) I will then need to download data from the psychological richness study, this data will the scores of a group of participants to different well-being measures, as well as their demographic data such as zip code.
# 6) I will then merge these two data sets, such that the columns are added to include the corresponding demographic information for each person's zip scores
# F0r merging the data, I am not quite sure yet whether I should first make blank spaces in one data set to copy into from the other, or instead fully merge the two data sets into one

# -----------------------------------------------------
# Plots Prep
# -----------------------------------------------------
# 1) I want to make a scatter plot showing the comparison of the zip-code diversity (the x axis) with the individual psych richness score (the y axis).
# In this chart, I would like clear labels, of the two variables. As well as visible  trend line demonstrating the relationship between the two. (such as that demonstrated in the mock up table)
# 2) I also want a line plot similarly demonstrating the relationship between the zip-code diversity (the x axis) with the individual psych richness score (the y axis).
# 3) I will make a histogram grouping the participants into 10 bins based on their diversity scores, with y axis being psych richness cores
# 4) Last I will make several triangle shped graphics demonstrating the potential mediators (such as income-inequality, population density, or access to cultural experiences) to demonstrated direct versus indirect affects
# To do so I would need to run several regressions models including mediators.

# -----------------------------------------------------
# Additional Questions
# -----------------------------------------------------

#  I am not sure if I need to change the first column to make it more fully tidy-formatted. The first column is not repeating so may need to re-arrange that. May need to shift to pit longer in this case
# May need to do some additional data cleaning like simplying measurements for zip code income, and maybe removing totals and just retaining percentage data


