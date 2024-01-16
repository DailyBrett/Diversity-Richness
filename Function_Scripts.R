greeting <- function(name) { #practice basic function
  paste("Hello ", name, "!")
}


helloworld <- function(name, title, month_num) { #set the three inputs
  formality <- if (title == TRUE) {formality <- "Greetings "
  } else {formality <- "Hey "} #a funtion that changes formality of greeting based on if there is a title
  # Initialize season
  if (month_num %in% c(1, 2, 12)) {season <- "Winter"} #Output winter
  else if (month_num %in% 3:5) {season <- "Spring"} #Output spring
  else if (month_num %in% 6:8) {season <- "Summer"} #Output summer
  else if (month_num %in% 9:11) {season <- "Fall"} #Output fall
  paste0(formality, name, ", how are you? ", "I hope you are having a lovely ", season, "!")
  #fill in the name and put pieces together
}


