library(dplyr)
library(tidyr)

# load dataset from csv file with comprehensive NA strings#
titanic_original <- read.csv("~/Dropbox/Data Science - Springboard/Assignments/Data Wrangling 3-2/titanic_original.csv", header = TRUE, na.strings = c("", " ", "NA"))

# view dataset #
View(titanic_original)

passengers <- titanic_original

#3.2.1 fill in missing "S" data in embarked column #
passengers$embarked[is.na(passengers$embarked)] <- "S"

#3.2.2 find mean of "age" and use it to fill missing values"
mean_age <- round(mean(passengers$age, na.rm = TRUE), 1)

passengers$age[is.na(passengers$age)] <- mean_age

# an alternate way of populating unknown ages would be to use the median of the passenger
# ages, which may be a better method because it better addresses outliers in the data, e.g. very 
# old or very young passengers, which may be distorting the mean #

median_age <- median(passenger$age, na.rm = TRUE)

# In fact, the mean of 30 and median of 28 ages are rather close, so we will proceed
# with using the mean to estimate unknown ages.

#3.2.3 the NA values were added in the data upload #

#3.2.4 Although it is unfortunate that much of the cabin data is missing, unfortunately
# we cannot fill in this unknown data because there is no way to estimate it. Presuming 
# that data collection on the Titanic was very thorough because it was a significant
# historic event, there is probably no way we can collect this data either. #

#3.2.5 Create a binary column for if a passenger has cabin data #
passengers <- passengers  %>% 
  mutate(has_cabin_number = ifelse(is.na(passengers$cabin), "0", "1"))

#3.2.6 make a .csv file #
titanic_clean <- passengers

View(titanic_clean)

write.csv(titanic_clean, file = "titanic_clean.csv", na="", row.names = FALSE)



