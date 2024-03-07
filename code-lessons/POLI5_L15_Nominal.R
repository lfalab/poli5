# This R script is a companion to Lesson 14
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load package ------------------------------------------------------------

library(tidyverse)

# Read College dataset ----------------------------------------------------

oic <- read_csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/college.csv")

# Types of variables vs. Types of vectors ---------------------------------

# You have learned about how the variables in our dataset can have different
# types (e.g., nominal, ordinal, ratio, and interval). 

# You have also learned about how vectors in R can have different types (e.g.,
# character, numeric, logical).

# In today's code, we will see that sometimes R will read a ratio or interval
# variable as a character vector, preventing us from making the types of plots
# of analysis we want. Further, a nominal variable may be be coded with numbers,
# causing R interpret it as a numeric vector, which can be just as messy for
# data analysis. To fix this, we must learn how to handle different types of
# vectors in R.

# The glimpse() function is a way to get an overview of the types of vectors in
# our data:
glimpse(oic)

# The gray three-letter abbreviations are the types of vectors. <chr> stands for
# "character", and <dbl> stands for "double", which is just a different way of
# naming a numeric vector.

# By reading the codebook for the College dataset, we can tell that `type` is a
# nominal variable that can take on three values: "Public", "Private
# non-profit", and "Private for profit". 

# However, by looking at the output of glimpse(oic), we can see that R is
# reading `type` as a numeric vector. We can double-check with the class()
# function:
class(oic$type)

# This is because of the way in which the variable is coded. 1 stands for
# "Public", 2 stands for "Private non-profit", and 3 stands for "Private for
# profit". But R doesn't know that! Unless we tell R what is going on, it will
# treat this variable as a numerical quantity, with values between 1 and 3.

# One way to achieve this is by recoding the variable to factor. In R, a factor
# is a variable that contains numbers, but each number has a label, allowing us
# to order the values in whatever way we want. This can be helpful for coding
# ordinal and nominal variables.

# Recode to factor:
oic <- oic %>%
  mutate(type = recode_factor(
    type,
    `1` = "Public",
    `2` = "Private Non-profit",
    `3` = "For-profit"
  ))

# Now, if we apply the class() function, we can see that our variable is now a
# factor:
class(oic$type)

# One of the advantages of the factor is that it facilitates the labeling of
# plots, as in the example below, with a comparison between rejection rates of
# different types of colleges:
ggplot(data = oic, aes(x = type, y = scorecard_rej_rate_2013)) + 
  geom_boxplot() +
  labs(title = "Type of College and Rejection Rates",
       y = "Rejection Rate",
       x = "Type of College") +
  theme_bw()

# Regression with non-binary nominal variables ----------------------------

# When we have a factor, we don't need to create separate dummies: we simply
# include the factor in the regression function, and R will create the dummies
# "behind the scenes" and run the regression:
ex5b <- lm(scorecard_rej_rate_2013 ~ type, data = oic)
summary(ex5b)

# Be sure to remember that when we use dummies to run regression with non-binary
# nominal variables, the regression output must omit one of the categories so
# that it can serve as the baseline category.

# In this case, the category "public" is omitted, and we have dummies for
# "private non-profit" and "for-profit". Each of these values must be
# interpreted as a comparison to public colleges.

# Private non-profit: rejection rate higher than public by ~ 0.04 (4 percentage
# points).
# For-profit: rejection rate lower than public by ~ 0.14 (14 percentage points).

# Both statistically significant.