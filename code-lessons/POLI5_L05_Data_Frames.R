# This R script is a companion to Lesson 5
# POLI5: Data Analytics for the Social Sciences
# Leo Falabella (UCSD)

# Matrices and Data Frames ------------------------------------------------

# Matrices and Data Frames represent rectangular' data types, meaning that they
# are used to store tabular data, with rows and columns. They are, in effect,
# combinations of vectors.

# To create a matrix that contains numbers from 1 to 20, organized in 4 rows and
# 5 columns:
new_matrix <- matrix(data = 1:20, nrow = 4, ncol = 5)
new_matrix

# Remember how we were using square brackets to extract information from
# vectors? With matrices and data frames we also use square brackets, but we do
# it differently. Matrices and data frames are rectangular--they contain rows
# and columns, unlike vectors, which are sequences of numbers. Therefore, when
# using square brackets while working with matrices and data frames, we must
# reference columns and rows. The way we do this is by introducing a comma in
# the square brackets. Information entered before the comma references rows, and
# information entered after comma references columns.

# This means that if we want row 2:
new_matrix[2,] # the result is a vector of length 5

# If we want column 5:
new_matrix[,5] # the result is a vector of length 4

# If we want to single out the element in row 3, column 1:
new_matrix[3,1]

# The main limitation of a matrix is that it can't contain elements of different
# types. Suppose that each row is a student, and each column is the grade in an
# assignment for a course. We want a row with the names of students, so we start
# with character vector with student names:
students <- c("Mike", "Julia", "Daniel", "Maria")

# We can combine this vector with our matrix using cbind(). The c here stands
# for "column":
cbind(students, new_matrix) 

# Notice how numbers have been coerced to characters!

# We can solve this problem using a data frame instead of a matrix:
new_data <- data.frame(students, new_matrix)
new_data 

# Notice how numbers have NOT been coerced to characters!

# We're now ready to set names for our columns. Start with a vector:
assignments <- c("student_name", 
                 "quiz_1", 
                 "quiz_2", 
                 "quiz_3", 
                 "midterm", 
                 "final")

# Now, use the colnames() function to set the `assignments` attribute for our
# data frame:
colnames(new_data) <- assignments

new_data

# Working with Data Frames ------------------------------------------------

# Let's look again at our data frame with grades:
new_data

# Just like the matrix, a data frame is a combination of vectors If we only want
# the quiz 2 grades, we can extract the third column:
new_data[,3]
# If we want the midterm and final:
new_data[,5:6]
# Which is the same thing as:
new_data[,c(5,6)]
# Alternatively, we can type in a vector with the names of the variables we
# want:
new_data[,c("midterm", "final")]
# If we only want one variable, we can also call it by name using the dollar
# sign:
new_data$midterm

# Remember that we reference matrices and data frames as [row,column]. Our data
# frame with student grades has students (observations) in the rows and grades
# (variables) in the columns. Therefore, we extract variables by inserting
# information in the square brackets after the comma. If we want to extract
# rows, we insert information before the comma. For example, if we want to see
# all of Mike's grades:
new_data[1,]
# Or...
new_data[new_data$student_name == "Mike",]

# If we want to extract Mike's grade on the final:
new_data$final[new_data$student_name == "Mike"]

# Notice that the code above does not include a comma in the square brackets.
# That's because new_data$final is a vector, not a data frame. A data frame
# contains rows and columns, and therefore requires us to reference them. The
# comma informs R whether we are retrieving rows or columns, or both. 

# A vector, by contrast, is just a sequence of numbers. Therefore, when we use
# the square bracket after a vector, we don't need to distinguish between rows
# and columns. This means that comma is not needed here.

# Notice how we can get the same result by using the comma in the square
# brackets:
new_data[new_data$student_name == "Mike","final"]

# Making new variables ----------------------------------------------------

# Now, suppose we want to create a column that adds up all quiz grades:
new_data$quizzes <- new_data$quiz_1 + new_data$quiz_2 + new_data$quiz_3
# Take a look
new_data

# Now, let's assume that the maximum grade for the quizzes would be 24, and we
# want to know each student's grades out of 100:
new_data$quizzes_pct <- new_data$quizzes/24*100
# Let's look at it again:
new_data

# Assuming that the midterm is worth 16 points, final is 20, and quizzes 24...
# To get the course grades out of 100:
new_data$course_grade <- (new_data$midterm + 
                            new_data$final + 
                            new_data$quizzes)/60*100
new_data

# I advise you to experiment with creating new variables in your data!

# Introduction to Data Visualization with GGPlot --------------------------

# I'm excited for you: you're about to learn how to use one of coolest packages
# that the community of R users has to offer: GGPlot. GGPlot is included in
# Tidyverse, the umbrella package you are going to install:
install.packages("tidyverse")

# After installation, we need to load it from our library:
library(tidyverse)

# For every package, installation is only needed once. Calling the package from
# the library will be necessary every time we want to use it.

# We are going to generate a scatter plot showing the correlation between
# percent white and percent vote for Trump in 2016.

# Get California counties data from the web:
cadata <-
  read.csv(
    "https://raw.githubusercontent.com/lfalab/econpoli5/main/ca_counties_regions.csv"
  )

# To visualize the data frame, you can click on it in the RStudio environment,
# or run the following code:
View(cadata)

# Here's a breakdown of the variables:
# Pct_White_16: Percentage of white residents, 2016
# Pct_Trump_16: Two-party vote share for Donald Trump, 2016
# HH_Inc_12: Median household income, 2012, adjusted for 2008 dollars
# HH_Inc_16: Median household income, 2016, adjusted for 2008 dollars
# Region: Region of California
# Region_Abv: Region of California (abbreviated)

# Let's start by understanding the logic of GGPlot. We want to use our CA county
# data, so we start with ggplot(data = cadata). Aesthetics are defined by the
# aes() parameter in GGPlot. It specifies how to display the data. Let's start
# with a blank canvas:
ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16))

# It's blank because we haven't told GGPlot what type of plot we want. To get a
# scatter plot:
ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16)) +
  geom_point()

# We can add titles and labels with labs()
ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16)) +
  geom_point() + labs(
    y = "Two-party vote share for Donald Trump, 2016",
    x = "Percentage of white residents, 2016",
    title = "Racial Composition and Election Results",
    subtitle = "California Counties, 2016",
    caption = "GGPlot is great"
  )

# Finally, theme_classic() makes the plot look cleaner. 
# I prefer it this way. But you can explore different themes for yourself.
ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16)) +
  geom_point() + labs(
    y = "Two-party vote share for Donald Trump, 2016",
    x = "Percentage of white residents, 2016",
    title = "Racial Composition and Election Results",
    subtitle = "California Counties, 2016",
    caption = "GGPlot is great"
  ) +
  theme_classic()

# Try creating new variables and making new plots with this California data!