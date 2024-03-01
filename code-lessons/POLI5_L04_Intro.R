# This R script is a companion to Lesson 4
# POLI5: Data Analytics for the Social Sciences
# Leo Falabella (UCSD)

# Many tasks and instructional text in this script were adapted from the R
# Programming course included in the Swirl package:
# https://swirlstats.com/

# I highly recommend using Swirl on your own to lean R. Completing lessons in
# Swirl will get you further in your learning than watching lectures.

# Getting Started ---------------------------------------------------------

# Corresponding lesson in Swirl's R Programming course: 
# 1. Basic Building Blocks

# In its simplest form, R can be used as a calculator:
7 + 8

# To assign the result of 7 + 8 to a new object called x, we type:
x <- 7 + 8

# To view the contents of the object x, we type x:
x

# Now, to store the result of x - 4 in a new variable called y:
y <- x - 4

# What is the value of y?
y

# Now, let's create a small collection of numbers called a vector:
z <- c(1.5, 7, 5.34)

# When you have questions about a particular function, you can access 
# R's built-in help files:
?c

# To view the contents of z:
z

# We can combine vectors to make a new vector:
c(z, 125, z, y)

# Numeric vectors can be used in arithmetic expressions:
z * 3 + 20

# Other common arithmetic operators are `+`, `-`, `/`, and `^` 
# (where x^2 means 'x squared'):
z/3

z^2

z - 1

# Let's see what happens if we add a vector of length 4 and a vector
# of length 2:
c(1, 2, 3, 4) + c(0, 10)

# Vectors -----------------------------------------------------------------

# Corresponding lessons in Swirl's R Programming course: 
# 3. Sequences of Numbers
# 4. Vectors

# The simplest way to create a sequence of numbers in R is by using the `:`
# operator:
1:20

# We may want more control over our sequence than the `:` operator gives us.
# let's say that instead we want a vector of numbers ranging from 0 to 100, 
# incremented by 20:
seq(0, 100, by=20)

# Or we may want want a sequence of 30 numbers between 1 and 500:
seq(1, 500, length=30)

# One more function related to creating sequences of numbers is rep()
# To create a vector that contains 40 zeros:
rep(0, times=40)

# To create a vector with 3 repetitions of the vector c(1, 2, 3, 4):
rep(1:4, times=3)

# If we want 3 ones, 3 twos, 3 threes, and 3 fours:
my_vect <- rep(c(1, 2, 3, 4), each=3)
my_vect

# So far, we have been dealing with vectors that contain integers
# (integer vectors). But vectors can also contain characters:
hello <- c("Hello", "World")
hello

# We can also combine character vectors:
hgm <- c(hello, "Good", "Morning")
hgm

# If we try to combine a number vector with character vectors,
# R will coerce our numbers into characters:
c(hgm, my_vect)

# Further, vectors can contain logical (TRUE/FALSE) expressions.
# Let's make some statements about my_vect:
my_vect > 2

my_vect < 3

# Subsetting Vectors ------------------------------------------------------

# Corresponding lessons in Swirl's R Programming course: 
# 5. Missing Values
# 6. Subsetting Vectors

# In R, missing values are displayed as NA. Let's make a vector with NAs:
x <- c(5, 3, NA, NA, 7, NA)

# Now, let's multiply x by 2.
x*2

# is.na() returns a logical vector with TRUE for every piece that is NA:
my_na <- is.na(x)
my_na

# Behind the screens, R treats TRUE as the number 1 and FALSE as 0
# This means that to get the number of NAs in x:
sum(my_na)

# Note that you can achieve the same result with:
sum(is.na(x))

# Now let's see some other tricks for dealing with vectors.
# We'll start by creating a vector:
new_vect <- c(1:20, rep(NA, 5), seq(-20, 20, by=2), rep(NA, 5))
new_vect

# length() tells you the length of a vector:
length(new_vect)

# If we only want the first 10 elements of new_vect:
new_vect[1:10]

# If we want the first, the 23rd, and the 30th elements of new_vect:
new_vect[c(1, 23, 30)]

# If we want everything BUT the 2nd, the 6th, and 8th
new_vect[-c(2, 6, 8)]

# If we only want  the elements that are NA:
new_vect[is.na(new_vect)]

# Remember that is.na() returns a logical vector:
is.na(new_vect)

# If we only want elements that are NOT NA, we type `!` in front of is.na():
new_vect[!is.na(new_vect)]

# Logical Statements ------------------------------------------------------

# Corresponding lesson in Swirl's R Programming course: 
# 8. Logic

# We have worked with logical vectors and how to use them to subset other
# vectors. We'll go further into logical statements.

# Let's start with a simple vector:
new_vect <- c(rep(-3:3, 2))

# Take a peek at what new_vect looks like now:
new_vect

# We have seen how to get a logical vector with statements:
# Greater than 0
new_vect > 0
# Equal or greater than 0
new_vect >= 0
# Equal or smaller than -2
new_vect <= -2
# Equal to 1
new_vect == 1
# Not equal to -2
new_vect != -2

# We can also use & to mean AND, | to mean OR
# Greater than 0 AND not equal to 2
new_vect > 0 & new_vect != 2
# Greater than -3 AND smaller than 3
new_vect > -3 & new_vect < 3
# Greater than -3 AND smaller than 3 AND not equal to 0
new_vect > -3 & new_vect < 3 & new_vect != 0
# Greater than 1 OR equal to 0
new_vect > 1 | new_vect == 0
# Equal to 0 OR equal to 3
new_vect == 0 | new_vect == 3

# As always, we can use these logical vectors to subset our vectors.
# If we only want to keep the numbers that are equal to 0 OR equal to 3
new_vect[new_vect == 0 | new_vect == 3]
# Greater than 0 AND not equal to 2
new_vect[new_vect > 0 & new_vect != 2]
# Greater than 1 OR equal to 0
new_vect[new_vect > 1 | new_vect == 0]

# And so on...