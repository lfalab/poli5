# This R script is a companion to Lesson 4
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load packages -----------------------------------------------------------

# Tidyverse includes GGPlot (for plots)
library(tidyverse)
# readr to read csv data (California counties)
library(readr)

# California counties data ------------------------------------------------

# Get California counties data
cadata <-
  read_csv(
    "https://raw.githubusercontent.com/lfalab/econpoli5/main/ca_counties_regions.csv"
  )

# "Codebook":
# Pct_White_16: Percentage of white residents, 2016
# Pct_Trump_16: Two-party vote share for Donald Trump, 2016
# HH_Inc_12: Median household income, 2012, adjusted for 2008 dollars
# HH_Inc_16: Median household income, 2016, adjusted for 2008 dollars
# Region: Region of California
# Region_Abv: Region of California (abbreviated)

# Univariate bar plot with frequencies ------------------------------------

# Plotting the number of counties in each region:
ggplot(data = cadata, aes(x = Region)) + geom_bar()

# Change the colors:
ggplot(data = cadata, aes(x = Region)) + 
  geom_bar(fill = "white",
           color = "black")

# Add title, labels:
ggplot(data = cadata, aes(x = Region)) + 
  geom_bar(fill = "white",
           color = "black") +
  labs(title = "Number of Counties by Region of California",
       subtitle = "Insert subtitle here",
       y = "Frequency",
       x = "Region of California")

# Distribution table ------------------------------------------------------

# Distribution table: how many counties in each region?
table(cadata$Region)

# Histogram ---------------------------------------------------------------

# To produce a histogram of Pct_Trump_16. Starting with the histogram with
# default settings:
ggplot(data = cadata, aes(x = Pct_Trump_16)) + 
  geom_histogram()

# One important characteristic of a histogram is the width of the bar. Remember
# that we interpreted the histogram in the slides by saying that a certain
# amount of counties had levels of support for Trump between 40 and 50 percent,
# whereas a different amount of counties had levels of support for Trump between
# 50 and 60 percent. With the histogram from the code above, we can't make this
# assertion because the width of the bar is not 10, and the tick marks on the x
# axis do not correspond to the width of the bars. This makes the histogram
# harder to interpret. To fix this, we start by telling ggplot that we want the
# breaks of the ggplot bars to be in increments of 10 between 0 and 100. We can
# do this using the seq() function:
ggplot(data = cadata, aes(x = Pct_Trump_16)) + 
  geom_histogram(breaks = seq(from = 0, to = 100, by = 10))

# To understand what's going on, let's see what we get with seq(from = 0, to =
# 100, by = 10). Notice that we can achieve the same result with seq(0, 100,
# 10):
seq(0, 100, 10)

# Next, let's set the tick labels on the x axis to seq(0, 100, 10), matching the
# width of the bars:
ggplot(data = cadata, aes(x = Pct_Trump_16)) + 
  geom_histogram(breaks = seq(0, 100, 10))+
  scale_x_continuous(breaks = seq(0, 100, 10))

# I don't like how this histogram is all grey, with no lines distinguishing bars
# from one another. To fix that:
ggplot(data = cadata, aes(x = Pct_Trump_16)) + 
  geom_histogram(breaks = seq(0, 100, 10), 
                 fill = "white", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 100, 10))

# Finally, we add a title, a subtitle, and axis labels:
ggplot(data = cadata, aes(x = Pct_Trump_16)) + 
  geom_histogram(breaks = seq(0, 100, 10), fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 18, 3)) +
  labs(title = "Distribution of Support for Trump",
       subtitle = "California Counties, 2016",
       x = "Two-party vote share for Donald Trump, 2016",
       y = "Frequency")

# Be sure to know that I'm showing you these multiple graphs as increments for
# pedagogical reasons. It would have worked just the same if we had gone
# straight into the last histogram.

# Summary statistics table ------------------------------------------------

# Descriptive statistics for the Trump vote using summary():
summary(cadata$Pct_Trump_16)

# Bivariate bar plot with frequencies -------------------------------------

# Now we want to know: For each region in California, how many counties were won
# by Trump? How many counties were won by Clinton? First, let's run some code to
# add a variable on whether a county was won by Clinton or Trump:
source("https://raw.githubusercontent.com/lfalab/econpoli5/main/add_winner.R")

# We start with something
# similar to the code for a univariate bar plot with frequencies:
ggplot(data = cadata, aes(x = Region, fill = Winner)) + 
  geom_bar()

# The main difference is that we are telling GGPlot to fill up the bar with
# different colors for each category of "Winner." The problem with the plot from
# the code above is that the Trump and Clinton bars are overlapping each other.
# We want them side by side:
ggplot(data = cadata, aes(x = Region, fill = Winner)) + 
  geom_bar(position = "dodge") # Here we tell the bars to "dodge"

# The weird thing about the plot from the code above is that the Clinton bars
# for the Bay Area and Southern California are twice as thick as the other bars.
# This is because Trump didn't win in any county in the Bay Area and SoCal. To
# have all bars with the same width:
ggplot(data = cadata, aes(x = Region, fill = Winner)) + 
  geom_bar(position = position_dodge2(preserve = "single"))

# Now we will add a title, a subtitle, and a y axis label. We will store this
# into an R object called p1:
p1 <- ggplot(data = cadata, aes(x = Region, fill = Winner)) + 
  geom_bar(position = position_dodge2(preserve = "single")) +
  labs(title = "Number of Counties Where Clinton vs. Trump Won",
       subtitle = "By Region of California, 2016",
       y = "Frequency")

# To see how p1 comes out:
p1

# Now let's play around with colors. To have red bars for Trump and blue bars
# for Clinton:
p1 + scale_fill_manual(values = c("Red", "Blue"))

# We can see many more color options at this link:
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Examples of finer grained color options:
p1 + scale_fill_manual(values = c("darkred", "dodgerblue4"))

# If we want to display or bars horizontally instead of vertically, we use
# coord_flip(). We don't have to do this for every graph--I just thought it
# would look nicer this way:
p1 + scale_fill_manual(values = c("darkred", "dodgerblue4")) +
  coord_flip()

# To send the legend to the bottom:
p1 + scale_fill_manual(values = c("darkred", "dodgerblue4")) +
  coord_flip() +
  theme(legend.position = "bottom")

# Stacked bar plot w/ percentages (optional for HW1) ------------------

# Now let's look at the proportions of counties won by Clinton and Trump by
# region of California.

# This involves some more advanced data transformation using a package called
# dplyr. You will learn how to make data transformations like these very soon in
# this course. For now, I will skip the explanation of how dplyr is working to
# transform the data.

plotdata <- cadata %>% 
  group_by(Winner, Region_Abv) %>% 
  tally()

# This transformation generated a separate data frame, made exclusively for this
# plot, which tallies the number of counties won by each candidate in each
# region of California:
plotdata

# Using this data to create a stacked bar plot with proportions, and storing it
# into an object called p2:
p2 <- ggplot(plotdata, aes(fill = Winner, y = n, x = Region_Abv)) + 
  geom_bar(position = "fill", stat = "identity")

# Checking out p2:
p2

# Changing the colors and adding title, subtitle, labels:
p2 + scale_fill_manual(values = c("darkred", "dodgerblue4")) +
  labs(title = "Proportions of Counties Where Clinton vs. Trump Won",
       subtitle = "By Region of California, 2016",
       y = "Proportion",
       x = "Region")

# Cross table (optional for HW1) --------------------------------------

# Now, let's see the same information from the last two plots using tables:
my_crosstab <- table(cadata$Region, cadata$Winner)

# See what my_crosstab produces:
my_crosstab

prop.table(my_crosstab, 1) # Row proportions

# Bar plot with means -----------------------------------------------------

# Now we want to plot the mean household incomes of counties by region of
# California. Let's go straight to the point:
ggplot(data = cadata, 
       aes(x = Region, y = HH_Inc_16)) +
  geom_bar(stat = "summary",
           color = "blue",
           fill = "lightblue") +
  labs(y = "Household Income, 2016",
       x = "Region",
       title = "Mean Household Income Across Counties",
       subtitle = "By Region of California, 2016") +
  theme_minimal() +
  coord_flip()

# Comparative descriptive statistics table (optional for HW1) ---------------

# Comparing means, median, max, min of variables by region of California.
# All variables:
by(cadata, cadata$Region, summary)
# HH_Inc_16 only:
by(cadata[,c("Region", "HH_Inc_16")], cadata$Region, summary)


# Scatter plot ------------------------------------------------------------

# Support for Trump on the y axis and percent white on the x axis:
p3 <- ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16)) +
  geom_point() +
  labs(y = "Two-party vote share for Donald Trump, 2016",
       x = "Percent White, 2016",
       title = "Racial Composition and Election Results",
       subtitle = "California Counties, 2012-16")

p3

# Adding a regression line. You will learn more about regression analysis soon.
# For now, just notice the upward slope:
p3 + geom_smooth(method = "lm")

# Correlation coefficient (optional for HW1) ------------------------------

# Notice that as percent white goes up, support for Trump also goes up.
# This means that there is a positive correlation between the two variables.
# Therefore, we should expect a positive correlation coefficient:
cor(cadata$Pct_White_16, cadata$Pct_Trump_16, use = "complete.obs")
