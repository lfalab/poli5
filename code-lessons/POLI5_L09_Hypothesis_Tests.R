# This R script is a companion to Lesson 9
# POLI5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load packages -----------------------------------------------------------

# Tidyverse for data manipulation and plotting
library(tidyverse)

# Simplified QoG data -----------------------------------------------------

qog <- read.csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/simplified_qog.csv")

# "Codebook":
# cname: Country name
# reg_type: Regime Type (dictatorship or democracy)
# urban: Urbanization status (Urban if more than 50% urban, Rural otherwise)
# peaceful: Level of peacefulness: peaceful or non-peaceful
# wdi_expmil: Military expenditure (as % of GDP) 
# wdi_expedu: Education expenditure (as % of GDP)

# From the original QoG cross-section codebook: "In the QoG Basic CS dataset,
# data from and around 2017 is included. Data from 2017 is prioritized, however,
# if no data is available for a country for 2017, data for 2018 is included. If
# no data exists for 2018, data for 2016 is included, and so on up to a maximum
# of +/- 3 years."

# We will assume that QoG data is a representative sample of countries.

# Descriptive analysis ----------------------------------------------------

# Distribution table of urbanization status
table(qog$urban)
# Univariate bar plot with frequencies
ggplot(data = filter(qog, !is.na(urban)), aes(x = urban)) + 
  geom_bar(fill = "white",
           color = "black") +
  labs(title = "Number of Rural Vs. Urban Countries",
       x = "Urbanization Status",
       y = "Frequency")

# Distribution table of regime type
table(qog$reg_type)
# Univariate bar plot with frequencies
ggplot(data = qog, aes(x = reg_type)) + 
  geom_bar(fill = "white",
           color = "black") +
  labs(title = "Number of Democracies vs. Dictatorships",
       x = "Regime Type",
       y = "Frequency")

# Bivariate
ggplot(data = filter(qog,!is.na(urban) &
                       !is.na(reg_type)), aes(x = urban, fill = reg_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Urbanization Status and Regime Type",
       fill = "Regime Type",
       x = "Urbanization Status",
       y = "Frequency")

# Now, let's see a cross table:
urb_regtype <- table(qog$urban, qog$reg_type)
urb_regtype

# What are the proportions of democracies in urban vs. rural countries?
prop.table(urb_regtype, 1) # Row proportions

# Difference in proportions test
# The Yates's continuity correction is disabled for pedagogical reasons.
prop.test(urb_regtype, correct = FALSE)

# The 95% confidence interval estimate of the difference between the democratic
# proportion of urban countries and the democratic proportion of rural countries
# is between 8 percentage points and 37 percentage points.