# This R script is a companion to Lesson 16
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load tidyverse ----------------------------------------------------------

library(tidyverse)

# Read data ---------------------------------------------------------------

cadata <- read_csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/ca_counties_regions.csv") %>% 
  mutate(Inc_Change = (HH_Inc_16-HH_Inc_12)/HH_Inc_12*100)

# Simple regression 1: Pct white and Trump vote ---------------------------

# Histogram of support for Trump (dependent variable)
ggplot(data = cadata, aes(x = Pct_Trump_16)) + 
  geom_histogram(breaks = seq(0, 100, 10), 
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Distribution of Support for Trump",
       subtitle = "California Counties, 2016",
       x = "Two-party vote share for Donald Trump, 2016",
       y = "Frequency") +
  theme_bw()

# Histogram of percent white (independent variable)
ggplot(data = cadata, aes(x = Pct_White_16)) + 
  geom_histogram(breaks = seq(0, 100, 10), 
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Distribution of Percent White",
       subtitle = "California Counties, 2016",
       x = "Percent White, 2016",
       y = "Frequency") +
  theme_bw()

# Scatter plot
ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16)) + 
  geom_point() +
  labs(title = "Percent White and the Trump Vote",
       subtitle = "California Counties, 2016",
       x = "Percent White, 2016",
       y = "Two-party vote share for Trump, 2016") +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# Regression analysis: just give me the basics:
lm(Pct_Trump_16 ~ Pct_White_16, data = cadata)

# Tell me more: p-value, R-squared...
simple1 <- lm(Pct_Trump_16 ~ Pct_White_16, data = cadata)

# Tell me more: p-values, R-squared...
summary(simple1)

# Simple regression 2: Income change and Trump vote -----------------------

# Histogram of percent income change (independent variable)
ggplot(data = cadata, aes(x = Inc_Change)) + 
  geom_histogram(breaks = seq(-15, 15, 5), 
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(-15, 15, 5)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  labs(title = "Distribution of Income Change, 2012-16",
       subtitle = "California Counties",
       x = "Income Change 2012-16",
       y = "Frequency") +
  theme_bw()

# Scatter plot
ggplot(data = cadata, aes(x = Inc_Change, y = Pct_Trump_16)) + 
  geom_point() +
  labs(title = "Income Change and the Trump Vote",
       subtitle = "California Counties, 2012-16",
       x = "Income Change, 2012-16",
       y = "Two-party vote share for Trump, 2016") +
  scale_x_continuous(breaks = seq(-15, 15, 5)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# Regression analysis: just give me the basics:
lm(Pct_Trump_16 ~ Inc_Change, data = cadata)

# Tell me more: p-value, R-squared...
simple2 <- lm(Pct_Trump_16 ~ Inc_Change, data = cadata)

# Tell me more: p-values, R-squared...
summary(simple2)

# Multivariate regression -------------------------------------------------

# Regression analysis: just give me the basics:
lm(Pct_Trump_16 ~ Pct_White_16 + Inc_Change, data = cadata)

# Tell me more: p-value, R-squared...
multi <- lm(Pct_Trump_16 ~ Pct_White_16 + Inc_Change, data = cadata)

# Tell me more: p-values, R-squared...
summary(multi)