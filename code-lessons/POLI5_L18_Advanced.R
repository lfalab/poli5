# This R script is a companion to Lesson 17
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load tidyverse ----------------------------------------------------------

library(tidyverse)

# Read data ---------------------------------------------------------------

# Read data
qog <- read_csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/qog_ca18.csv")

# GDP v. Life Expectancy --------------------------------------------------

# Histogram of GDP without log
qog %>% 
  ggplot(aes(x = gdp/1000)) +
  geom_histogram(breaks = seq(0, 100, 10),
                 color = "black",
                 fill = "gray") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "GDP Per Capita: A Highly Skewed Distribution",
       x = "GDP Per Capita (in thousand USD, 2010)",
       y = "Frequency") +
  theme_bw()

# Scatter plot without log
qog %>% 
  ggplot(aes(x = gdp/1000, y = life_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(50, 90, 10), limits = c(50, 90)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title = "GDP Per Capita and Life Expentancy by Country",
    y = "Life Expectancy",
    x = "GDP per Capita (in thousand USD, 2010)",
    caption = "Source: Quality of Government Dataset"
  ) +
  theme_bw()

# Taking the log ----------------------------------------------------------

# Histogram of Log of GDP
qog %>% 
  ggplot(aes(x = log(gdp))) +
  geom_histogram(breaks = seq(4, 12, 1),
                 color = "black",
                 fill = "gray") +
  scale_x_continuous(breaks = seq(4, 12, 1), limits = c(5, 12)) +
  labs(title = "Log of GDP: A Not-so-skewed Distribution",
       x = "Log GDP Per Capita (in 2010 USD)",
       y = "Frequency") +
  theme_bw()

# Scatter plot with log
qog %>% 
  ggplot(aes(x = log(gdp), y = life_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(50, 90, 10), limits = c(50, 90)) +
  scale_x_continuous(breaks = seq(5, 12, 1), limits = c(5, 12)) +
  labs(
    title = "Log GDP Per Capita and Life Expentancy by Country",
    y = "Life Expectancy",
    x = "Log GDP Per Capita (in 2010 USD)",
    caption = "Source: Quality of Government Dataset"
  ) +
  theme_bw()

# Compare Regressions:

# Without log
m1 <- lm(life_exp ~ I(gdp/1000), data = qog)
summary(m1)

# With log
m2 <- lm(life_exp ~ log(gdp), data = qog)
summary(m2)


# Regression table --------------------------------------------------------

# Stargazer cheat sheet:
# https://www.jakeruss.com/cheatsheets/stargazer/

# Make regression table:
# Enter regression objects
library(stargazer)
stargazer(m1, m2,
          # Specify format
          type = "html", 
          # Label independent variables
          covariate.labels = c("GDP Per Capita (in thousand USD)", 
                               "Log GDP Per Capita"),
          # Label dependent variable
          dep.var.labels   = "Life Expectancy",
          # Saves table as .html file (open file and copy-paste into Word doc)
          out = "life_exp_models.html")

# Plot w/ dot sizes & colors depicting data -------------------------------

# Make plot
qog %>% ggplot(aes(
  x = log(gdp),
  y = life_exp,
  # Sizes of the dots will represent the population
  size = population,
  # Color of the dots will represent the country's V-Dem score
  color = lib_dem
)) +
  geom_point() +
  scale_x_continuous(breaks = seq(4, 12, 2), limits = c(5, 12)) +
  scale_y_continuous(breaks = seq(50, 90, 10)) +
  # Specify how to label the color scale
  scale_color_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  # Specify how to label the sizes of the dots
  scale_size_continuous(breaks = c(250, 500, 1000)) +
  labs(
    title = "Can Money Buy a Long Life in a Democracy?",
    subtitle = "People in Richer Countries Live Longer, Under More Democratic Regimes",
    y = "Life Expectancy",
    x = "Log of GDP per Capita (2010 U.S. Dollars)",
    color = "Liberal\nDemocracy\nScore",
    size = "Population\n(in millions)",
    caption = "Source: Quality of Government Dataset"
  ) +
  theme_bw()
