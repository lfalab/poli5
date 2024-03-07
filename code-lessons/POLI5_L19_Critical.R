# This R script is a companion to Lesson 17
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load tidyverse ----------------------------------------------------------

library(tidyverse)

# Logarithmic transformations ---------------------------------------------

# Read dataset, calculate foreign_pct2010
oicc <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv") %>% 
  mutate(foreign_pct2010 = foreign_share2010*100)

# Histogram of the dependent variable:
ggplot(data = oicc, aes(x = foreign_pct2010)) +
  geom_histogram(breaks = seq(0, 75, 2.5),
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  labs(title = "Distribution of Percent Foreign Residents",
       subtitle = "By U.S. County, 2010",
       x = "Percent Foreign Residents, 2010",
       y = "Frequency") +
  theme_bw()

# Histogram of the independent variable:
ggplot(data = oicc, aes(x = popdensity2010)) +
  geom_histogram(breaks = seq(0, 5000, 125),
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 5000, 1000)) +
  labs(title = "Distribution of Population Density",
       subtitle = "By U.S. County, 2010",
       x = "Population Density, 2010",
       y = "Frequency") +
  theme_bw()

# Scatter plot
ggplot(data = oicc, aes(x = popdensity2010, y = foreign_pct2010)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 70000, 10000)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  labs(title = "Percent Foreign Residents and\nPopulation Density",
       subtitle = "By U.S. County, 2010",
       x = "Population Density, 2010",
       y = "Percent Foreign Residents, 2010") +
  theme_bw()

# Run regression without log transform
reg1 <- lm(foreign_pct2010 ~ popdensity2010, data = oicc)

summary(reg1)

# Distribution of ln(population density)
ggplot(data = oicc, aes(x = log(popdensity2010))) +
  geom_histogram(breaks = seq(-4, 12, 2),
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(-4, 12, 2)) +
  labs(title = "Distribution of Log Population Density",
       subtitle = "By U.S. County, 2010",
       x = "Log Population Density, 2010",
       y = "Frequency") +
  theme_bw()

# Distribution of ln(percent foreign)
ggplot(data = oicc, aes(x = log(foreign_pct2010))) +
  geom_histogram(breaks = seq(-4, 6, 1),
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(-4, 6, 1)) +
  labs(title = "Distribution of Log Percent Foreign",
       subtitle = "By U.S. County, 2010",
       x = "Log Percent Foreign, 2010",
       y = "Frequency") +
  theme_bw()

# Scatter plot with logarithmic scales
ggplot(data = oicc, aes(x = log(popdensity2010), y = log(foreign_pct2010))) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(-4, 12, 2)) +
  scale_y_continuous(breaks = seq(-4, 6, 1)) +
  labs(title = "Log Percent Foreign Residents and\nLog Population Density",
       subtitle = "By U.S. County, 2010",
       x = "Log Population Density, 2010",
       y = "Log Percent Foreign Residents, 2010") +
  theme_bw()

# Run regression with log scales
reg2 <- lm(log(foreign_pct2010+1) ~ log(popdensity2010), data = oicc)

summary(reg2)


# Plot w/ dot sizes & colors depicting data -------------------------------

# Read data
qog <- read_csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/qog_ca19.csv")

# Make plot
qog %>% ggplot(aes(
  x = log_gdp,
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