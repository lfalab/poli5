# This R script is a companion to Lesson 13
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load packages -----------------------------------------------------------

# Tidyverse to transform data and make plots
library(tidyverse)

# Example regression ------------------------------------------------------

# Read dataset
cadata <-
  read_csv(
    "https://raw.githubusercontent.com/lfalab/econpoli5/main/ca_counties_regions.csv"
  )

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
my_reg <- lm(Pct_Trump_16 ~ Pct_White_16, data = cadata)

# Tell me more: p-values, R-squared...
summary(my_reg)

# Number of observations:
nobs(my_reg)

# 95% confidence interval of the slope:
confint(my_reg ,"Pct_White_16",level = 0.95)

# Alternative Hypothesis: In 2016, voting for Trump in California was higher in
# counties with a greater white population than in counties with a smaller white
# population.

# Null Hypothesis: In the 2016 presidential election, voting for Trump in
# California was no different between counties with a greater white population
# and a smaller white population.

# P-value of the slope = 0.000473
# p < 0.05

# We reject the null hypothesis!

# In case you are curious about what 6.5e-05 is...
format(6.5e-05, scientific = FALSE)
# It's the same as 0.000065!

# Regression 1: Foreign-born residents and population density -------------

# Read dataset, calculate foreign_pct2010
oicc <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv") %>% 
  mutate(foreign_pct2010 = foreign_share2010*100)

# Histogram of the dependent variable:
ggplot(data = oicc, aes(x = foreign_pct2010)) +
  geom_histogram(breaks = seq(0, 75, 2.5),
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  labs(title = "Distribution of the Percentage of Foreign Residents",
       subtitle = "By U.S. County, 2010",
       x = "Percent Foreign Residents, 2010",
       y = "Frequency") +
  theme_bw()

# Histogram of the independent variable:
ggplot(data = oicc, aes(x = popdensity2010)) +
  geom_histogram(breaks = seq(0, 5000, 125),
                 fill = "grey", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 5000, 500)) +
  labs(title = "Distribution of the Population Density",
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
  labs(title = "Percent Foreign Residents and Population Density",
       subtitle = "By U.S. County, 2010",
       x = "Population Density, 2010",
       y = "Percent Foreign Residents, 2010") +
  theme_bw()

# Your turn: interpret the regression results!
reg1 <- lm(foreign_pct2010 ~ popdensity2010, data = oicc)
summary(reg1)

# Regression 2: Children in poverty and teen pregnancy rates --------------

# Read dataset, rename and transform variables
chr <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/chr.csv") %>% 
  rename(teen_births = measure_14_value) %>% 
  mutate(pct_children_poverty = measure_24_value*100)

# Histogram of the dependent variable:
ggplot(data = chr, aes(x = teen_births)) +
  geom_histogram(breaks = seq(0, 120, 5),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 120, 10)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  labs(title = "Distribution of Teen Births Across U.S. Counties",
       subtitle = "Per 100,000 people",
       x = "Teen Births per 100,000 people",
       y = "Frequency") +
  theme_bw()

# Histogram of the independent variable:
ggplot(data = chr, aes(x = pct_children_poverty)) +
  geom_histogram(breaks = seq(0, 70, 5),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 70, 5)) +
  scale_y_continuous(breaks = seq(0, 700, 100)) +
  labs(title = "Distribution of Percent Childen in Poverty",
       subtitle = "By U.S. County",
       x = "Percent Childen in Poverty",
       y = "Frequency") +
  theme_bw()

# Scatter plot
ggplot(data = chr, aes(x = pct_children_poverty, y = teen_births)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 65, 5)) +
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  labs(title = "Teen Births and Percent Childen in Poverty",
       subtitle = "By U.S. County",
       x = "Percent Children in Poverty",
       y = "Teen Births per 100,000 people") + 
  theme_bw()

# Your turn: interpret the regression results!
reg2 <- lm(teen_births ~ pct_children_poverty, data = chr)
summary(reg2)

# Regression 3: Voter turnout and trust in elections ----------------------

# Read dataset, rename variables
qog <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/qog.csv") %>% 
  rename(voter_turnout = cpds_vt,
         trust_election = pei_eir)

# Histogram of the dependent variable
ggplot(data = qog, aes(x = voter_turnout)) +
  geom_histogram(breaks = seq(40, 100, 5),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(40, 100, 5)) +
  scale_y_continuous(breaks = 1:7) +
  labs(title = "Distribution of Percent Voter Turnout",
       subtitle = "By Country",
       x = "Percent Voter Turnout in Presidential Elections",
       y = "Frequency") +
  theme_bw()

# Histogram of the independent variable
ggplot(data = qog, aes(x = trust_election)) +
  geom_histogram(breaks = 1:10,
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(title = "Distribution of Trust in Elections",
       subtitle = "On scale from 1 to 10",
       x = "Level of Trust in Elections",
       y = "Frequency") +
  theme_bw()

# Scatter plot
ggplot(data = qog, aes(x = trust_election, y = voter_turnout)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = 5:10) +
  xlim(5, 10) +
  scale_y_continuous(breaks = seq(40, 100, 5)) +
  labs(title = "Trust in Elections and Voter Turnout",
       subtitle = "By Country",
       x = "Level of Trust in Elections",
       y = "Percent Voter Turnout in Presidential Elections") + 
  theme_bw()

# Your turn: interpret the regression results!
reg3 <- lm(voter_turnout ~ trust_election, data = qog)
summary(reg3)

# Regression 4: Educational attainment and work in the farm ---------------

# Read dataset, rename variables
ihds <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/ihds.csv") %>%
  rename(farm_work = FM37,
         education = ED6)

# Histogram of the dependent variable
ggplot(data = ihds, aes(x = farm_work)) +
  geom_histogram(breaks = seq(0, 360, 30),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  scale_y_continuous(breaks = seq(0, 9000, 1000)) +
  labs(title = "Distribution of Days/Year Working on the Farm",
       subtitle = "By Person (India Human Development Survey)",
       x = "Days per Year Working on the Farm",
       y = "Frequency") +
  theme_bw()

# Histogram of the independent variable
ggplot(data = ihds, aes(x = education)) +
  geom_histogram(breaks = seq(0, 16, 2),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  scale_y_continuous(breaks = seq(0, 80000, 10000)) +
  labs(title = "Distribution of Years of Education",
       subtitle = "By Person (India Human Development Survey)",
       x = "Years of Education",
       y = "Frequency") +
  theme_bw()

# Scatter plot
ggplot(data = ihds, aes(x = education, y = farm_work)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  scale_y_continuous(breaks = seq(0, 360, 60)) +
  labs(title = "Time Spent Working in the Farm and Education",
       subtitle = "By Person (India Human Development Survey)",
       x = "Years of Education",
       y = "Days per Year Working on the Farm") +
  theme_bw()

# Notice that this scatter plot doesn't look good, even though both our
# variables are ratio. For each amount of years of education, the values for
# "days working on the farm" are all over the place, which makes it hard to
# visualize. If you ever generate a scatter plot that looks like this when
# working on your final project, it is worth exploring ways to improve the
# visualization. One way is to transform the data using tidyverse so that we
# only have the mean number of days working on the farm for each amount of years
# of education:
ihds %>% 
  group_by(education) %>% 
  summarize(mean_farm = mean(farm_work, na.rm = TRUE)) %>% 
  ggplot(aes(x = education, y = mean_farm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  labs(title = "Time Spent Working in the Farm and Education",
       subtitle = "By Person (India Human Development Survey)",
       x = "Years of Education",
       y = "Days per Year Working on the Farm (Mean)") +
  theme_bw()

# Your turn: interpret the regression results!
reg4 <- lm(farm_work ~ education, data = ihds)
summary(reg4)