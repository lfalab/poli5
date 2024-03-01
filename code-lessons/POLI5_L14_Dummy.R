# This R script is a companion to Lesson 14
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load package ------------------------------------------------------------

library(tidyverse)

# Read simplified QoG -----------------------------------------------------

qog <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/simplified_qog.csv")

# Variables:
# cname: Country name
# reg_type: Regime Type (dictatorship or democracy)
# urban: Urbanization status (Urban if more than 50% urban, Rural otherwise)
# peaceful: Level of peacefulness: peaceful or non-peaceful
# freedom_exp: Guarantee of freedom of expression (guaranteed, not guaranteed)
# wdi_expmil: Military expenditure (as % of GDP) 
# wdi_expedu: Education expenditure (as % of GDP)

# Read California counties ------------------------------------------------

cadata <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/ca_counties_regions.csv")

# Variables:
# Pct_White_16: Percentage of white residents, 2016
# Pct_Trump_16: Two-party vote share for Donald Trump, 2016
# HH_Inc_12: Median household income, 2012, adjusted for 2008 dollars
# HH_Inc_16: Median household income, 2016, adjusted for 2008 dollars
# Region: Region of California
# Region_Abv: Region of California (abbreviated)

# Recoding variables to dummy: QoG data -----------------------------------

# Recode `peaceful` variable to dummy. Two ways:

# This is a less efficient way, but I wanted to show you anyhow:
qog <- qog %>% mutate(peace_dummy = case_when(peaceful == "Peaceful" ~ 1,
                                              peaceful != "Peaceful" ~ 0))

# The code below achieves the same result, but more efficiently. 

# It generates a logical vector with TRUE for peaceful countries and FALSE for
# non-peaceful countries.

# As it turns out, R treats logical vectors as if TRUE = 1 and FALSE = 0, so we
# can do the same operations for logical vectors as if they were vectors with 0s
# and 1s.
qog <- qog %>% mutate(peace_dummy = peaceful == "Peaceful")

# We will be using this method from now on.

# Describe the peace dummy:
sum(qog$peace_dummy, na.rm = TRUE)
mean(qog$peace_dummy, na.rm = TRUE)

# Recode freedom of speech dummy (1 if guaranteed, 0 otherwise)
qog <- qog %>% mutate(frexp_dummy = freedom_exp == "Guaranteed")

# Describe the Freedom of expression dummy:
sum(qog$frexp_dummy, na.rm = TRUE)
mean(qog$frexp_dummy, na.rm = TRUE)

# Urban dummy (1 if country is urban, 0 otherwise):
qog <- qog %>% mutate(urban_dummy = urban == "Urban")

# Describe the Urban dummy:
sum(qog$urban_dummy, na.rm = TRUE)
mean(qog$urban_dummy, na.rm = TRUE)

# Democracy dummy (1 if country is democratic, 0 otherwise):
qog <- qog %>% mutate(democ_dummy = reg_type == "Democracy")

# Describe the Democracy dummy:
sum(qog$democ_dummy, na.rm = TRUE)
mean(qog$democ_dummy, na.rm = TRUE)

# Recoding variables to dummy: California data ----------------------------

# Recode SoCal/Bay dummy (1 if county is in Bay Area or SoCal, 0 otherwise):
cadata <- cadata %>%
  mutate(scb_dummy = Region == "Bay Area" | Region == "Southern California")

# Describe the SoCal/Bay dummy:
sum(cadata$scb_dummy, na.rm = TRUE)
mean(cadata$scb_dummy, na.rm = TRUE)

# Clinton "landslide" dummy (1 if Clinton beats Trump by at least a
# 2-to-1 ratio, 0 otherwise)
cadata <- cadata %>% mutate(Clinton_dummy = Pct_Trump_16 <= 33)

# Describe the Clinton "landslide" dummy:
sum(cadata$Clinton_dummy, na.rm = TRUE)
mean(cadata$Clinton_dummy, na.rm = TRUE)

# "Income growth" dummy (1 if income grew from 2012-16 in the county, 0
# otherwise)
cadata <- cadata %>%  mutate(Income_Growth = HH_Inc_16 > HH_Inc_12)

# Describe the Income growth dummy:
sum(cadata$Income_Growth, na.rm = TRUE)
mean(cadata$Income_Growth, na.rm = TRUE)

# 1. Both variables are interval/ratio ------------------------------------

# Example 1.1 -------------------------------------------------------------

# Alternative Hypothesis: Countries that spend higher percentages of their GDP
# in the military spend lower percentages of their GDP in education.

# Null Hypothesis: There is no relationship between military and educational
# spending as percent of GDP.

# Dependent variable: Educational spending as % of GDP (ratio)
# Independent variable: Military spending as % of GDP (ratio)

# Bivariate descriptive analysis:
ggplot(data = qog, aes(x = wdi_expmil, y = wdi_expedu)) +
  geom_point() +
  labs(
    title = "Educational and Military Spending by Country",
    subtitle = "As % of GDP",
    x = "Military Spending as % of GDP",
    y = "Educational Spending as % of GDP",
    caption = "Source: Quality of Government (QoG) Dataset"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 6) +
  ylim(0, 10) +
  theme_bw()

# No recoding needed.

# Regression:
ex11 <- lm(wdi_expedu ~ wdi_expmil, data = qog)
summary(ex11)

# Interpretation: An increase of one in military spending as percent of GDP is
# associated with a decrease of 0.137 in educational spending as percent of GDP.

# Is the result statistically significant at the 0.05 level? Can we reject the
# null hypothesis with 95% confidence?

# p > 0.05. Not statistically significant.
# We fail to reject the null hypothesis.

# Example 1.2 -------------------------------------------------------------

# Alternative Hypothesis: In 2016, voting for Trump in California was higher in
# counties with a greater white population than in counties with a smaller white
# population.

# Null Hypothesis: In the 2016 presidential election, voting for Trump in
# California was no different between counties with a greater white population
# and a smaller white population.

# Dependent variable: Support for Trump (ratio)
# Independent variable: Percent White (ratio)

# Descriptive analysis:
ggplot(data = cadata, aes(x = Pct_White_16, y = Pct_Trump_16)) +
  geom_point() +
  labs(
    title = "Percent White and the Trump Vote",
    subtitle = "California Counties, 2016",
    x = "Percent White, 2016",
    y = "Two-party vote share for Trump, 2016"
  ) +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# No recoding needed.

# Regression:
ex12 <- lm(Pct_Trump_16 ~ Pct_White_16, data = cadata)
summary(ex12)

# Interpretation: In counties of California, an increase of one percentage point
# in white residents is associated with an increase of 0.34 percentage points in
# support for Trump in 2016.

# p < 0.05. Statistically significant.
# We reject the null hypothesis.

# 2. Dependent is ratio/interval. Independent is nominal -------------

# Example 2.1 -------------------------------------------------------------

# Alternative Hypothesis: Peaceful countries spend more on education than
# non-peaceful countries.

# Null Hypothesis: There is no difference in the educational spending of
# peaceful and non-peaceful countries.

# Dependent variable: Educational spending as % of GDP (ratio variable)
# Independent variable: Peacefulness (nominal variable)

# Descriptive analysis -- same plot, different way to filter the data (requires
# dplyr/tidyverse)
qog %>%
  filter(!is.na(peaceful)) %>% # Notice the ordering of the %>%
  ggplot(aes(x = peaceful, y = wdi_expedu)) +
  geom_bar(stat = "summary",
           color = "blue",
           fill = "lightblue") +
  labs(
    y = "Education Spending",
    x = "Peacefulness",
    title = "Mean Education Spending (as % of GDP) and Peace",
    subtitle = "By Country",
    caption = "Source: Quality of Government (QoG) Dataset"
  ) +
  theme_bw()

# Regression:
ex21 <- lm(wdi_expedu ~ peace_dummy, data = qog)
summary(ex21)

# Interpretation: In peaceful countries, educational spending as percent of GDP
# is, on average, 0.57 greater than in non-peaceful countries.

# p < 0.05. Statistically significant.
# We reject the null hypothesis.

# Example 2.2 -------------------------------------------------------------

# Alternative Hypothesis: Counties in the Bay Area and Southern California have
# proportionately fewer white residents than counties in other regions of
# California.

# Null Hypothesis: There is no difference in the percentage of white residents
# between the Bay Area and Southern California, on one hand, and counties in
# other parts of California, on the other.

# Dependent variable: Percent white (ratio)
# Independent variable: Region (nominal)

# Descriptive analysis:
ggplot(data = cadata,
       aes(x = Region, y = Pct_White_16)) +
  geom_bar(stat = "summary",
           color = "blue",
           fill = "lightblue") +
  labs(y = "Percent White (2016)",
       title = "Mean Percentage of White Residents if California Counties",
       subtitle = "By Region") +
  theme_bw()

# Regression:
ex22 <- lm(Pct_White_16 ~ scb_dummy, data = cadata)
summary(ex22)

# Interpretation: In counties in the Bay Area and Southern California, the
# percentage of white residents is, on average, 18 percentage points lower than
# in counties in other regions of California.

# p < 0.05. Statistically significant.
# We reject the null hypothesis.

# 3. Dependent is nominal. Independent is ratio/interval -------------

# Example 3.1 -------------------------------------------------------------

# Hypothesis: Countries with higher military spending are less likely to
# guarantee freedom of expression for its citizens.
# Null Hypothesis: There is no difference in the likelihood of guaranteeing
# freedom of expression between countries with higher and lower levels of
# military spending.

# Dependent variable: Guarantee of freedom of expression (nominal)
# Independent variable: Military spending (as % of GDP)

# Descriptive analysis with box plots:
qog %>% 
  # Drop NAs
  filter(!is.na(freedom_exp)) %>% 
  # Start plotting
  ggplot(aes(x = freedom_exp, y = wdi_expmil)) +
  geom_boxplot() +
  labs(title = "Military Expenditure and Freedom of Expression",
       y = "Military Expenditure as % of GDP",
       x = "Freedom of Expression") +
  theme_bw()

# Regression:
ex31 <- lm(frexp_dummy ~ wdi_expmil, data = qog)
summary(ex31)

# Interpretation: For an increase of one in military spending as percent of GDP,
# the probability of a country guaranteeing freedom of expression decreases by
# approximately 6 percentage points.

# p < 0.05. Statistically significant.
# We reject the null hypothesis.

# Example 3.2 -------------------------------------------------------------

# Alternative Hypothesis: In California counties with higher household income
# levels, Clinton was more likely to beat Trump by a landslide (at least 2-1
# ratio).

# Null Hypothesis: There is no relationship between household income and the
# likelihood of Clinton beating Trump by a landslide.

cadata %>% 
  # Start plotting
  ggplot(aes(x = Clinton_dummy, y = HH_Inc_16)) +
  geom_boxplot() +
  labs(title = "Did Clinton Get Landslide Wins in Richer Counties?",
       subtitle = "California Counties, 2016 Presidential Election",
       y = "Household Income 2016",
       x = "Clinton Landslide") +
  scale_x_discrete(labels = c("No Landslide Win", "Landslide Win")) +
  theme_bw()

# Regression:
ex32 <- lm(Clinton_dummy ~ HH_Inc_16, data = cadata)
summary(ex32)

# Slope is 1.590e-05. What number is this? To find out:
format(1.590e-05, scientific = FALSE)

# We could interpret this result by saying that for a 1 dollar increase in HH
# income, the likelihood of Clinton winning the county by a landslide is 0.00159
# percentage points higher.

# But in this case, should we be talking about a one dollar increase? Household
# income is usually a five-digit figure. If instead of talking about a one
# dollar increase we move onto the thousands, or tens of thousands, we should
# get at more meaningful ways to interpret the data.

# Interpretation: For an increase of 10,000 dollars in the household income of a
# county of California, the likelihood of Clinton beating Trump by a landslide
# increases by approximately 16 percentage points.

# That's more like it! And...

# p < 0.05. Statistically significant.
# We reject the null hypothesis.

# 4. Both variables are nominal --------------------------------------

# Example 4.1 -------------------------------------------------------------

# Alternative Hypothesis: Urbanized countries are more likely to be democracies
# than rural countries.
# Null Hypothesis: There is no difference in the regime types of urban and rural
# countries.

# Dependent variable: Regime type (nominal)
# Independent variable: Urbanization status (nominal)

# Descriptive analysis
qog %>% 
  # Drop NAs
  filter(!is.na(urban)) %>% 
  # Start plotting
  ggplot(aes(x = urban, fill = reg_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Urbanization Status and Regime Type",
       fill = "Regime Type",
       x = "Urbanization Status",
       y = "Frequency",
       caption = "Source: Quality of Government (QoG) Dataset") +
  theme_bw()

# Run regression:
ex41 <- lm(as.numeric(democ_dummy) ~ urban_dummy, data = qog)
summary(ex41)

# Interpretation: Urban countries are 22.5 percentage points more likely to be
# democracies than rural countries.

# p < 0.05. Statistically significant.
# We reject the null hypothesis.

# Example 4.2 -------------------------------------------------------------

# Alternative Hypothesis: Counties in Southern California and the Bay Area were
# more likely to have gained income between 2012 and 2016 than counties in other
# regions of California.

# Null Hypothesis: Counties in Southern California and the Bay Area were no more
# likely to have gained income between 2012 and 2016 than counties in other
# regions of California.

# Dependent variable: Whether a county has gained income from 2012-16 (nominal)
# Independent variable: Region (nominal)

# Descriptive analysis
cadata %>%
  # Drop NAs
  filter(!is.na(Income_Growth)) %>%
  # Start plotting
  ggplot(aes(x = scb_dummy,
             fill = Income_Growth)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Income Gain in California Counties, 2012-16",
    subtitle = "SoCal and Bay Area Vs. Central and Northern CA",
    fill = "Income Change Status",
    x = "Region",
    y = "Frequency"
  ) +
  scale_fill_discrete(labels = c('No Income Growth', 'Income Growth')) +
  scale_x_discrete(labels = c("Central and Northern CA", "SoCal and Bay Area")) +
  theme_bw()

# Regression
ex42 <- lm(Income_Growth ~ scb_dummy, data = cadata)
summary(ex42)

# Interpretation: Counties in SoCal and the Bay are 18 percentage points more
# likely to have gained income from 2012-16.

# p > 0.05. Not statistically significant.
# We fail to reject the null hypothesis.