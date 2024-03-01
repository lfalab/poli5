# This R script is a companion to Lesson 12
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load packages -----------------------------------------------------------

# Tidyverse for data manipulation and plotting
library(tidyverse)

# Simplified QoG data -----------------------------------------------------

qog <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/simplified_qog.csv")

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

# Peacefulness and education expenditure ----------------------------------

# Distribution of peacefulness
ggplot(data = qog %>% filter(!is.na(peaceful)), aes(x = peaceful)) + 
  geom_bar(fill = "white",
           color = "black") +
  labs(title = "Number of Peaceful Vs. Non-Peaceful Countries",
       subtitle = "By Country",
       x = "Peacefulness",
       y = "Frequency") +
  theme_bw()
# Distribution table
table(qog$peaceful)

# Distribution of education expenditure as % of GDP
ggplot(data = qog, aes(x = wdi_expedu)) + 
  geom_histogram(breaks = seq(0, 13, 1), 
                 fill = "white", 
                 color = "black") +
  scale_x_continuous(breaks = seq(0, 13, 1)) +
  labs(title = "Distribution of education expenditure (as % of GDP)",
       subtitle = "By Country",
       x = "Education expenditure",
       y = "Frequency") +
  theme_bw()
# Summary statistics
summary(qog$wdi_expedu)

# Bar plot with means: peace and education spending
ggplot(data = qog %>% filter(!is.na(peaceful)),
       aes(x = peaceful, y = wdi_expedu)) +
  geom_bar(stat = "summary",
           color = "blue",
           fill = "lightblue") +
  labs(
    y = "Education Spending",
    x = "Peacefulness",
    title = "Mean Education Spending (as % of GDP) and Peace",
    subtitle = "By Country"
  ) +
  theme_bw()

# Box plot: peace and education spending
ggplot(data = qog %>% filter(!is.na(peaceful)),
       aes(x = peaceful, y = wdi_expedu)) +
  geom_boxplot() +
  labs(
    y = "Education Spending",
    x = "Peacefulness",
    title = "Education Spending (as % of GDP) and Peace",
    subtitle = "By Country"
  ) +
  theme_bw()

# Difference in means test ------------------------------------------------

t.test(wdi_expedu ~ peaceful, data = qog)
