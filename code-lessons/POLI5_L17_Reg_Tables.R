# This R script is a companion to Lesson 17
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load tidyverse ----------------------------------------------------------

library(tidyverse)

# Class activity example: correlates of corruption ------------------------

# Read data
qog <- read_csv("https://raw.githubusercontent.com/lfalab/econpoli5/main/qog_ca17.csv")

# Dependent variable: Corruption Perceptions Score. Scale of 0-100 where 0 means
# no perception of corruption and 100 means complete perception of corruption.
ggplot(data = qog, aes(x = corrup)) +
  geom_histogram(breaks = seq(0, 100, 10),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Distribution of Corruption\nPerception Score",
       subtitle = "Higher Scores Denote More Corruption",
       x = "Corruption Perception Score",
       y = "Frequency") +
  theme_bw()

# Independent Variable 1: Political Participation. Scale from 1-10. Higher
# scores refer better conditions of political participation and other political
# freedoms.
ggplot(data = qog, aes(x = pol_part)) +
  geom_histogram(breaks = 0:10,
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Distribution of Political\nParticipation Score",
       subtitle = "Higher Scores Denote More Participation",
       x = "Political Participation Score",
       y = "Frequency") +
  theme_bw()

# Independent Variable 2: Literacy rate
ggplot(data = qog, aes(x = lit_rate)) +
  geom_histogram(breaks = seq(0, 100, 10),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Distribution of Literacy Rate",
       subtitle = "Population Age 15 and Above",
       x = "Literacy Rate",
       y = "Frequency") +
  theme_bw()

# Potential Confound: Urbanization (percentage of the population living in urban
# areas)
ggplot(data = qog, aes(x = pct_urban)) +
  geom_histogram(breaks = seq(0, 100, 10),
                 color = "black",
                 fill = "grey") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Distribution of Urbanization Rate",
       subtitle = "Percentage of the Population Living in Urban Areas",
       x = "Urbanization",
       y = "Frequency") +
  theme_bw()

# Hypothesis 1: Countries with more political participation have lower levels of
# corruption.
# Theory: More political participation entails greater degrees of political
# accountability, thus making corruption less frequent.
# Scatter plot:
qog %>% ggplot(aes(x = pol_part, y = corrup)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 90, 10)) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Corruption and Political Participation",
       x = "Political Participation\n(Higher Scores = More Participation)",
       y = "Corruption Perception Score\n(Higher Scores = More Corruption)") +
  theme_bw()

# Hypothesis 2: Countries with higher literacy rates have lower levels of
# corruption.
# Theory: In countries where more people can read and write, people are better
# able to hold politicians accountable as they are better able to seek
# information and communicate their views.
# Scatter plot:
qog %>% ggplot(aes(x = lit_rate, y = corrup)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 90, 10)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  labs(title = "Corruption and Literacy Rate",
       x = "Literacy Rate\n(Population Age 15 and Above)",
       y = "Corruption Perception Score\n(Higher Scores = More Corruption)") +
  theme_bw()

# Potential confounding variable: Urbanization.
# Justification: A country's urbanization level may cause changes in corruption
# levels and be correlated with political participation and literacy rates. Life
# in a more densely populated area can facilitate a more active role of civil
# society in politics and help citizens fight corruption. City life can also
# make political participation more likely and boost literacy rates by making
# societies more likely to establish labor unions, universities, and a free
# press.
qog %>% ggplot(aes(x = pct_urban, y = corrup)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 90, 10)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Corruption and Urbanization",
       x = "Urbanization Rate",
       y = "Corruption Perception Score\n(Higher Scores = More Corruption)") +
  theme_bw()

# Model 1: Bivariate regression (just political participation as independent
# variable)
qog_m1 <- lm(corrup ~ pol_part, data = qog)
summary(qog_m1)

# Model 2: Bivariate regression (just literacy rate as independent variable)
qog_m2 <- lm(corrup ~ lit_rate, data = qog)
summary(qog_m2)

# Model 3: Multivariate regression (participation, literacy rate, socio-economic
# level)
qog_m3 <- lm(corrup ~ pol_part + lit_rate + pct_urban, data = qog)
summary(qog_m3)

# Publishable Regression Table --------------------------------------------

# Install package (Stargazer)
library(stargazer)

# Consult the cheat sheet to learn how to customize your table:
# https://www.jakeruss.com/cheatsheets/stargazer/

          # Enter regression objects
stargazer(qog_m1, qog_m2, qog_m3,
          # Specify format
          type = "html", 
          # Label independent variables
          covariate.labels = c("Political Participation", 
                               "Literacy Rate", 
                               "Percent Urban"),
          # Label dependent variable
          dep.var.labels   = "Corruption Perception Score",
          # Saves table as .html file (open file and copy-paste into Word doc)
          out = "qog_models.html")