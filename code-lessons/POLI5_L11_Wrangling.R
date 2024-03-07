# This R script is a companion to Lesson 11
# Poli 5: Data Analytics for the Social Sciences, UCSD
# Leo Falabella

# Load packages -----------------------------------------------------------

# Tidyverse to clean data (dplyr) and make plots (ggplot)
library(tidyverse)

# Opportunity Insights County data ----------------------------------------

# Read data from the web
dat <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv")

# Notice how this time we used read_csv() instead of read.csv()

dat

# Add state and county names, population
source("https://raw.githubusercontent.com/lfalab/poli5/main/code-source/add_st_cty_names_pop.R")

# Three new variables have been added: datname, stname, and pop_2010:
glimpse(dat)

# Select and rename variables ---------------------------------------------

# In this exercise, we will focus on a few variables in the Opportunity Insights
# data. Let's use the select() function in dplyr to select them.

dat <- select(dat, # Start with the data frame, then list the variables
              stname, # Note that with select() we don't have to use quotation marks
              ctyname,
              gsmn_math_g3_2013,
              frac_coll_plus2010,
              med_hhinc2016,
              poor_share2010,
              pop_2010)

# Look at dat again
dat

# If we only want the data between stname and gsmn_math_g3_2013:
select(dat, stname:gsmn_math_g3_2013)

# if we want to drop med_hhinc2016:
select(dat, -med_hhinc2016)

# Let's rename these columns to make our lives easier from now on...
dat <- rename(dat,
              state = stname,
              county = ctyname,
              math_score = gsmn_math_g3_2013,
              college_share = frac_coll_plus2010,
              income = med_hhinc2016,
              poor_share = poor_share2010,
              pop = pop_2010)

# One more look
dat

# Filter, arrange, summarize ----------------------------------------------

# filter() allows you to subset your data according to values. Examples:
# If we only want counties in California:
filter(dat, state == "California")
# If we only want counties in California, dropping if math_score is missing:
filter(dat, state == "California" & !is.na(math_score))
# If we only want counties with population > 2000000
filter(dat, pop > 2000000)
# If we only want counties in CA, with pop > 2000000
filter(dat, state == "California" & pop > 2000000)

# Sorting data with arrange()
# Ascending order: which counties rank lowest in poor_share?
arrange(dat, poor_share)
# Descending order: which counties rank highest in math_score?
arrange(dat, desc(math_score))

# Summarizing data with summarize()
# summarize() in its crudest form:
summarize(dat, total_pop = sum(pop, na.rm = TRUE))

# We just got the US population in 2010: 309 million. But that's kinda meh. We
# could have done the same with this line of code:
sum(dat$pop, na.rm = TRUE)

# summarize() is more powerful when combined with group_by(). Let's use
# group_by() to group our data by state:
by_state <- group_by(dat, state)
# See what we got now:
by_state

# Now, to get the total population by state:
summarize(by_state, total_pop = sum(pop, na.rm = TRUE))

# The pipe operator (%>%) -------------------------------------------------

# We have seen how summarize() can be powerful when combined with group_by().
# Another nice thing about dplyr is that we can achieve the same result without
# going through the intermediate step of creating a new data frame, as we
# created by_state. This is where the pipe operator (%>%) comes in. Let's see it
# at work.

# Instead of
group_by(dat, state)

# We could have done
dat %>% group_by(state)

# To get the total population by state, skipping the intermediate data frame:
dat %>% group_by(state) %>% summarize(total_pop = sum(pop, na.rm = TRUE))

# We can continue adding one pipe operator after another. Suppose we want to get
# the population by state, and only keep states with a population greater than 5
# million:
dat %>% 
  group_by(state) %>% 
  summarize(total_pop = sum(pop, na.rm = TRUE)) %>% 
  filter(total_pop > 5000000)

# Get total population by state, subset states with pop > 5M, and rank these
# states from highest to lowest population:
dat %>% 
  group_by(state) %>% 
  summarize(total_pop = sum(pop, na.rm = TRUE)) %>% 
  filter(total_pop > 5000000) %>%
  arrange(desc(total_pop))

# Mutate ------------------------------------------------------------------

# In this video, you will learn to create and modify variables with dplyr using
# the mutate() function. You can check the tidyverse website for more:
# https://dplyr.tidyverse.org/reference/mutate.html

# Let's look at our county data once more:
dat

# We may want the percent of college educated adults instead of the proportion.
# To achieve this with mutate():
dat %>% 
  mutate(pct_college = college_share*100)

# We may also prefer to work with income on the logarithmic scale:
dat %>% 
  mutate(log_income = log(income))

# We may also multiply population by poor_share to get the absolute number of
# people who live below the poverty line in each county:
dat %>% 
  mutate(poor_n = poor_share*pop)

# To do all at once:
dat %>% 
  mutate(log_income = log(income),
         pct_college = college_share*100,
         poor_n  = poor_share*pop)

# Before we move onto another nice thing we can do with mutate(), let's look at
# the histogram for income (base R way):
hist(dat$income)

# Now suppose we want to divide counties into three categories of income: low,
# middle, and high. Suppose we want to consider counties below 40k as low,
# between 40-60k as middle, and above 60k as high. We can do that using
# mutate() and case_when():
newdat <- dat %>%
  mutate(
    income_group = case_when(
      income < 40000 ~ "Low",
      income >= 40000 &
        income < 60000 ~ "Middle",
      income >= 60000 ~ "High"
    )
  )

# Recode ------------------------------------------------------------------

# Combining mutate() and recode() lets you change values in your dataset. For
# example, suppose we want to substitute "Alabama" with states acronym, "AL":
dat %>% mutate(state = recode(state, Alabama = "AL"))

# Another useful function is recode_factor. To use it, let's get a different
# dataset, the Opportunity Insights College data:
oic <- read_csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/college.csv")

# Bar plot before recode:
ggplot(data = oic, aes(x = type)) + geom_bar()

# Recode:
oic <- oic %>% mutate(type = recode_factor(type,
                                           `1` = "Public",
                                           `2` = "Private Non-profit",
                                           `3` = "For-profit")) 

# Bar plot after recode:
ggplot(data = oic, aes(x = type)) + geom_bar()

# Recode to NA ------------------------------------------------------------

# Recoding when missing appears as numeric values
# Read made up data
msdat <- read_csv("https://raw.githubusercontent.com/lfalab/poli5/main/data/missing.csv")

# Suppose that a survey with students collects data on how many states they
# visited, but some students did not provide a valid answer. Some refused to
# answer the question, others stated that they don't remember the number of
# states they visited. The researcher compiling this dataset may want to
# differentiate between "refused" and "don't know", and state in the codebook
# that -8 means "refused" and -9 means "don't remember":
msdat

# This happens in various datasets, and will require you to take additional
# steps before analyzing and plotting your data. If you were to conduct a
# hypothesis test, or calculate the mean number of states visited, these
# negative numbers would mess everything up. In these cases, we must recode
# these negative numbers to NA, to let R know that these are missing values.
msdat %>% mutate(states_visited = ifelse(states_visited == -8 |
                                           states_visited == -9,
                                         NA,
                                         states_visited))

# Note that in this case we could have achieved the same result in a simpler
# way:
msdat %>% mutate(states_visited = ifelse(states_visited < 0,
                                         NA,
                                         states_visited))

# So you can better understand what is going on, let's look into what ifelse()
# does. Starting with a made up vector with positive and negative numbers:
new_vec <- c(3, 4, -5, 7, -2, -3, 1, -1, 8)

# Taking a look:
new_vec

# We can use ifelse() to return "Positive" for every value in new_vec greater
# than zero, and "Negative" for every number in new_vec lower than zero:
ifelse(new_vec > 0, # Start with a logical statement
       "Positive", # Output if TRUE
       "Negative") # Output if FALSE

# Bringing it back to the original recode:
msdat %>% mutate(states_visited = ifelse(states_visited < 0, # Logical statement
                                         NA, # Output if TRUE
                                         states_visited)) # Output if FALSE

# Integrating cleaning and plotting ---------------------------------------

# Suppose we want a bar plot with the number of counties in each income
# group. One option is to use dplyr to create a new data object and then use
# it in ggplot. Creating the new data object first:
dat2 <- dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High"))
# Passing it onto ggplot:
ggplot(data = dat2, aes(x = income_group)) +
  geom_bar()

# That's an option. But because dplyr and ggplot are both part of the same
# "family" of packages (tidyverse), we can use the pipe operator and use dplyr
# and ggplot in one single line of code:
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High")) %>% 
  ggplot(aes(x = income_group)) +
  geom_bar()

# Let's fix a few things about this plot. First, we don't want that "NA" in
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High")) %>% 
  filter(!is.na(income_group)) %>% 
  ggplot(aes(x = income_group)) +
  geom_bar()

# To have the order go from "Low" to "High":
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High"),
         income_group = factor(income_group,
                                  levels = c("Low",
                                             "Middle",
                                             "High"))) %>% 
  filter(!is.na(income_group)) %>% 
  ggplot(aes(x = income_group)) +
  geom_bar()

# Adding a title and axis labels:
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High"),
         income_group = factor(income_group,
                                  levels = c("Low",
                                             "Middle",
                                             "High"))) %>% 
  filter(!is.na(income_group)) %>% 
  ggplot(aes(x = income_group)) +
  geom_bar() +
  labs(title = "Distribution of U.S. counties into income groups",
       y = "Frequency",
       x = "Income group",
       caption = "Low: Less than $40k
       Middle: Between $40-60k
       High: Greater than or equal to $60k")

# Facets ------------------------------------------------------------------

# Facets are a way to have ggplot slice the data into different mini-plots.

# https://ggplot2.tidyverse.org/reference/facet_grid.html
# https://ggplot2.tidyverse.org/reference/facet_wrap.html

# We will continue with our dplyr-ggplot integration in the next example:
# filtering the data to include only four states and comparing the distributions
# of U.S. counties into income groups:
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High"),
         income_group = factor(income_group,
                               levels = c("Low",
                                          "Middle",
                                          "High"))) %>% 
  filter(!is.na(income_group),
         !is.na(state)) %>% 
  ggplot(aes(x = income_group)) +
  geom_bar() +
  facet_wrap(~ state, 
             ncol = 7) +
  labs(title = "Distribution of U.S. counties into income groups",
       y = "Frequency",
       x = "Income group",
       caption = "Low: Less than $40k
       Middle: Between $40-60k
       High: Greater than or equal to $60k")

# With facet_wrap(), we make mini-plots according to one variable. ncol=7 means
# that we want to display the mini-plots in 7 columns.

# Because some states have more counties than others, states with fewer counties
# contains only a few slivers as their bars because Texas is forcing the y axis
# to go all the way up to 150. If we want to allow for the y axes of different
# states to be different, we include scales = "free" into facet_wrap().
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High"),
         income_group = factor(income_group,
                               levels = c("Low",
                                          "Middle",
                                          "High"))) %>% 
  filter(!is.na(income_group),
         !is.na(state)) %>% 
  ggplot(aes(x = income_group)) +
  geom_bar() +
  facet_wrap(~ state, 
             ncol = 7,
             scales = "free") +
  labs(title = "Distribution of U.S. counties into income groups",
       y = "Frequency",
       x = "Income group",
       caption = "Low: Less than $40k
       Middle: Between $40-60k
       High: Greater than or equal to $60k")

# facet_grid() allows us to create mini-plots according to two variables. Let's
# compare between math scores in counties by income group and state:
dat %>% 
  mutate(income_group = case_when(income < 40000 ~ "Low",
                                  income >= 40000 & 
                                    income < 60000 ~ "Middle",
                                  income >= 60000 ~ "High"),
         income_group = factor(income_group,
                               levels = c("Low",
                                          "Middle",
                                          "High"))) %>% 
  filter(state == "Illinois" |
           state == "New York" |
           state == "Michigan" |
           state == "Georgia",
         !is.na(income_group)) %>% 
  ggplot(aes(x = math_score)) +
  geom_histogram(binwidth = 0.25,
                 color = "black",
                 fill = "white") +
  facet_grid(state ~ income_group) +
  labs(title = "Distribution of 3rd grader math scores by county in four states",
       subtitle = "By income level",
       y = "Frequency",
       x = "Math scores",
       caption = "Low: Less than $40k
       Middle: Between $40-60k
       High: Greater than or equal to $60k")
