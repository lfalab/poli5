# Read data ---------------------------------------------------------------

# Opportunity Insights - County Covariates data
dat <- read.csv("https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv")
# Population in 2010 (Census)
pop <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010/2010-eval-estimates/co-est2010-totals.csv")

# Select variables from Census data ---------------------------------------

library(tidyverse)
pop <- pop %>% select(STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE072010)
# Rename variables from census data
names(pop) <- c("state", "county", "stname", "ctyname", "pop_2010")

# Add variables -----------------------------------------------------------

dat <- dat %>% left_join(pop, by = c("state", "county")) %>% as_tibble()

# Remove lingering objects ------------------------------------------------

rm(pop)
