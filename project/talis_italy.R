# Load packages -----------------------------------------------------------
library(haven)

# Load data ---------------------------------------------------------------
df1 <- read_sav('project/data/BTGITAT3.sav')
str(df1)

# Clean data --------------------------------------------------------------
# Glimpse data
glimpse(df1)


# Explore data ------------------------------------------------------------

# Number of schools
length(unique(df1$IDSCHOOL))
# Number of teeachers
length(unique(df1$IDTEACH))
