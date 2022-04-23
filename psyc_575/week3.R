# Load packages -----------------------------------------------------------
library(here)  # makes reading data more consistent
library(tidyverse)  # for data manipulation and plotting
library(haven)  # for importing SPSS/SAS/Stata data
library(lme4)  # for multilevel analysis
library(lattice)  # for dotplot (working with lme4)
library(sjPlot)  # for plotting effects
library(MuMIn)  # for computing r-squared
library(r2mlm)  # for computing r-squared
library(broom.mixed)  # for summarizing results
library(modelsummary)  # for making tables
theme_set(theme_bw())  # Theme; just my personal preference

# Import data -------------------------------------------------------------
hsball <- read_sav("psyc_575/hsball.sav")
# Print data
hsball

# Random intercept model --------------------------------------------------
ran_int <- lmer(mathach ~ 1 + (1 | id), data = hsball)
# Summarize results
summary(ran_int)
## Fixed effect:
### gamma00: 12.637
## Variance components of the random effects:
### 8.614
### 39.148

