# Load packages -----------------------------------------------------------
library(tidyverse)
library(GGally)
library(ggplot2)
library(DataExplorer)
library(lme4)
library(lattice)
library(sjPlot)
library(r2mlm)
theme_set(theme_bw())

# Load data ---------------------------------------------------------------
df4 <- read.csv('project/data/df4.csv')

# Glimpse data
glimpse(df4)

# Clean data --------------------------------------------------------------

# * Convert to factors ----------------------------------------------------
# Convert to factor: School ID
df4$IDSCHOOL <- as.factor(df4$IDSCHOOL)
nlevels(df4$IDSCHOOL)
# Convert to factor: Teacher ID
df4$IDTEACH <- as.factor(df4$IDTEACH)
nlevels(df4$IDTEACH)
# Convert to factor: Gender
df4$TT3G01 <- as.factor(df4$TT3G01)
nlevels(df4$TT3G01)
# Convert to factor: Age group
df4$TCHAGEGR <- as.factor(df4$TCHAGEGR)
nlevels(df4$TCHAGEGR)
# Convert to factor: School location
df4$SCHLOC <- as.factor(df4$SCHLOC)
nlevels(df4$SCHLOC)
# Glimpse data
glimpse(df4)


# Center predictor --------------------------------------------------------
df4 <- df4 %>%
  # Grand Mean Centered variable (GMC)
  mutate(T3WELS_GMC = T3WELS - mean(T3WELS)) %>%
  # Cluster Mean Centered variable (CMC) - Cluster Mean (CM)
  group_by(IDSCHOOL) %>%
  mutate(T3WELS_CM = mean(T3WELS),
         T3WELS_CMC = T3WELS - T3WELS_CM) %>%
  ungroup %>%
  # Grand mean centering of the aggregated variable
  mutate(T3WELS_AGG_GMC = T3WELS_CM - mean(T3WELS_CM))

# Comparing the results of the centering approaches
df4 %>%
  select(T3WELS, T3WELS_GMC, T3WELS_CM,
         T3WELS_CMC, T3WELS_AGG_GMC) %>%
  summary

