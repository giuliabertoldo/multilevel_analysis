
# Load packages -----------------------------------------------------------
library(tidyverse)
library(lme4)
library(lattice)
library(sjPlot)
library(MuMIn)
library(r2mlm)
library(broom.mixed)
library(modelsummary)
library(DataExplorer)
theme_set(theme_bw())







# Load data ---------------------------------------------------------------
df <- read.csv("project/data/airbnb.csv")


# Clean data --------------------------------------------------------------
# Glimpse data
glimpse(df)
# Convert characters to factors
# Room Type
df$room_type <- as.factor(df$room_type)
levels(df$room_type)
nlevels
# Neighborhood
df$neighborhood <- as.factor(df$neighborhood)
levels(df$neighborhood)
nlevels(df$neighborhood)
# District
df$district <- as.factor(df$district)
levels(df$district)
nlevels(df$district)
# Glimpse data
glimpse(df)



# Exploratory data analysis  ----------------------------------------------
# How many accommodations?
length(unique(df$X))
# How many neighborhoods?
length(unique(df$neighborhood))

# Overview
summary(df)

# DataExplorer
df %>%
  create_report(
    output_file = paste("Report", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep=" - "),
    report_title = "EDA Report - Airbnb data",
    y = "X"
  )

