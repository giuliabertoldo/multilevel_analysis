
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
airbnb <- read.csv("project/old_ideas/airbnb/airbnb.csv")


# Clean data --------------------------------------------------------------
# Glimpse data
glimpse(airbnb)
# Convert characters to factors
# Room Type
airbnb$room_type <- as.factor(airbnb$room_type)
levels(airbnb$room_type)
nlevels
# Neighborhood
airbnb$neighborhood <- as.factor(airbnb$neighborhood)
levels(airbnb$neighborhood)
nlevels(airbnb$neighborhood)
# District
airbnb$district <- as.factor(airbnb$district)
levels(airbnb$district)
nlevels(airbnb$district)
# Glimpse data
glimpse(airbnb)



# Exploratory data analysis  ----------------------------------------------
# How many accommodations?
length(unique(airbnb$X))
# How many neighborhoods?
length(unique(airbnb$neighborhood))

# Overview
summary(airbnb)

# DataExplorer
airbnb %>%
  create_report(
    output_file = paste("Report", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), sep=" - "),
    report_title = "EDA Report - Airbnb data",
    y = "X"
  )



# * Bivariate analysis ----------------------------------------------------

# Basic stripchart
ggplot(airbnb2, aes(x=IDSCHOOL, y=T3JOBSA)) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red") +
  coord_flip()

# Relationship between Workload Stress and Job Satisfaction
ggplot(data  = airbnb,
       aes(x = accommodates,
           y = price,
           col = neighborhood))+
  geom_point(size = 1.2,
             alpha = .8)+
  geom_smooth(method = lm,
              se     = FALSE,
              col = "black",
              size   = .5,
              alpha  = .8)+
  theme(legend.position = "none")+
  labs(title = "Price vs. Number of guests",
       subtitle = "Colored by neighborhood",
       x = "Number of guests",
       y = "Price")


# Relationship between Workload Stress and Job Satisfaction
ggplot(data  = airbnb,
       aes(x = accommodates,
           y = price,
           col = neighborhood,
           group = neighborhood))+
  geom_point(size = 1.2,
             alpha = .8)+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5,
              alpha  = .8)+
  theme(legend.position = "none")+
  labs(title = "Price vs. Number of guests",
       subtitle = "Colored by neighborhood",
       x = "Number of guests",
       y = "Price")

# Model -------------------------------------------------------------------

ran_int1 <- lmer(price ~ 1 + (1 | neighborhood), data = airbnb)
summary(ran_int1)

(round(icc <- 1099/(1099+6776),2))


