# Load packages -----------------------------------------------------------
library(tidyverse)
library(GGally)
library(ggplot2)
library(DataExplorer)
theme_set(theme_bw())

# Load data ---------------------------------------------------------------
df2 <- read.csv('project/data/df2.csv')

# Glimpse data
glimpse(df2)

# Clean data --------------------------------------------------------------

# * Convert to factors ----------------------------------------------------
# Convert to factor: School ID
df2$IDSCHOOL <- as.factor(df2$IDSCHOOL)
nlevels(df2$IDSCHOOL)
# Convert to factor: Teacher ID
df2$IDTEACH <- as.factor(df2$IDTEACH)
nlevels(df2$IDTEACH)
# Convert to factor: Gender
df2$TT3G01 <- as.factor(df2$TT3G01)
nlevels(df2$TT3G01)
# Convert to factor: Age group
df2$TCHAGEGR <- as.factor(df2$TCHAGEGR)
nlevels(df2$TCHAGEGR)
# Convert to factor: School location
df2$SCHLOC <- as.factor(df2$SCHLOC)
nlevels(df2$SCHLOC)
# Convert to factor: Lack of resources
df2$T3PLACRE <- as.factor(df2$T3PLACRE)
nlevels(df2$T3PLACRE)
# Glimpse data
glimpse(df2)


# * Center predictors -----------------------------------------------------
# Mean- Center Workload stress
df2$T3WLOAD <- scale(df2$T3WLOAD, scale=FALSE)
summary(df2$T3WLOAD )

# Exploratory data analysis  ----------------------------------------------
# * Univariate analysis ---------------------------------------------------
# Teachers' gender
ggplot(df2, aes(x=TT3G01)) +
  geom_bar() +
  labs(x = 'Gender')
summary(df2$TT3G01)

# Teachers' age group
ggplot(df2, aes(x=TCHAGEGR)) +
  geom_bar() +
  labs(x = 'Age group')

# Job Satisfaction, composite
ggplot(df2, aes(x=T3JOBSA)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Job satisfaction, composite')
summary(df2$T3JOBSA)

# Workload stress
ggplot(df2, aes(x=T3WLOAD)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Workload stress')
summary(df2$T3WLOAD)

# Create a dataframe where one row = one school
school_subset <- df2 %>%
  group_by(IDSCHOOL) %>%
  filter(row_number()==1)

# School location
ggplot(school_subset, aes(x=SCHLOC)) +
  geom_bar() +
  labs(x = "School location")

# Lack of resources
ggplot(school_subset, aes(x=T3PLACRE)) +
  geom_bar() +
  labs(x = "Lack of resources")


# * Bivariate analysis ----------------------------------------------------

# Basic stripchart
ggplot(df2, aes(x=IDSCHOOL, y=T3JOBSA)) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red") +
  coord_flip()

# Relationship between Workload Stress and Job Satisfaction
ggplot(data  = df2,
       aes(x = T3WLOAD,
           y = T3JOBSA,
           col = IDSCHOOL))+
  geom_point(size = 1.2,
             alpha = .8)+
  geom_smooth(method = lm,
              se     = FALSE,
              col = "black",
              size   = .5,
              alpha  = .8)+
  theme(legend.position = "none")+
  labs(title = "Job Satisfaction vs. Workload Stress",
       subtitle = "Colored by school",
       x = "Workload Stress",
       y = "Job Satisfaction")


# Relationship between Workload Stress and Job Satisfaction
ggplot(data  = df2,
       aes(x = T3WLOAD,
           y = T3JOBSA,
           col = IDSCHOOL,
           group = IDSCHOOL))+
  geom_point(size = 1.2,
             alpha = .8)+
  geom_smooth(method = lm,
             se     = FALSE,
             size   = .5,
             alpha  = .8)+
  theme(legend.position = "none")+
  labs(title = "Job Satisfaction vs. Workload Stress",
       subtitle = "Colored by school",
       x = "Workload Stress",
       y = "Job Satisfaction")

