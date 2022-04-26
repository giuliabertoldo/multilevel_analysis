# Load packages -----------------------------------------------------------
library(haven)
library(tidyverse)
library(GGally)
library(ggplot2)
library(DataExplorer)
theme_set(theme_bw())

# Load data ---------------------------------------------------------------
# Teacher questionnaire data
teach_quest <- read_sav('project/data/BTGITAT3.sav')
# School questionnaire data
school_quest <- read_sav('project/data/BCGITAT3.sav')

# Glimpse data
glimpse(teach_quest)
glimpse(school_quest)


# Create merged dataset with variables of interest ------------------------

# * Basic data set exploration ---------------------------------------------
# Number of schools
length(unique(teach_quest$IDSCHOOL))
length(unique(school_quest$IDSCHOOL))
# Number of teachers
length(unique(teach_quest$IDTEACH))

# * Outcome measure: Job satisfaction -------------------------------------
# Composite: Job satisfaction
summary(teach_quest$T3JOBSA)
# Subscale: Job satisfaction with work environment
summary(teach_quest$T3JSENV)
# Subscale: Job satisfaction with profession
summary(teach_quest$T3JSPRO)
# Subscale: Job satisfaction with target class autonomy
summary(teach_quest$T3SATAT)

# * Level 1 variables -----------------------------------------------------
# Teacher's gender
unique(teach_quest$TT3G01)

# Teacher's age group
unique(teach_quest$TCHAGEGR)

# Workplace well-being and stress
summary(teach_quest$T3WELS)
# Workload stress
summary(teach_quest$T3WLOAD)
# Student behavior stress
summary(teach_quest$T3STBEH)

# Composite: Teacher self-efficacy
summary(teach_quest$T3SELF)
# Subscale: Self-efficacy in classroom management
summary(teach_quest$T3SECLS)
# Subscale: Self-efficacy in instruction
summary(teach_quest$T3SEINS)
# Subscale: Self-efficacy in student egagement
summary(teach_quest$T3SEENG)

# * Level 2 variables -----------------------------------------------------
# School location
unique(school_quest$SCHLOC)

# School climate: Academic pressure
summary(school_quest$T3PACAD)
# School climate: Stakeholder involvement, partnership
summary(school_quest$T3PCOM)
# School climate: Lack of special needs personnel
summary(school_quest$T3PLACSN)
# School climate: School delinquency and violence
summary(school_quest$T3PDELI)

# Lack of pedagogical personnel
unique(school_quest$T3PLACPE)
# Lack of resources
unique(school_quest$T3PLACRE)
# Lack of material resources
unique(school_quest$T3PLACMA)

# * Subset teacher dataset ------------------------------------------------
teachers <- teach_quest %>%
  select(IDSCHOOL,
         IDTEACH,
         T3JOBSA,
         T3JSENV,
         T3JSPRO,
         T3SATAT,
         TT3G01,
         TCHAGEGR,
         T3WELS,
         T3WLOAD,
         T3STBEH,
         T3SELF,
         T3SECLS,
         T3SEINS,
         T3SEENG,
         T3PERUT,
         T3SOCUT,
         T3VALP,
         T3COOP,
         T3EXCH,
         T3COLES,
         T3EFFPD,
         T3PDPED,
         T3PDIV,
         T3PDBAR)

# * Subset school dataset -------------------------------------------------
schools <- school_quest %>%
  select(IDSCHOOL,
         SCHLOC,
         T3PACAD,
         T3PCOM,
         T3PLACSN,
         T3PDELI,
         T3PLACPE,
         T3PLACRE,
         T3PLACMA,
         T3PLEADS,
         T3PLEADP,
         T3PDIVB)


# * Merge teacher and school dataset --------------------------------------
df <- inner_join(teachers, schools, by = 'IDSCHOOL')
# Save dataframe as csv
write.csv(df, "df.csv", row.names = FALSE)

# Data Cleaning -----------------------------------------------------------

# * Convert characters to factors -----------------------------------------
# Glimpse data
glimpse(df)
# School ID
df$IDSCHOOL <- as.factor(df$IDSCHOOL)
nlevels(df$IDSCHOOL)
# Teacher ID
df$IDTEACH <- as.factor(df$IDTEACH)
nlevels(df$IDTEACH)
# Gender
df$TT3G01 <- as.factor(df$TT3G01)
levels(df$TT3G01)
# Age group
df$TCHAGEGR <- as.factor(df$TCHAGEGR)
levels(df$TCHAGEGR)
# School location
df$SCHLOC <- as.factor(df$SCHLOC)
levels(df$SCHLOC)
# Lack of pedagogical personnel
df$T3PLACPE <- as.factor(df$T3PLACPE )
levels(df$T3PLACPE)
# Lack of resources
df$T3PLACRE <- as.factor(df$T3PLACRE )
levels(df$T3PLACRE)
# Lack of material resources
df$T3PLACMA <- as.factor(df$T3PLACMA )
levels(df$T3PLACMA)

# Exploratory data analysis  ----------------------------------------------
# * Univariate analysis ---------------------------------------------------

# Teachers' gender
ggplot(df, aes(x=TT3G01)) +
  geom_bar() +
  labs(x = 'Gender')
summary(df$TT3G01)
sum(is.na(df$TT3G01))
sum(!is.na(df$TT3G01))

# Teachers' age
ggplot(df, aes(x=TCHAGEGR)) +
  geom_bar() +
  labs(x = 'Age group')
sum(is.na(df$TCHAGEGR))
sum(!is.na(df$TCHAGEGR))

# Job Satisfaction, composite
ggplot(df, aes(x=T3JOBSA)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Job satisfaction, composite')
summary(df$T3JOBSA)
sum(is.na(df$T3JOBSA))
sum(!is.na(df$T3JOBSA))

# Workplace well-being and stress
ggplot(df, aes(x=T3WELS)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Workplace well-being and stress')
summary(df$T3WELS)
sum(is.na(df$T3WELS))
sum(!is.na(df$T3WELS))

# Workload stress
ggplot(df, aes(x=T3WLOAD)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Workload stress')
summary(df$T3WLOAD)
sum(is.na(df$T3WLOAD))
sum(!is.na(df$T3WLOAD))

# Student behavior stress
ggplot(df, aes(x=T3STBEH)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Student behavior stress')
summary(df$T3STBEH)
sum(is.na(df$T3STBEH))
sum(!is.na(df$T3STBEH))


# Teacher self-efficacy, composite
ggplot(df, aes(x=T3SELF)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Teacher self-efficacy, composite')
summary(df$T3SELF)
sum(is.na(df$T3SELF))
sum(!is.na(df$T3SELF))

# School location
# Create a dataframe where one row = one school
school_subset <- df %>%
  group_by(IDSCHOOL) %>%
  filter(row_number()==1)

ggplot(school_subset, aes(x=SCHLOC)) +
  geom_bar() +
  labs(x = "School location")

sum(is.na(school_subset$SCHLOC))
sum(!is.na(school_subset$SCHLOC))

# School climate: Academic pressure
ggplot(school_subset, aes(x=T3PACAD)) +
  geom_histogram() +
  labs(x = "Academic pressure ")
summary(school_subset$T3PACAD)
sum(is.na(school_subset$T3PACAD))
sum(!is.na(school_subset$T3PACAD))

# School climate: Stakeholder involvement, partnership
ggplot(school_subset, aes(x=T3PCOM)) +
  geom_histogram() +
  labs(x = "Stakeholder involvement, partnership")
summary(school_subset$T3PCOM)
sum(is.na(school_subset$T3PCOM))
sum(!is.na(school_subset$T3PCOM))

# School climate: Lack of special needs personnel
ggplot(school_subset, aes(x=T3PLACSN)) +
  geom_histogram() +
  labs(x = "Lack of special needs personnel")
summary(school_subset$T3PLACSN)
sum(is.na(school_subset$T3PLACSN))
sum(!is.na(school_subset$T3PLACSN))

# School climate: School delinquency and violence
ggplot(school_subset, aes(x=T3PDELI)) +
  geom_histogram() +
  labs(x = "School delinquency and violence")
summary(school_subset$T3PDELI)
sum(is.na(school_subset$T3PDELI))
sum(!is.na(school_subset$T3PDELI))

# School resources: Lack of pedagogical personnel
ggplot(school_subset, aes(x=T3PLACPE)) +
  geom_bar() +
  labs(x = "Lack of pedagogical personnel")
summary(school_subset$T3PLACPE)
sum(is.na(school_subset$T3PLACPE))
sum(!is.na(school_subset$T3PLACPE))

# School resources: Lack of resources
ggplot(school_subset, aes(x=T3PLACRE)) +
  geom_bar() +
  labs(x = "Lack of resources")
summary(school_subset$T3PLACRE)
sum(is.na(school_subset$T3PLACRE))
sum(!is.na(school_subset$T3PLACRE))

# School resources: Lack of material resources
ggplot(school_subset, aes(x=T3PLACMA)) +
  geom_bar() +
  labs(x = "Lack of materialresources")
summary(school_subset$T3PLACMA)
sum(is.na(school_subset$T3PLACMA))
sum(!is.na(school_subset$T3PLACMA))


# * Bivariate analysis ----------------------------------------------------

# Basic stripchart
ggplot(df, aes(x=IDSCHOOL, y=T3JOBSA)) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red") +
  stat_summary(fun.data=mean_sdl, mult=1,
               geom="pointrange", color="red")



# Create final dataset ----------------------------------------------------
# Option 1
# The dataset includes the variables listed below
# also with missing observations
df1 <- df %>%
  select(IDSCHOOL,
         IDTEACH,
         TT3G01,
         TCHAGEGR,
         T3JOBSA,
         T3WLOAD,
         SCHLOC,
         T3PLACRE)
summary(df1)
# How many teachers?
length(unique(df1$IDTEACH))
# How many schools?
length(unique(df1$IDSCHOOL))
# Save dataframe as csv
write.csv(df1, "df1.csv", row.names = FALSE)

# Option 2
# The dataset includes the variables listed below
# with non missing observations only

df2 <- df1 %>%
  drop_na(IDSCHOOL,
          IDTEACH,
          TT3G01,
          TCHAGEGR,
          T3JOBSA,
          T3WLOAD,
          SCHLOC,
          T3PLACRE)
summary(df2)
# How many teachers?
length(unique(df2$IDTEACH))
# How many schools?
length(unique(df2$IDSCHOOL))
# Save dataframe as csv
write.csv(df2, "df2.csv", row.names = FALSE)

