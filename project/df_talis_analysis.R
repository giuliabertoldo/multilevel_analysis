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
df <- read.csv('project/data/df.csv')

# Glimpse data
glimpse(df)

# Clean data --------------------------------------------------------------

# * Convert to factors ----------------------------------------------------
# Convert to factor: School ID
df$IDSCHOOL <- as.factor(df$IDSCHOOL)
nlevels(df$IDSCHOOL)
# Convert to factor: Teacher ID
df$IDTEACH <- as.factor(df$IDTEACH)
nlevels(df$IDTEACH)
# Convert to factor: Gender
df$TT3G01 <- as.factor(df$TT3G01)
nlevels(df$TT3G01)
# Convert to factor: Age group
df$TCHAGEGR <- as.factor(df$TCHAGEGR)
nlevels(df$TCHAGEGR)
# Convert to factor: School location
df$SCHLOC <- as.factor(df$SCHLOC)
nlevels(df$SCHLOC)
# Convert to factor: Lack of resources
df$T3PLACRE <- as.factor(df$T3PLACRE)
nlevels(df$T3PLACRE)
# Convert to factor: Lack of pedagogical personnel
df$T3PLACPE <- as.factor(df$T3PLACPE)
nlevels(df$T3PLACPE)
# Convert to factor: Lack of material resources
df$T3PLACMA <- as.factor(df$T3PLACMA)
nlevels(df$T3PLACMA)
# Glimpse data
glimpse(df)



# * Center predictors (Grand mean centering) ------------------------------

# Mean-center Workplace wellbeing and stress
df$T3WELS <- df$T3WELS - mean(df$T3WELS, na.rm =TRUE)
summary(df$T3WELS )
# Mean- Center Workload stress
df$T3WLOAD <- df$T3WLOAD - mean(df$T3WLOAD, na.rm =TRUE)
summary(df$T3WLOAD )
# Mean-center Student behavior stress
df$T3STBEH <- df$T3STBEH - mean(df$T3STBEH, na.rm =TRUE)
summary(df$T3STBEH)
# Mean-center Teacher self-efficacy, composite
df$T3SELF <- df$T3SELF - mean(df$T3SELF, na.rm =TRUE)
summary(df$T3SELF)
# Mean-center Self-efficacy in classroom management
df$T3SECLS <- df$T3SECLS - mean(df$T3SECLS, na.rm =TRUE)
summary(df$T3SECLS)
# Mean-center Self-efficacy in instruction
df$T3SEINS <- df$T3SEINS - mean(df$T3SEINS, na.rm =TRUE)
summary(df$T3SEINS)
# Mean-center Self-efficacy in student engagement
df$T3SEENG <- df$T3SEENG - mean(df$T3SEENG, na.rm =TRUE)
summary(df$T3SEENG)
# Mean-center Self-efficacy in student engagement
df$T3SEENG <- df$T3SEENG - mean(df$T3SEENG, na.rm =TRUE)
summary(df$T3SEENG)
# Mean-center Personal utility motivation to teach
df$T3PERUT <- df$T3PERUT - mean(df$T3PERUT, na.rm =TRUE)
summary(df$T3PERUT)
# Mean-center Social utility motivation to teach
df$T3SOCUT <- df$T3SOCUT - mean(df$T3SOCUT, na.rm =TRUE)
summary(df$T3SOCUT)
# Mean-center Perceptions of value and policy influence
df$T3VALP <- df$T3VALP - mean(df$T3VALP, na.rm =TRUE)
summary(df$T3VALP)
# Mean-center Teacher cooperation composite
df$T3COOP <- df$T3COOP - mean(df$T3COOP, na.rm =TRUE)
summary(df$T3COOP)
# Mean-center Exchange and coordination among teachers
df$T3EXCH <- df$T3EXCH - mean(df$T3EXCH, na.rm =TRUE)
summary(df$T3EXCH)
# Mean-center Professional collaboration in lessons among teachers
df$T3COLES <- df$T3COLES - mean(df$T3COLES, na.rm =TRUE)
summary(df$T3COLES)
# Mean-center Effective professional development
df$T3EFFPD <- df$T3EFFPD - mean(df$T3EFFPD, na.rm =TRUE)
summary(df$T3EFFPD)
# Mean-center Needs for professional development
df$T3PDPED <- df$T3PDPED - mean(df$T3PDPED, na.rm =TRUE)
summary(df$T3PDPED)
# Mean-center Needs for professional development for teaching for diversity
df$T3PDIV <- df$T3PDIV - mean(df$T3PDIV, na.rm =TRUE)
summary(df$T3PDIV)
# Mean-center Professional development barriers
df$T3PDBAR <- df$T3PDBAR - mean(df$T3PDBAR, na.rm =TRUE)
summary(df$T3PDBAR)

# Mean-center Academic pressure
df$T3PACAD <- df$T3PACAD - mean(df$T3PACAD, na.rm =TRUE)
summary(df$T3PACAD)
# Mean-center Stakeholder involvement, partnership
df$T3PCOM <- df$T3PCOM - mean(df$T3PCOM, na.rm =TRUE)
summary(df$T3PCOM)
# Mean-center Lack of special needs personnel
df$T3PLACSN <- df$T3PLACSN - mean(df$T3PLACSN, na.rm =TRUE)
summary(df$T3PLACSN)
# Mean-center School delinquency and violence
df$T3PDELI <- df$T3PDELI - mean(df$T3PDELI, na.rm =TRUE)
summary(df$T3PDELI)
# Mean-center School leadership
df$T3PLEADS <- df$T3PLEADS - mean(df$T3PLEADS, na.rm =TRUE)
summary(df$T3PLEADS)
# Mean-center Participation among stakeholders
df$T3PLEADP <- df$T3PLEADP - mean(df$T3PLEADP, na.rm =TRUE)
summary(df$T3PLEADP)
# Mean-center Equity and diversity - diversity beliefs
df$T3PDIVB <- df$T3PDIVB - mean(df$T3PDIVB, na.rm =TRUE)
summary(df$T3PDIVB)

# Glimpse data
glimpse(df)


# * Center predictors (Group mean centering)  -----------------------------


# Exploratory data analysis  ----------------------------------------------
# * Univariate analysis ---------------------------------------------------
# Teachers' gender
ggplot(df, aes(x=TT3G01)) +
  geom_bar() +
  labs(x = 'Gender')
summary(df$TT3G01)

# Teachers' age group
ggplot(df, aes(x=TCHAGEGR)) +
  geom_bar() +
  labs(x = 'Age group')

# Job Satisfaction, composite
ggplot(df, aes(x=T3JOBSA)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Job satisfaction, composite')
summary(df$T3JOBSA)

# Workload stress
ggplot(df, aes(x=T3WLOAD)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Workload stress')
summary(df$T3WLOAD)

# Create a dataframe where one row = one school
school_subset <- df %>%
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
ggplot(df, aes(x=IDSCHOOL, y=T3JOBSA)) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red") +
  coord_flip()

# Relationship between Workload Stress and Job Satisfaction
ggplot(data  = df,
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
ggplot(data  = df,
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

# Relationship between Workload Stress and Job Satisfaction
df %>%
  ggplot(aes(x = T3WLOAD, y = T3JOBSA, col = IDSCHOOL)) +
  geom_point(alpha = 0.5, size = 0.5) +
  guides(col = "none") +
  labs(title = "Job Satisfaction vs. Workload Stress",
       subtitle = "Colored by school",
       x = "Workload Stress",
       y = "Job Satisfaction")

# Relationship between lack of resources and Job Satisfaction
ggplot(data  = df,
       aes(x = T3PLACRE,
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
  labs(title = "Job Satisfaction vs. Lack of resources",
       subtitle = "Colored by school",
       x = "Lack of resources",
       y = "Job Satisfaction")

# Show variation across a subset of ten schools ---------------------------
random_id <- sample(unique(df$IDSCHOOL), size = 10)
(p_subset <- df %>%
    filter(IDSCHOOL %in% random_id) %>%  # select only 10 schools
    ggplot(aes(x = IDSCHOOL, y = T3JOBSA )) +
    geom_jitter(height = 0, width = 0.1, alpha = 0.3) +
    # Add school means
    stat_summary(
      fun = "mean",
      geom = "point",
      col = "red",
      shape = 17,
      # use triangles
      size = 4
    )  # make them larger
)


# Unconditional random intercept model ------------------------------------
model1 <- lmer(T3JOBSA ~ 1 + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model1)
# Profile likelihood confidence intervals
confint(model1)


# ICC ---------------------------------------------------------------------
variance_components <- as.data.frame(VarCorr(model1))
(between_var <- variance_components$vcov[1])
(within_var <- variance_components$vcov[2])
(icc <- between_var / (between_var + within_var))


# Empirical Bayes estimates ------------------------------------------------
dotplot(ranef(model1, condVar = TRUE))


# Design effect -----------------------------------------------------------
# Calculate by hand
cluster_size <- df %>%
  group_by(IDSCHOOL) %>%
  summarise(count = n())
(average_cluster_size <- mean(cluster_size$count))
(design_effect <- 1 + (average_cluster_size - 1) * icc)
# Effective sample size
(n_eff <- length(df$IDTEACH)/design_effect)


# Model with a level-2 variable: School delinquency -----------------------
model2 <- lmer(T3JOBSA ~ T3PDELI + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model2)
# Likelihood-based confidence intervals for fixed effects
confint(model2)

# Plot
sjPlot::plot_model(model2,
                   type = "pred",
                   terms = "T3PDELI",
                   show.data = TRUE,
                   title = "",
                   dot.size = 0.5) +
  stat_summary(data = df, aes(x = T3PDELI, y = T3JOBSA),
               fun = mean, geom = "point",
               col = "red",
               shape = 17,
               # use triangles
               size = 3,
               alpha = 0.7)


# Proportion of variance explained  ---------------------------------------
# Use Rights & Sterba (2019)
r2mlm::r2mlm(model2)





