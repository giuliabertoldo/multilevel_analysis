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


# Center predictor --------------------------------------------------------

# T3WLOAD
# Cluster-mean centering
# To separate the effects of a lv-1 predictor into different levels,
# one needs to first center the predictor on the cluster means:
df <- df %>%
  group_by(IDSCHOOL) %>%   # operate within schools
  mutate(stress_cm = mean(T3WLOAD, na.rm = TRUE),   # create cluster means
         stress_cmc = T3WLOAD - stress_cm) %>%   # cluster-mean centered
  ungroup()

# T3WELS
# Cluster-mean centering
# To separate the effects of a lv-1 predictor into different levels,
# one needs to first center the predictor on the cluster means:
df <- df %>%
  group_by(IDSCHOOL) %>%   # operate within schools
  mutate(stress2_cm = mean(T3WELS, na.rm = TRUE),   # create cluster means
         stress2_cmc = T3WELS - stress2_cm) %>%   # cluster-mean centered
  ungroup()

# T3SLEF
# Cluster-mean centering
# To separate the effects of a lv-1 predictor into different levels,
# one needs to first center the predictor on the cluster means:
df <- df %>%
  group_by(IDSCHOOL) %>%   # operate within schools
  mutate(tself_cm = mean(T3SELF, na.rm = TRUE),   # create cluster means
         tself_cmc = T3SELF - tself_cm) %>%   # cluster-mean centered
  ungroup()

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


#  Model with a level-2 variable: Mean workload stress --------------------
model2 <- lmer(T3JOBSA ~ stress_cm + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model2)
# Likelihood-based confidence intervals for fixed effects
confint(model2)

# Plot
sjPlot::plot_model(model2,
                   type = "pred",
                   terms = "stress_cm",
                   show.data = TRUE,
                   title = "",
                   dot.size = 0.5) +
  stat_summary(data = df, aes(x = stress_cm, y = T3JOBSA),
               fun = mean, geom = "point",
               col = "red",
               shape = 17,
               # use triangles
               size = 3,
               alpha = 0.7)


# Proportion of variance explained  ---------------------------------------
# Use Rights & Sterba (2019)
r2mlm::r2mlm(model2)



# Between-Within: Model with lvl-1 predictor: Workload stress  ------------
model3 <- lmer(T3JOBSA ~ stress_cm + stress_cmc + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model3)
# Likelihood-based confidence intervals for fixed effects
confint(model3)

# Contextual effect: Model with lvl-1 predictor: Workload stress ----------
model4 <- lmer(T3JOBSA ~ stress_cm + T3WLOAD + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model4)
# Likelihood-based confidence intervals for fixed effects
confint(model4)


# Random coefficients model -----------------------------------------------
# Graphically explore slopes of a sample
set.seed(1)
df %>%
  # randomly sample 16 schools
  filter(IDSCHOOL %in% sample(unique(IDSCHOOL), 16)) %>%
  ggplot(aes(x = stress_cmc, y = T3JOBSA)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~IDSCHOOL)

# Model 5
model5 <- lmer(T3JOBSA ~ stress_cm + stress_cmc + (stress_cmc | IDSCHOOL), data = df)
# Summarize results
summary(model5)
# Likelihood-based confidence intervals for fixed effects
confint(model5)

# Plotting random slopes
augment(model5, data = df) %>% # augmented data (adding EB estimates)
  ggplot(aes(x = T3WLOAD, y = .fitted, color = factor(IDSCHOOL))) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction") +
  guides(color = "none")

# Create a common base graph
pbase <- augment(model5, data = df) %>%
  ggplot(aes(x = T3WLOAD, y = T3JOBSA, color = factor(IDSCHOOL))) +
  # Add points
  geom_point(size = 0.2, alpha = 0.2) +
  labs(y = "Job Satisfaction") +
  # Suppress legend
  guides(color = "none")
# Lv-1 effect
p1 <- pbase +
  # Add within-cluster lines
  geom_smooth(aes(y = .fitted),
              method = "lm", se = FALSE, size = 0.5)
# Lv-2 effect
p2 <- pbase +
  # Add group means
  stat_summary(aes(x = stress_cm, y = .fitted),
               fun = mean,
               geom = "point",
               shape = 17,
               # use triangles
               size = 2.5) +
  # Add between coefficient
  geom_smooth(aes(x = stress_cm, y = .fitted),
              method = "lm", se = FALSE,
              color = "black")
# Put the two graphs together (need the gridExtra package)
gridExtra::grid.arrange(p1, p2, ncol = 2)


# Effect size -------------------------------------------------------------

(r2_model5 <- r.squaredGLMM(model5))

# Cross-level interaction -------------------------------------------------
# Model 6
model6 <- lmer(T3JOBSA ~ stress_cm + SCHLOC * stress_cmc + (stress_cmc | IDSCHOOL), data = df)
# Summarize results
summary(model6)
# Likelihood-based confidence intervals for fixed effects
confint(model6)

# Effect size
(r2_model6 <- r.squaredGLMM(model6))
# Change
r2_model6 - r2_model5

# Plot the interaction
model6 %>%
  augment(data = df) %>%
  ggplot(aes(
    x = T3WLOAD, y = .fitted, group = factor(IDSCHOOL),
    color = factor(SCHLOC)
  )) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction", color = "SCHLOC")

# Interaction just the fixed effects
interact_plot(model6,
              pred = "stress_cm",
              modx = "SCHLOC",
              modx.labels = c("Rural", "Town", "City"),
              plot.points = TRUE,
              point.size = 0.5,
              point.alpha = 0.2,
              facet.modx = TRUE,  # use this to make two panels
              x.label = "Workload Stress (cluster-mean centered)",
              y.label = "Job Satisfaction")


# T3WELS ------------------------------------------------------------------

# Model with a level-2 variable: Mean workload stress --------------------
  model22 <- lmer(T3JOBSA ~ stress2_cm + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model22)
# Likelihood-based confidence intervals for fixed effects
confint(model22)

# Plot
sjPlot::plot_model(model22,
                   type = "pred",
                   terms = "stress2_cm",
                   show.data = TRUE,
                   title = "",
                   dot.size = 0.5) +
  stat_summary(data = df, aes(x = stress2_cm, y = T3JOBSA),
               fun = mean, geom = "point",
               col = "red",
               shape = 17,
               # use triangles
               size = 3,
               alpha = 0.7)


# Proportion of variance explained  ---------------------------------------
# Use Rights & Sterba (2019)
r2mlm::r2mlm(model22)



# Between-Within: Model with lvl-1 predictor: Workload stress  ------------
model32 <- lmer(T3JOBSA ~ stress2_cm + stress2_cmc + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model32)
# Likelihood-based confidence intervals for fixed effects
confint(model32)

# Contextual effect: Model with lvl-1 predictor: Workload stress ----------
model42 <- lmer(T3JOBSA ~ stress2_cm + T3WELS + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model42)
# Likelihood-based confidence intervals for fixed effects
confint(model42)


# Random coefficients model -----------------------------------------------
# Graphically explore slopes of a sample
set.seed(12)
df %>%
  # randomly sample 16 schools
  filter(IDSCHOOL %in% sample(unique(IDSCHOOL), 16)) %>%
  ggplot(aes(x = stress2_cmc, y = T3JOBSA)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~IDSCHOOL)

# Model 5
model52 <- lmer(T3JOBSA ~ stress2_cm + stress2_cmc + (stress2_cmc | IDSCHOOL), data = df)
# Summarize results
summary(model52)
# Likelihood-based confidence intervals for fixed effects
confint(model52)

# Plotting random slopes
augment(model52, data = df) %>% # augmented data (adding EB estimates)
  ggplot(aes(x = T3WELS, y = .fitted, color = factor(IDSCHOOL))) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction") +
  guides(color = "none")

# Create a common base graph
pbase <- augment(model52, data = df) %>%
  ggplot(aes(x = T3WELS, y = T3JOBSA, color = factor(IDSCHOOL))) +
  # Add points
  geom_point(size = 0.2, alpha = 0.2) +
  labs(y = "Job Satisfaction") +
  # Suppress legend
  guides(color = "none")
# Lv-1 effect
p1 <- pbase +
  # Add within-cluster lines
  geom_smooth(aes(y = .fitted),
              method = "lm", se = FALSE, size = 0.5)
# Lv-2 effect
p2 <- pbase +
  # Add group means
  stat_summary(aes(x = stress2_cm, y = .fitted),
               fun = mean,
               geom = "point",
               shape = 17,
               # use triangles
               size = 2.5) +
  # Add between coefficient
  geom_smooth(aes(x = stress2_cm, y = .fitted),
              method = "lm", se = FALSE,
              color = "black")
# Put the two graphs together (need the gridExtra package)
gridExtra::grid.arrange(p1, p2, ncol = 2)


# Effect size -------------------------------------------------------------

(r2_model52 <- r.squaredGLMM(model52))

# Cross-level interaction -------------------------------------------------
# Model 6
model62 <- lmer(T3JOBSA ~ stress2_cm + T3PLACPE * stress2_cmc + (stress2_cmc | IDSCHOOL), data = df)
# Summarize results
summary(model62)
# Likelihood-based confidence intervals for fixed effects
confint(model62)

# Effect size
(r2_model62 <- r.squaredGLMM(model62))
# Change
r2_model62 - r2_model52

# Plot the interaction
model62 %>%
  augment(data = df) %>%
  ggplot(aes(
    x = T3WELS, y = .fitted, group = factor(T3PLACPE),
    color = factor(T3PLACPE)
  )) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction", color = "T3PLACPE")

# Interaction just the fixed effects
interact_plot(model62,
              pred = "stress2_cmc",
              modx = "T3PLACPE",
              modx.labels = c("Not a problem", "A bit of a problem", "A problem"),
              plot.points = TRUE,
              point.size = 0.5,
              point.alpha = 0.2,
              facet.modx = TRUE,  # use this to make two panels
              x.label = "Stress (cluster-mean centered)",
              y.label = "Job Satisfaction")

# T3SELF ------------------------------------------------------------------

# Model with a level-2 variable: Mean workload stress --------------------
model23 <- lmer(T3JOBSA ~ tself_cm + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model23)
# Likelihood-based confidence intervals for fixed effects
confint(model23)

# Plot
sjPlot::plot_model(model23,
                   type = "pred",
                   terms = "tself_cm",
                   show.data = TRUE,
                   title = "",
                   dot.size = 0.5) +
  stat_summary(data = df, aes(x = tself_cm, y = T3JOBSA),
               fun = mean, geom = "point",
               col = "red",
               shape = 17,
               # use triangles
               size = 3,
               alpha = 0.7)


# Proportion of variance explained  ---------------------------------------
# Use Rights & Sterba (2019)
r2mlm::r2mlm(model22)



# Between-Within: Model with lvl-1 predictor: Workload stress  ------------
model32 <- lmer(T3JOBSA ~ stress2_cm + stress2_cmc + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model32)
# Likelihood-based confidence intervals for fixed effects
confint(model32)

# Contextual effect: Model with lvl-1 predictor: Workload stress ----------
model42 <- lmer(T3JOBSA ~ stress2_cm + T3WELS + (1 | IDSCHOOL), data = df)
# Summarize results
summary(model42)
# Likelihood-based confidence intervals for fixed effects
confint(model42)


# Random coefficients model -----------------------------------------------
# Graphically explore slopes of a sample
set.seed(12)
df %>%
  # randomly sample 16 schools
  filter(IDSCHOOL %in% sample(unique(IDSCHOOL), 16)) %>%
  ggplot(aes(x = stress2_cmc, y = T3JOBSA)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~IDSCHOOL)

# Model 5
model52 <- lmer(T3JOBSA ~ stress2_cm + stress2_cmc + (stress2_cmc | IDSCHOOL), data = df)
# Summarize results
summary(model52)
# Likelihood-based confidence intervals for fixed effects
confint(model52)

# Plotting random slopes
augment(model52, data = df) %>% # augmented data (adding EB estimates)
  ggplot(aes(x = T3WELS, y = .fitted, color = factor(IDSCHOOL))) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction") +
  guides(color = "none")

# Create a common base graph
pbase <- augment(model52, data = df) %>%
  ggplot(aes(x = T3WELS, y = T3JOBSA, color = factor(IDSCHOOL))) +
  # Add points
  geom_point(size = 0.2, alpha = 0.2) +
  labs(y = "Job Satisfaction") +
  # Suppress legend
  guides(color = "none")
# Lv-1 effect
p1 <- pbase +
  # Add within-cluster lines
  geom_smooth(aes(y = .fitted),
              method = "lm", se = FALSE, size = 0.5)
# Lv-2 effect
p2 <- pbase +
  # Add group means
  stat_summary(aes(x = stress2_cm, y = .fitted),
               fun = mean,
               geom = "point",
               shape = 17,
               # use triangles
               size = 2.5) +
  # Add between coefficient
  geom_smooth(aes(x = stress2_cm, y = .fitted),
              method = "lm", se = FALSE,
              color = "black")
# Put the two graphs together (need the gridExtra package)
gridExtra::grid.arrange(p1, p2, ncol = 2)


# Effect size -------------------------------------------------------------

(r2_model52 <- r.squaredGLMM(model52))

# Cross-level interaction -------------------------------------------------
# Model 6
model62 <- lmer(T3JOBSA ~ stress2_cm + T3PLACPE * stress2_cmc + (stress2_cmc | IDSCHOOL), data = df)
# Summarize results
summary(model62)
# Likelihood-based confidence intervals for fixed effects
confint(model62)

# Effect size
(r2_model62 <- r.squaredGLMM(model62))
# Change
r2_model62 - r2_model52

# Plot the interaction
model62 %>%
  augment(data = df) %>%
  ggplot(aes(
    x = T3WELS, y = .fitted, group = factor(T3PLACPE),
    color = factor(T3PLACPE)
  )) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction", color = "T3PLACPE")

# Interaction just the fixed effects
interact_plot(model62,
              pred = "stress2_cmc",
              modx = "T3PLACPE",
              modx.labels = c("Not a problem", "A bit of a problem", "A problem"),
              plot.points = TRUE,
              point.size = 0.5,
              point.alpha = 0.2,
              facet.modx = TRUE,  # use this to make two panels
              x.label = "Stress (cluster-mean centered)",
              y.label = "Job Satisfaction")
