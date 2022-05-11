# Load packages -----------------------------------------------------------
library(tidyverse)
library(GGally)
library(ggplot2)
library(DataExplorer)
library(lme4)
library(lattice)
library(sjPlot)
library(r2mlm)
library(broom)
library(performance)
library(effects)
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
# Convert to factor: Lack of resources
df4$T3PLACRE <- as.factor(df4$T3PLACRE)
nlevels(df4$T3PLACRE)
# Convert to factor: School location
df4$SCHLOC <- as.factor(df4$SCHLOC)
nlevels(df4$SCHLOC)
# Glimpse data
glimpse(df4)


# * Center lvl-1 predictors WELS & create lvl-2 predictor -----------------
df4 <- df4 %>%
  # GMC = Grand Mean Centered variable
  mutate(T3WELS_GMC = T3WELS - mean(T3WELS),
         T3SELF_GMC = T3SELF - mean(T3SELF))

# Check centering results
df4 %>%
  select(T3WELS,
         T3WELS_GMC,
         T3SELF,
         T3SELF_GMC) %>%
  summary


# Dataset description -----------------------------------------------------
# How many teachers?
length(unique(df4$IDTEACH))
# How many schools?
length(unique(df4$IDSCHOOL))

# Average number of teacher's per school
df4 %>%
  group_by(IDSCHOOL) %>%
  summarise(count = n()) %>%
  mutate(max = max(count),
         min = min(count),
         average = mean(count))


# Exploratory data analysis  ----------------------------------------------
# * Univariate analysis ---------------------------------------------------

# ** Teachers' gender -----------------------------------------------------
ggplot(df4, aes(x=TT3G01)) +
  geom_bar() +
  labs(x = 'Gender')
gender <- summary(df4$TT3G01)
(per_female <- gender[1]/(gender[1]+gender[2]))
(per_male <- gender[2]/(gender[1]+gender[2]))

# ** Teachers' age group --------------------------------------------------
ggplot(df4, aes(x=TCHAGEGR)) +
  geom_bar() +
  labs(x = 'Age group')


# ** Teachers' self efficacy ----------------------------------------------
ggplot(df4, aes(x=T3SELF_GMC)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = "Teacher's Self Efficacy, composite")
summary(df4$T3SELF_GMC)

# ** Job Satisfaction, composite ------------------------------------------
ggplot(df4, aes(x=T3JOBSA)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Job satisfaction, composite') +
  scale_x_continuous(breaks = seq(1:17))
summary(df4$T3JOBSA)

# Basic stripchart
ggplot(df4, aes(x=IDSCHOOL, y=T3JOBSA)) +
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red") +
  labs(y = "Job Satisfaction",
       x = "School ID") +
  theme(axis.text.x = element_text(angle = 90))

# Show variation across a subset of ten schools
set.seed(1994)
random_id <- sample(unique(df4$IDSCHOOL), size = 10)
(p_subset <- df4 %>%
    filter(IDSCHOOL %in% random_id) %>%  # select only 10 schools
    ggplot(aes(x = IDSCHOOL, y = T3JOBSA )) +
    geom_jitter(height = 0, width = 0.1, alpha = 0.3) +
    labs(y = "Job Satisfaction",
         x = "School ID") +
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

# ** Workplace stress -----------------------------------------------------
ggplot(df4, aes(x=T3WELS_GMC)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Workplace Stress')
summary(df4$T3WELS_GMC)

# ** School location ------------------------------------------------------
df4 %>%
  group_by(IDSCHOOL) %>%
  filter(row_number()==1) %>%
  ggplot(aes(x=SCHLOC)) +
  geom_bar() +
  labs(x = "School Location")


# * Bivariate analysis ----------------------------------------------------

# ** Workplace Stress vs Job Satisfaction ---------------------------------
# Overall regression line
ggplot(data  = df4,
       aes(x = T3WELS_GMC,
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
  labs(title = "Job Satisfaction vs. Workplace Stress",
       subtitle = "Colored by school",
       x = "Workplace Stress",
       y = "Job Satisfaction")

# One regression line per school
ggplot(data  = df4,
       aes(x = T3WELS_GMC,
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
  labs(title = "Job Satisfaction vs. Workplace Stress",
       subtitle = "Colored by school",
       x = "Workplace Stress",
       y = "Job Satisfaction")

# Unconditional random intercept model ------------------------------------
model1 <- lmer(T3JOBSA ~ 1 + (1 | IDSCHOOL),
               data = df4,
               REML = FALSE)
# Summarize results
summary(model1)
# Profile likelihood confidence intervals
confint(model1)

# Testing for "school effects"
# Null single level model
fit <- lm(T3JOBSA ~ 1, data = df4)
summary(fit)
# Likelihood ratio
logLik(fit)
logLik(model1)
(LR <- 2*(-7001.487-(-7035.406))) # 1df because only 1 parameter difference
(pval1 <- (pchisq(LR, df=1, lower.tail = FALSE)/2))

# * ICC -------------------------------------------------------------------
# Manual calculation
variance_components <- as.data.frame(VarCorr(model1))
(between_var <- variance_components$vcov[1])
(within_var <- variance_components$vcov[2])
(icc <- between_var / (between_var + within_var))
# Check with performance package
performance::icc(model1)

# * Design effect ---------------------------------------------------------
cluster_size <- df4 %>%
  group_by(IDSCHOOL) %>%
  summarise(count = n())
(average_cluster_size <- mean(cluster_size$count))
(design_effect <- 1 + (average_cluster_size - 1) * icc)
# Effective sample size
(n_eff <- length(df4$IDTEACH)/design_effect)

# Model 2: Add lower level-explanatory variables, fixed,  ML --------------
# * Model 2a: Workplace stress  -------------------------------------------
model2a_ml <- lmer(T3JOBSA ~ T3WELS_GMC + (1 | IDSCHOOL),
                   data = df4,
                   REML = FALSE)            # to compare fixed var sig
model2a_reml <- lmer(T3JOBSA ~ T3WELS_GMC + (1 | IDSCHOOL),
                     data = df4,
                     REML = TRUE)           # for R-sq calcs
summary(model2a_reml )

# Assess significance of effects
anova(model1, model2a_ml)

# What does the model look like?
effects::Effect(focal.predictors = c("T3WELS_GMC"),
                mod = model2a_ml) %>%
  data.frame() %>%
  ggplot(aes(x = T3WELS_GMC,
             y = fit)) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = .3) +
  geom_line()

# Proportion of variance explained
## Extract the variance covariance estimates
## Baseline model (BL = Baseline)
VarCorr(model1) %>%
  print(comp = c("Variance", "Std.Dev"),
        digits = 4)
## Model to compare (REML estimate)
VarCorr(model2a_ml) %>%
  print(comp = c("Variance", "Std.Dev"),
        digits = 4)
# Use the performance package
performance::r2(model2a_ml)
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model2a_ml)

# * Model 2b: Add covariates - age gender self efficacy -------------------
model2b_ml <- lmer(T3JOBSA ~ TT3G01 * T3WELS_GMC + (1 | IDSCHOOL),
                   data = df4,
                   REML = FALSE)            # to compare fixed var sig
model2b_reml <- lmer(T3JOBSA ~ TT3G01 * T3WELS_GMC  + (1 | IDSCHOOL),
                     data = df4,
                     REML = TRUE)           # for R-sq calcs
summary(model2b_reml)

# Assess significance of effects
anova(model1, model2a_ml, model2b_ml)

# What does the model look like?
effects::Effect(focal.predictors = c("T3WELS_GMC"),
                mod = model2b_ml) %>%
  data.frame() %>%
  ggplot(aes(x = T3WELS_GMC,
             y = fit)) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = .3) +
  geom_line()

# Proportion of variance explained
## Extract the variance covariance estimates
## Baseline model (BL = Baseline)
VarCorr(model1) %>%
  print(comp = c("Variance", "Std.Dev"),
        digits = 4)
## Model to compare (REML estimate)
VarCorr(model2a_ml) %>%
  print(comp = c("Variance", "Std.Dev"),
        digits = 4)

VarCorr(model2b_ml) %>%
  print(comp = c("Variance", "Std.Dev"),
        digits = 4)

# Use the performance package
performance::r2(model2b_ml)
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model2b_ml)

# Model 3: Add higher-level explanatory variables, fixed, ML --------------


# * Model 3a: Lack of resources -------------------------------------------


# * Model 3b: School location ---------------------------------------------


# * Model 3c: Lack of resources and school location -----------------------


# Model 4: Explanatory variables predict slopes, random, REML -------------


# Model 5: Cross-level interaction ----------------------------------------


# Model diagnostics -------------------------------------------------------


# * Workplace stress ------------------------------------------------------


model1.1 <- lmer(T3JOBSA ~ T3WELS_CMC + (1 | IDSCHOOL), data = df4)
# Summarize results
summary(model1.1)
# Likelihood-based confidence intervals for fixed effects
confint(model1.1)

# Plot
plot1.1 <- sjPlot::plot_model(model1.1,
                              type = "pred",
                              terms = "T3WELS_CMC",
                              show.data = TRUE,
                              title = "",
                              dot.size = 0.5)


# Add lvl-2 predictor: mean workplace stress ------------------------------
model2 <- lmer(T3JOBSA ~ T3WELS_CM_GMC + (1 | IDSCHOOL), data = df4)
# Summarize results
summary(model2)
# Likelihood-based confidence intervals for fixed effects
confint(model2)

# Plot
sjPlot::plot_model(model2,
                   type = "pred",
                   terms = "T3WELS_CM_GMC",
                   show.data = TRUE,
                   title = "",
                   dot.size = 0.5) +
  stat_summary(data = df4, aes(x = T3WELS_CM_GMC, y = T3JOBSA),
               fun = mean, geom = "point",
               col = "red",
               shape = 17,
               # use triangles
               size = 3,
               alpha = 0.7)

# Proportion of variance explained
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model2)


# Add lvl-1 predicor: workplace stress (between-within model) -------------
model3 <- lmer(T3JOBSA ~ T3WELS_CM_GMC + T3WELS_CMC + (1 | IDSCHOOL), data = df4)
# Summarize results
summary(model3)
# Likelihood-based confidence intervals for fixed effects
confint(model3)

# * LRT Fixed effects -----------------------------------------------------


# * LRT Random effects ----------------------------------------------------


# * Proportion of variance explained --------------------------------------
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model3)


# Random coefficients model -----------------------------------------------
# Graphically explore slopes of a sample
set.seed(2022)
df4 %>%
  # randomly sample 16 schools
  filter(IDSCHOOL %in% sample(unique(IDSCHOOL), 16)) %>%
  ggplot(aes(x = T3WELS_CMC, y = T3JOBSA)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~IDSCHOOL)

# Model 4
model4 <- lmer(T3JOBSA ~ T3WELS_CM_GMC + T3WELS_CMC + (1 + T3WELS_CMC | IDSCHOOL),
               data = df4,
               control = lmerControl(optimizer = "bobyqa"))
# Summarize results
summary(model4)
# Likelihood-based confidence intervals for fixed effects
confint(model4)


# * LRT Fixed effects -----------------------------------------------------


# * LRT Random effects ---------------------------------------------------


# * Proportion of variance explained --------------------------------------
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model4)

# Plotting random slopes
augment(model4, data = df4) %>% # augmented data (adding EB estimates)
  ggplot(aes(x = T3WELS_CMC, y = .fitted, color = factor(IDSCHOOL))) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction") +
  guides(color = "none")

# Create a common base graph
pbase <- augment(model4, data = df4) %>%
  ggplot(aes(x = T3WELS_CMC, y = T3JOBSA, color = factor(IDSCHOOL))) +
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
  stat_summary(aes(x = T3WELS_CM_GMC, y = .fitted),
               fun = mean,
               geom = "point",
               shape = 17,
               # use triangles
               size = 2.5) +
  # Add between coefficient
  geom_smooth(aes(x = T3WELS_CM_GMC, y = .fitted),
              method = "lm", se = FALSE,
              color = "black")
# Put the two graphs together (need the gridExtra package)
gridExtra::grid.arrange(p1, p2, ncol = 2)



# Add level-1 covariate ---------------------------------------------------

# Model 4
model5 <- lmer(T3JOBSA ~ T3WELS_CM_GMC + T3WELS_CMC + T3SELF_CMC + (1 + T3WELS_CMC | IDSCHOOL),
               data = df4,
               control = lmerControl(optimizer = "bobyqa"))
# Summarize results
summary(model5)
# Likelihood-based confidence intervals for fixed effects
confint(model5)


# * LRT Fixed effects -----------------------------------------------------


# * LRT Random effects ---------------------------------------------------


# * Proportion of variance explained --------------------------------------
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model5)


# Model diagnostics -------------------------------------------------------




# Cross-level interaction using Schloc? -----------------------------------





