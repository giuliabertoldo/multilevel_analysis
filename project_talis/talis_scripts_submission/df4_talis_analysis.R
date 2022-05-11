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
theme_set(theme_bw())

# Load data ---------------------------------------------------------------
df4 <- read.csv('project_talis/talis_data/df4.csv')

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
  group_by(IDSCHOOL) %>%
  # CM = Cluster Mean
  # CMC = Cluster Mean Centered variable
  mutate(T3WELS_CM = mean(T3WELS),
         T3WELS_CMC = T3WELS - T3WELS_CM,
         T3SELF_CM = mean(T3SELF),
         T3SELF_CMC = T3SELF - T3SELF_CM) %>%
  ungroup() %>%
  # Grand mean centering (GMC) of the aggregated variable
  mutate(T3WELS_CM_GMC = T3WELS_CM - mean(T3WELS_CM))

# Check centering results
df4 %>%
  select(T3WELS,
         T3WELS_CM,
         T3WELS_CMC,
         T3WELS_CM_GMC,
         T3SELF_CM,
         T3SELF_CMC) %>%
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
ggplot(df4, aes(x=T3SELF_CMC)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = "Teacher's Self Efficacy, composite")
summary(df4$T3SELF_CMC)

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
ggplot(df4, aes(x=T3WELS_CMC)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Workplace Stress')
summary(df4$T3WELS_CMC)

# ** School location ------------------------------------------------------
df4 %>%
  group_by(IDSCHOOL) %>%
  filter(row_number()==1) %>%
  ggplot(aes(x=SCHLOC)) +
  geom_bar() +
  labs(x = "School Location")

# ** Mean workplace stress ------------------------------------------------
df4 %>%
  group_by(IDSCHOOL) %>%
  filter(row_number()==1) %>%
  ggplot(aes(x=T3WELS_CM_GMC)) +
  geom_histogram( colour="black", fill="white") +
  labs(x = 'Mean Workplace Stress')
summary(df4$T3WELS_CM_GMC)

# * Bivariate analysis ----------------------------------------------------

# ** Workplace Stress vs Job Satisfaction ---------------------------------
# Overall regression line
ggplot(data  = df4,
       aes(x = T3WELS_CMC,
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
       aes(x = T3WELS_CMC,
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
model1 <- lmer(T3JOBSA ~ 1 + (1 | IDSCHOOL), data = df4)
# Summarize results
summary(model1)
# Profile likelihood confidence intervals
confint(model1)

# Testing for "school effects"
# Null single level model
fit <- lm(T3JOBSA ~ 1, data = df4)
# Likelihood ratio
(ll_simple<-logLik(fit)[1])
(ll_complex <-logLik(model1)[1])
(LR <- -2*ll_simple-(-2*ll_complex)) # 1df because only 1 parameter difference
(pval_lr <- (pchisq(LR, df=1, lower.tail = FALSE)/2))

# * ICC -------------------------------------------------------------------
variance_components <- as.data.frame(VarCorr(model1))
(between_var <- variance_components$vcov[1])
(within_var <- variance_components$vcov[2])
(icc <- between_var / (between_var + within_var))

# * Empirical Bayes estimates ---------------------------------------------

dotplot(ranef(model1, condVar = TRUE))

# * Design effect ---------------------------------------------------------
cluster_size <- df4 %>%
  group_by(IDSCHOOL) %>%
  summarise(count = n())
(average_cluster_size <- mean(cluster_size$count))
(design_effect <- 1 + (average_cluster_size - 1) * icc)
# Effective sample size
(n_eff <- length(df4$IDTEACH)/design_effect)


# Add lvl-2 predictor: mean workplace stress ------------------------------
model2_pre <- lmer(T3JOBSA ~ T3WELS_CM_GMC + (1 | IDSCHOOL), data = df4)
# Summarize results
summary(model2_pre)
# Likelihood-based confidence intervals for fixed effects
confint(model2_pre)

# Plot
sjPlot::plot_model(model2_pre,
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
r2mlm::r2mlm(model2_pre)


# Add lvl-1 predicor: workplace stress (between-within model) -------------
model2 <- lmer(T3JOBSA ~ T3WELS_CMC+ T3WELS_CM_GMC + (1 | IDSCHOOL), data = df4)
# Summarize results
summary(model2)
# Likelihood-based confidence intervals for fixed effects
confint(model2)

# * Contextual effect -----------------------------------------------------
-0.60916 - (-0.45864)
## Look at the reparametrized model
model2_context <- lmer(T3JOBSA ~ T3WELS + T3WELS_CM_GMC + (1 | IDSCHOOL), data = df4)
summary(model2_context)
# Likelihood-based confidence intervals for fixed effects
confint(model2_context )


# * Visualizing the difference --------------------------------------------
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

# Create a common base graph
pbase <- augment(model2, data = df4) %>%
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
              method = "lm", se = FALSE, size = 0.5) +
  labs(x="Workload stress")
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
              color = "black") +
  labs(x="Workload stress")
# Put the two graphs together (need the gridExtra package)
gridExtra::grid.arrange(p1, p2, ncol = 2)
# Two separate graphs
p1
p2

# * How much variance explained? ------------------------------------------
# School-level variance
round((1-(0.1342/0.217)),2)
# Teacher-level variance
round((1-(2.6055/3.218)),2)


# * LRT for school-level variance -----------------------------------------
# Null single level model
fit <- lm(T3JOBSA ~ T3WELS + T3WELS_CM_GMC, data = df4)
# Likelihood ratio
(ll_simple<-logLik(fit)[1])
(ll_complex <-logLik(model2)[1])
(LR <- -2*ll_simple-(-2*ll_complex)) # 1df because only 1 parameter difference
(pval_lr <- (pchisq(LR, df=1, lower.tail = FALSE)/2))

# * Proportion of variance explained --------------------------------------
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model2)


# Random coefficients model -----------------------------------------------
# Graphically explore slopes of a sample
set.seed(2022)
df4 %>%
  # randomly sample 16 schools
  filter(IDSCHOOL %in% sample(unique(IDSCHOOL), 16)) %>%
  ggplot(aes(x = T3WELS_CMC, y = T3JOBSA)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~IDSCHOOL) +
  labs(x="Worplace stress",
       y="Job satisfaction")

# Model 3
model3 <- lmer(T3JOBSA ~ T3WELS_CM_GMC + T3WELS_CMC + (1 + T3WELS_CMC | IDSCHOOL),
               data = df4,
               control = lmerControl(optimizer = "bobyqa"))
# Summarize results
summary(model3)
# Likelihood-based confidence intervals for fixed effects
confint(model3)

# * LRT Random effects ---------------------------------------------------
anova(model2, model3)


# * Variance-covariance matrix --------------------------------------------
VarCorr(model3)$IDSCHOOL

# Plot of schools slopes vs schools intercepts
(myrandomeff <- ranef(model3, condVar = TRUE))
plot(myrandomeff[[1]],
     xlab = "Intercept(u0j)",
     ylab = "Slope (u1j)")
abline(h = 0 , col = "red")
abline(v=0, col = 'red')

# * Proportion of variance explained --------------------------------------
## Use Rights & Sterba (2019)
r2mlm::r2mlm(model3)

# Plotting random slopes
augment(model3, data = df4) %>% # augmented data (adding EB estimates)
  ggplot(aes(x = T3WELS_CMC, y = .fitted, color = factor(IDSCHOOL))) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(y = "Predicted Job Satisfaction") +
  guides(color = "none")

# Create a common base graph
pbase <- augment(model3, data = df4) %>%
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
              method = "lm", se = FALSE, size = 0.5)+
  labs(x= "Workplace stress")
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
              color = "black") +
  labs(x= "Workplace stress")
# Put the two graphs together (need the gridExtra package)
gridExtra::grid.arrange(p1, p2, ncol = 2)
# Print separate
p1

