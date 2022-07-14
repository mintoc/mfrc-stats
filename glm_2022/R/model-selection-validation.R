##--------------------------
## Model selection
## CM, OL: 13/7/22
##
##--------------------------

# preventing fiting data to different dataset
options(na.action = "na.fail")

# Read data
file <- "../data/jellyfish.txt"
dat <- read.table(file, header = TRUE, sep = " ")


## model selection

# nested models
modA <- glm(count ~ temp, data = dat, family = poisson(link="log"))
modB <- glm(count ~ temp + area + area:temp,
             data = dat, family = poisson(link="log"))
modC <- glm(count ~ area:temp, data = dat, family = poisson(link="log"))

# aummary
summary(modA)
summary(modB)
summary(modC)

# AICs
AIC(modA)
AIC(modB)
AIC(modC)

# anova
anova(modA, modB, modC, test='Chisq')
anova(modB)

# stepwise selection
mod.step <- MASS::stepAIC(modB, trace = FALSE)
mod.step$anova

# multimodel
?MuMIn::dredge
?MuMIn::model.sel

# models are ranked
mod.sel <- MuMIn::model.sel(modA, modB, modC, rank = AIC)
mod.sel

# select with delta AIC less 5
subset(mod.sel, delta <5)

# dredge - for exploratory purposes only

mod.dredge <- MuMIn::dredge(modB)

# grab best supported models
subset(mod.dredge, delta <5)

## model diagnostics

# - Residuals vs Fitted. Used to check the linear relationship assumptions. 
# A horizontal line, without distinct patterns is an indication for a linear relationship, 
# what is good.
# - Normal Q-Q. Used to examine whether the residuals are normally distributed. 
# It’s good if residuals points follow the straight dashed line.
# - Scale-Location (or Spread-Location). 
# Used to check the homogeneity of variance of the residuals (homoscedasticity). 
# Horizontal line with equally spread points is a good indication of homoscedasticity. 
# - Residuals vs Leverage. Used to identify influential cases, 
# that is extreme values that might influence the regression results when included or 
# excluded from the analysis. 

par(mfrow=c(2,2))
plot(modB)

# access plots individually
plot(modB, 4)

