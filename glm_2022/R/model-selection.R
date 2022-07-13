##--------------------------
## Model selection
## CM, OL: 13/7/22
##
##--------------------------

# Read data
file <- "../data/herring-ices.csv"
dat <- read.csv(file)

# Remove missing rows
dat <- dat[complete.cases(dat), ]
head(dat)

# 2 nested models
modA <- glm(weight ~ age, data = dat)
modB <- glm(weight ~ age + sst + sal + recr + ybir,
             data = dat)

# Summary
summary(modA)
summary(modB)

# AICs
AIC(modA)
AIC(modB)

# Stepwise selection
mod2.step <- MASS::stepAIC(modB, trace = FALSE)
mod2.step$anova

# Anova
anova(modA, modB)
anova(modB)

