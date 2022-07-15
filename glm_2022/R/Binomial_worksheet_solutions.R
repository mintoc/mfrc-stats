##-------------------
## Binomial worksheet solutions
## CM, OL: 15/7/22
##
##-------------------

## read in the data
setwd("C:/Users/CMinto/Desktop/mfrc-stats-main/glm_2022/R")

dat <- read.csv("../data/haddock_maturity.csv")

## look at the data
head(dat)

with(dat, plot(length, mature, col = factor(sex)))

dat$pmature <- with(dat, mature / (mature + immature))
##dat$pmature <- dat$mature / (dat$mature + dat$immature)

with(dat, plot(length, pmature, col = "coral2", pch = 19, bty = "l",
               ylab = "Probability of being mature"))

Y <- cbind(dat$mature, dat$immature)

fit <- glm(Y ~ length, family = binomial, data = dat)
summary(fit)

xpred <- seq(min(dat$length), max(dat$length), length = 100)
## use the inverse logit link
ypred <- plogis(coef(fit)[1] + coef(fit)[2] * xpred)
lines(xpred, ypred, col = "tomato")

## include sex in the model
fit1 <- glm(Y ~ length + sex, family = binomial, data = dat)
summary(fit1)

pred_df <- expand.grid(sex = c("F", "M"),
                       length = xpred)

pred_df$pred_p <- predict(fit1, newdata = pred_df, type = "response")

with(dat, plot(length, pmature, col = factor(sex), pch = 19, bty = "l",
               ylab = "Probability of being mature"))

with(subset(pred_df, sex == "M"), lines(length, pred_p, col = 2))
with(subset(pred_df, sex == "F"), lines(length, pred_p, col = 1))

fit2 <- glm(Y ~ length * sex, family = binomial, data = dat)
summary(fit2)

pred_df$pred_p <- predict(fit2, newdata = pred_df, type = "response")

with(dat, plot(length, pmature, col = factor(sex), pch = 19, bty = "l",
               ylab = "Probability of being mature"))

with(subset(pred_df, sex == "M"), lines(length, pred_p, col = 2))
with(subset(pred_df, sex == "F"), lines(length, pred_p, col = 1))

