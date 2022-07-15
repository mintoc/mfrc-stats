##-------------------
## Poisson worksheet solutions
## CM, OL: 15/7/22
##
##-------------------

## read in the data
setwd("C:/Users/CMinto/Desktop/mfrc-stats-main/glm_2022/R")

dat <- read.table("../data/jellyfish.txt", header = TRUE)

my_colours <- ifelse(dat$area == "NE", "slateblue", "orange")

with(dat, plot(temp, count, pch = 19, col = my_colours, bty = "l"))
legend("topleft", legend = c("NE", "NW"), col = c("slateblue", "orange"), pch = 19)

## fits
fita <- glm(count ~ 1, family = poisson, data = dat)
summary(fita)
fitb <- glm(count ~ temp, family = poisson, data = dat)

xpred <- with(dat, seq(min(temp), max(temp), length = 100))
ypred <- exp(coef(fitb)[1] + coef(fitb)[2] * xpred)

lines(xpred, ypred)

## by area mean
fitc <- glm(count ~ area, family = poisson, data = dat)
summary(fitc)

fitd <- glm(count ~ area + temp, family = poisson, data = dat)
summary(fitd)

pred_df <- expand.grid(area = c("NE", "NW"),
                       temp = xpred)

pred_df$rate <- predict(fitd, newdata = pred_df, type = "response")

with(subset(pred_df, area == "NE"), lines(temp, rate, col = "slateblue"))
with(subset(pred_df, area == "NW"), lines(temp, rate, col = "orange"))

fite <- glm(count ~ area * temp, family = poisson, data = dat)

pred_df$rate <- predict(fite, newdata = pred_df, type = "response")

with(dat, plot(temp, count, pch = 19, col = my_colours, bty = "l"))
legend("topleft", legend = c("NE", "NW"), col = c("slateblue", "orange"), pch = 19)

with(subset(pred_df, area == "NE"), lines(temp, rate, col = "slateblue"))
with(subset(pred_df, area == "NW"), lines(temp, rate, col = "orange"))


