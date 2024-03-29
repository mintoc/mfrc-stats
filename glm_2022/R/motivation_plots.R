##--------------------------
## Plots for presentations
## CM, OL: 12/7/22
##
##--------------------------

atugreen <- "#005b5e"

set.seed(101)
n <- 20
x <- rnorm(n, 6, 2)

set.seed(103)
y <- rnorm(n, -10 + 2 * x, 1)

pdf("../tex/figures/continuous_y_0.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = atugreen, bty = "l")
abline(h = 0, lty = 2)
dev.off()

## positive continuous
set.seed(104)
y0 <- rnorm(n, -5 + 0.5 * x, 1)
y <- exp(y0)

pdf("../tex/figures/continuous_y_1.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "orange2", bty = "l")
abline(h = 0, lty = 2)
dev.off()

## count data
set.seed(105)
y <- rpois(n, exp(-3 + 0.5 * x))

coral <- "#f88379"

pdf("../tex/figures/count_y.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = coral, bty = "l")
abline(h = 0, lty = 2)
dev.off()

y <- rbinom(n, size = 1, prob = plogis(-10 + 2 * x))


pdf("../tex/figures/binary_y.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "cornflowerblue", bty = "l")
dev.off()



##---------------
## distributions 
##---------------
rm(x)
pdf("../tex/figures/normal_0.pdf", height = 6, width = 7)
curve(dnorm(x), from = -3, to = 3, n = 1e3, bty = "l", col = atugreen, xlab = "y", ylab = "Density")
dev.off()

pdf("../tex/figures/lognormal_0.pdf", height = 6, width = 7)
curve(dlnorm(x), from = 0, to = 5, n = 1e3, bty = "l", col = "orange2", xlab = "y", ylab = "Density")
dev.off()


y <- 0:10
py <- dpois(y, lambda = 3)

pdf("../tex/figures/poisson_0.pdf", height = 6, width = 7)
plot(y, py, type = "h", col = coral, bty = "l", lwd = 2, xlab = "y", ylab = "Probability")
dev.off()

y <- 0:1
py <- dbinom(y, 1, prob = 0.3)

pdf("../tex/figures/binary_0.pdf", height = 6, width = 7)
plot(y, py, type = "h", col = "cornflowerblue", bty = "l", lwd = 2, xlab = "y", ylab = "Probability", ylim = c(0, 1), xaxt = "n")
axis(1, at = y)
dev.off()


## binomial
y <- 0:10
py <- dbinom(y, 10, prob = 0.3)

pdf("../tex/figures/binomial_0.pdf", height = 6, width = 7)
plot(y, py, type = "h", col = "cornflowerblue", bty = "l", lwd = 2, xlab = "y", ylab = "Probability", ylim = c(0, 0.4), xaxt = "n")
axis(1, at = y)
dev.off()

##-----------------------
## EXPLANATORY VARIABLES
##-----------------------
set.seed(101)
x <- rnorm(n, 6, 2)

set.seed(103)
y <- rnorm(n, -10 + 2 * x, 1)

pdf("../tex/figures/continuous_xy_0.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = atugreen, bty = "l")
abline(lm(y ~ x), col = atugreen)
dev.off()

## positive continuous
set.seed(104)
y0 <- rnorm(n, -5 + 0.5 * x, 1)
y <- exp(y0)

pdf("../tex/figures/continuous_xy_1.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "orange2", bty = "l", ylim = c(-0.5, max(y)))
abline(h = 0, lty = 2)
abline(lm(y ~ x), col = "orange2")
dev.off()

fit <- lm(log(y) ~ x)
theta <- coef(fit)
pdf("../tex/figures/continuous_xy_2.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "orange2", bty = "l", ylim = c(-0.5, max(y)))
abline(h = 0, lty = 2)
curve(exp(theta[1] + theta[2] * x), col = "orange2", add = TRUE)
dev.off()


## count data
set.seed(105)
y <- rpois(n, exp(-3 + 0.5 * x))

coral <- "#f88379"

fit <- glm(y ~ x, family = poisson)
theta <- coef(fit)


pdf("../tex/figures/count_xy_0.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = coral, bty = "l", ylim = c(-3, max(y)))
abline(h = 0, lty = 2)
abline(lm(y ~ x), col = coral)
dev.off()

pdf("../tex/figures/count_xy_1.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = coral, bty = "l", ylim = c(-3, max(y)))
abline(h = 0, lty = 2)
curve(exp(theta[1] + theta[2] * x), add = TRUE, col = coral)
dev.off()

y <- rbinom(n, size = 1, prob = plogis(-10 + 2 * x))


pdf("../tex/figures/binary_xy_0.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "cornflowerblue", bty = "l", ylim = c(-1, 2))
abline(lm(y ~ x), col = "cornflowerblue")
dev.off()

pdf("../tex/figures/binary_xy_1.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "cornflowerblue", bty = "l", ylim = c(-1, 2))
curve(plogis(-10 + 2 * x), col = "cornflowerblue", add = TRUE)
dev.off()






