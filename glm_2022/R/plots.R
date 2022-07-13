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

pdf("../tex/figures/count_y.pdf", height = 6, width = 7)
plot(x, y, pch = 19, col = "#f88379", bty = "l")
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

