##--------------------------
## Plots for fundamentals presentation
## CM, OL: 14/7/22
##
##--------------------------

dat <- read.table("../data/glm_citations.txt", header = TRUE)

dat <- dat[order(dat$year), ]
dat <- subset(dat, year != 2022) ## partial to date

pdf("../tex/figures/glm_citations.pdf", height = 6, width = 7)
with(dat, plot(year, count, type = "o", bty = "l", pch = 19, col = "purple", main = "Nelder and Wedderburn (1972) citations", xlab = "Year", ylab = "Count"))
dev.off()

y <- 0:10
py <- dpois(y, lambda = 3)

pdf("../tex/figures/distribution.pdf", height = 3, width = 3.5)
plot(y, py, type = "h", col = "purple", bty = "l", lwd = 2, xlab = "y", ylab = "Probability")
dev.off()


pdf("../tex/figures/link_function.pdf", height = 3, width = 3.5)
curve(exp(-3 + 0.5 * x), from = -3, to = 10, n = 1e3, bty = "l", ylab = "Rate", col = "purple")
dev.off()

x <- seq(-5, 5, length = 1e3)

pdf("../tex/figures/identity_link.pdf", height = 6, width = 7)
plot(x, x, bty = "n", xlab = expression(mu), ylab = expression(mu), type = "l", col = "darkred")
dev.off()


expx <- exp(x)

pdf("../tex/figures/log_link.pdf", height = 6, width = 7)
plot(expx, x, bty = "n", xlab = "Rate", ylab = "log(Rate)", type = "l", col = "darkred")
dev.off()

x <- seq(0, 1, length = 1e3)
logitx <- qlogis(x)


pdf("../tex/figures/logit_link.pdf", height = 6, width = 7)
plot(x, logitx, bty = "n", xlab = "Probability", ylab = "logit(Probability)", type = "l", col = "darkred")
dev.off()


