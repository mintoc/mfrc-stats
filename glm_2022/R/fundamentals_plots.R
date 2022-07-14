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


