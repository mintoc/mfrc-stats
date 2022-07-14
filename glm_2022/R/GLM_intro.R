##-------------------------
## MFRC Summer stats:
## Fitting GLMs in R: general structure (live coding)
## CM, OL: 14/7/2022
##-------------------------

?glm
?family

##----------------------------------
## LINEAR REGRESSION - GAUSSIAN GLM
##----------------------------------
## simulate some linear regression data
## NB: no need to do this when you have real data
n <- 20
x <- rnorm(n)
y <- rnorm(n, mean = 5 + 2*x, sd = 1)

plot(x, y)

## estimate the linear regression using glm
fit <- glm(y ~ x, family = gaussian)
summary(fit)

## add in fitted line

##-------------
## POISSON GLM
##-------------
## simuate some Poisson data
x <- rnorm(n, 6, 2)
y <- rpois(n, exp(-3 + 0.5 * x))

plot(x, y)

fit <- glm(y ~ x, family = poisson)
summary(fit)

## add in fitted line

##--------------
## BINOMIAL GLM
##--------------
## simuate some Poisson data
x <- rnorm(n, 6, 2)
y <- rbinom(n, size = 10, prob = plogis(-10 + 2 * x))

plot(x, y / 10)

## for binomial response best to make a matrix with the number of
## each category (e.g., mature, immature)

Y <- cbind(y, 10 - y)

## take a look at Y

fit <- glm(Y ~ x, family = binomial)
summary(fit)

## add in fitted line
