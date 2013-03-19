# flesh out dgrid class
# check (AGAIN) breaks
# explore optimisation problems when n is small

# library(bigvis)
load_all("../bigvis")
library(ggplot2)

m4 <- function(x, sigma = 1) {
  24 * sqrt(x * (1 - x)) * sin(2 * pi * 1.05 / (x + 0.05)) +
    rnorm(length(x), sd = sigma)
}

plot(function(x) m4(x, 0), n = 1000)


x <- runif(1e3, 0, 1)
y <- m4(x)

xsum <- condense(bin(x, 1/1e3), z = y, drop = TRUE)
qplot(x, .mean, data = xsum, geom = "line")

pieces <- function(m) m / (10 * log10(m))
width <- function(x) diff(range(x)) / pieces(length(x))

pieces <- split(xsum, as.integer(bin(xsum$x, width(xsum$x) * 2)))
hs <- lapply(pieces, best_h, var = ".mean")
