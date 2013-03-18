library(plyr)
library(ggplot2)

grid <- seq(0, pi, length = 50)
x <- rep(grid, each = 3)
ux <- unique(x)
y <- sin(x) + rt(length(x), df = 5) / 5

df <- data.frame(x, y)
base <- ggplot(df, aes(x, y)) + geom_point() + theme_bw()

# Binned mean -----------------------------------------------------
gx <- round_any(x, pi / 20, floor)
run_mean <- unname(tapply(y, gx, mean))

binned <- data.frame(x = unique(gx), y = run_mean)
base + geom_step(data = binned, size = 1)

# Running mean -----------------------------------------------------

w <- function(x, xs, h) {
  abs(x - xs) < h
}

y3 <- unlist(Map(function(xi) {
  weighted.mean(y, w(xi, x, 0.06))
}, ux))

running <- data.frame(x = ux, y = y3)
base + 
  geom_step(data = binned, col = "grey50") +
  geom_line(data = running, size = 1)


# Kernel smooth ----------------------------------------------------

w2 <- function(x, xs, h) {
  dnorm((x - xs) / h)
}

y4 <- unlist(Map(function(xi) {
  weighted.mean(y, w2(xi, x, 0.06))
}, ux))

smoothed <- data.frame(x = ux, y = y4)

base + 
  geom_line(data = running, col = "grey50") +
  geom_line(data = smoothed, size = 1)

# Kernel regression ------------------------------------------------

y5 <- unlist(Map(function(xi) {
  mod <- lm(y ~ x, weights = w2(xi, x, 0.06))
  predict(mod, data.frame(x = xi))
}, ux))
regressed <- data.frame(x = ux, y = y5)

base + 
  geom_line(data = smoothed, col = "grey50") +
  geom_line(data = regressed, size = 1)

