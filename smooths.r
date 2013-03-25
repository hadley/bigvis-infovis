library(plyr)
library(ggplot2)
library(MASS)

set.seed(1014)

x <- seq(0, pi, length = 50)
y <- sin(x) + rnorm(length(x), sd = 0.2)
y[25] <- -1
# plot(x, y)

# Binned mean -----------------------------------------------------
gx <- round_any(x, pi / 10, floor)
y2 <- ave(y, gx, FUN = mean)

# Running mean -----------------------------------------------------

w <- function(x, h) {
  abs(x) <= h
}

y3 <- unlist(Map(function(xi) {
  weighted.mean(y, w(x - xi, pi / 10))
}, x))


# Kernel smooth ----------------------------------------------------

tricube <- function(x, h) {
  y <- abs(x) / h
  ifelse(y > 1, 0, (1 - y ^ 3) ^ 3)
}

# Need to adjust bandwidth so total weights = 5
find_h <- function(h) sum(tricube(x - x[25], h)) - 5
uniroot(find_h, c(0.01, 1))
sum(tricube(x - x[25], 0.277))
sum(tricube(x - x[25], 0.277) > 0)
# 9 bins

y4 <- unlist(Map(function(xi) {
  weighted.mean(y, tricube(x - xi, 0.277))
}, x))

# Kernel regression ------------------------------------------------

y5 <- unlist(Map(function(xi) {
  mod <- lm(y ~ x, weights = tricube(x - xi, 0.277))
  predict(mod, data.frame(x = xi))
}, x))

# Robust kernel regression -----------------------------------------

y6 <- unlist(Map(function(xi) {
  mod <- rlm(y ~ x, weights = tricube(x - xi, 0.277), method = "MM",
    wt.method = "case")
  predict(mod, data.frame(x = xi))
}, x))


all <- rbind(
  data.frame(x, cur = y2, prev = NA, type = "binned"),
  data.frame(x, cur = y3, prev = y2, type = "running"),
  data.frame(x, cur = y4, prev = y3, type = "smoothed"),
  data.frame(x, cur = y5, prev = y4, type = "regression"),
  data.frame(x, cur = y6, prev = y5, type = "robust")
)

ggplot(all, aes(x, y)) +
  geom_point(data = df) +
  geom_line(aes(y = prev), na.rm = TRUE, col = "red") +
  geom_line(aes(y = cur), size = 1) +
  facet_wrap(~ type, ncol = 1) +
  theme(
    text = element_text(size = 18),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    panel.margin = unit(0.25, "cm")
  ) +
  xlab(NULL) +
  ylab(NULL)

ggsave("images/smooth-types.pdf", height = 10, width = 6)
