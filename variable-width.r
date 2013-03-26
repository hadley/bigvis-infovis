library(bigvis)
library(ggplot2)
library(grid)
library(memoise)
set.seed(1013)

tweak <- list(
  theme(
    text = element_text(size = 18),
    plot.margin = unit(c(0.5, 0.25, 0, 0.25), "lines")),
  xlab(NULL)
)
  
# From J. Fan and I. Gijbels. Data-driven bandwidth selection in local 
# polynomial fitting: variable bandwidth and spatial adaptation. Journal of the
# Royal Statistical Society. Series B (Methodological), pages 371–394, 1995.
m4 <- function(x, sigma = 1) {
  24 * sqrt(x * (1 - x)) * sin(2 * pi * 1.05 / (x + 0.05)) +
    rnorm(length(x), sd = sigma)
}

x <- runif(1e3, 0, 1)
y <- m4(x)

xsum <- condense(bin(x, 1/1e3), z = y, drop = TRUE)
qplot(x, .mean, data = xsum, geom = "line") + tweak
ggsave("images/variable-h-raw.pdf", width = 6, height = 6)

# Overall smooth ---------------------------------------------------------------
# best_h(xsum, var = ".mean")
# 0.00337894

xsmu <- smooth(xsum, 0.0033, var = ".mean")
qplot(x, .mean, data = xsmu, geom = "line") + tweak
ggsave("images/variable-h-smoothed-fixed.pdf", width = 6, height = 6)

# Variable smooth --------------------------------------------------------------

npieces <- function(m) floor(m / (10 * log(m)))
width <- function(x) diff(range(x)) / npieces(length(x))
npieces(nrow(xsum))
width(xsum$x)

bins <- as.integer(c(xsum$x / 0.1))
pieces <- split(xsum, bins)

mid <- seq.int(0, max(bins)) * 0.1
best_h2 <- memoise(best_h)
hs <- lapply(pieces, best_h2, var = ".mean")
failed <- vapply(hs, function(x) attr(x, "conv") != 0, logical(1))

widths <- data.frame(mid = mid, h = unlist(hs))
widths$h[failed] <- NA
widths$smoothed <- predict(loess(h ~ mid, data = widths, na.action = na.exclude))

ggplot(aes(mid, h), data = widths) + 
  geom_line(aes(col = "raw")) + 
  geom_line(aes(y = smoothed, col = "smoothed")) + 
  scale_colour_brewer(palette = "Set1") + 
  labs(colour = NULL) + 
  theme(legend.position = c(0, 1), legend.justification = c("left", "top")) +
  tweak
ggsave("images/variable-h-h-est.pdf", width = 6, height = 6)


# Results --------------------------------------------------------------

hf <- approxfun(widths$mid, widths$smoothed, rule = 2)
smooth_one <- function(i) {
  row <- xsum[i, , drop = FALSE]
  smooth(xsum, h = hf(row$x), grid = as.matrix(row[1]), var = ".mean")
}

xvar <- do.call("rbind", lapply(seq_len(nrow(xsum)), smooth_one))
autoplot(xvar) + tweak
ggsave("images/variable-h-smoothed-var.pdf", width = 6, height = 6)
