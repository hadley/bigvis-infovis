library(bigvis)
library(ggplot2)
library(grid)
library(reshape2)
source("1-load.r")

tweak <- list(
  scale_x_continuous("Distance (miles)", breaks = c(500, 1000, 1500, 2000, 2500)),
  ylab(NULL),
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    text = element_text(size = 18),
    panel.margin = unit(0.25, "cm")
  )
)

dist_sum <- condense(bin(dist, 10), z = speed, summary = "sd")
dist_sum$.count <- dist_sum$.count / 1e6

dsm <- data.frame(
  dist = rep(dist_sum$dist, 3),
  summary = rep(c("count (millions)", "mean", "sd"), each = nrow(dist_sum)),
  value = c(dist_sum$.count, dist_sum$.mean, dist_sum$.sd)
)

qplot(dist, value, data = dsm, geom = "line") +
  facet_grid(summary ~ ., scales = "free_y") +
  tweak
ggsave("images/condense.pdf", width = 8, height = 7)


dist_sum <- condense(bin(dist, 10), z = speed, summary = "sd")

best_h(dist_sum, var = ".count")
best_h(dist_sum, var = ".mean")
best_h(dist_sum, var = ".sd")

smoothes <- list(
  count = smooth(dist_sum, 50, var = ".count", type = "robust"),
  mean = smooth(dist_sum, 50, var = ".mean", type = "robust"),
  sd = smooth(dist_sum, 50, var = ".sd", type = "robust"))
smoothes <- Map(function(x, n) {
  names(x)[2] <- "value"
  x$summary <- n
  x
}, smoothes, names(smoothes))
smooth <- do.call(rbind, smoothes)
smooth$summary <- factor(smooth$summary, levels = c("count", "mean", "sd"))
levels(smooth$summary)[1] <- "count (millions)"

qplot(dist, value, data = smooth, geom = "line") +
  facet_grid(summary ~ ., scales = "free_y") +
  tweak
ggsave("images/smooth.pdf", width = 8, height = 7)
