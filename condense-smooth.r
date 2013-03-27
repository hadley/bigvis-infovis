library(bigvis)
library(ggplot2)
library(grid)
library(reshape2)
library(scales)
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

# Figure out optimal smoothing characteristics --------------------

best_h(dist_sum, var = ".count")
best_h(dist_sum, var = ".mean")
best_h(dist_sum, var = ".sd")

# dist_sum2 <- transform(dist_sum, 
#   .count = scale(.count), 
#   .mean = scale(.count), 
#   .sd = scale(.sd))
# 
# best <- list(
#   count = best_h(dist_sum2, var = ".count"),
#   mean = best_h(dist_sum2, var = ".mean"),
#   sd = best_h(dist_sum2, var = ".sd"))
# bestdf <- data.frame(
#   summary = names(best), 
#   dist = unlist(best)
#   err =
# )


grid <- h_grid(dist_sum2, max = 10, n = 50)
rmses <- list(
  count = rmse_cvs(dist_sum2, grid, var = ".count"),
  mean = rmse_cvs(dist_sum2, grid, var = ".mean"),
  sd = rmse_cvs(dist_sum2, grid, var = ".sd")  
)
rmses <- Map(function(x, n) {
  x$summary <- n
  x
}, rmses, names(rmses))
rmse <- do.call(rbind, rmses)

qplot(dist, pmin(err, 2), data = rmse, colour  = summary, geom = "line") +
  ylab("rmse") +
  scale_x_continuous("Bandwidth (miles)") + 
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )
ggsave("images/smooth-rmse.pdf", width = 6, height = 3)


# Display smoothed data in one plot -----------------------------------

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

# 2d ----------------

ds <- condense(bin(dist, 50), bin(speed, 20))

mt_trans <- function(lambda) {
  trans_new("modulo",
    function(x) mt(x, lambda),
    function(x) inv_mt(x, lambda)
  )
}


ggplot(ds, aes(dist, speed, fill = .count)) +
  geom_raster() +
  scale_fill_gradient("Count\n(x 1000)", low = "grey90", high = "black",
    breaks = c(1, 2, 3, 5, 10) * 1e5,
    labels = c("100", "200", "300", "500", "1000"),
    trans = mt_trans(0.5), guide = guide_colorbar(
      title.vjust = 0.75, barwidth = unit(5, "inches")
    )
  ) +
  scale_x_continuous("Distance (miles)",
    breaks = c(500, 1000, 1500, 2000, 2500)) +
  ylab("Speed (mph)") +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    text = element_text(size = 18),
    legend.position = "bottom"
  )
ggsave("images/condense-2d.pdf", width = 8, height = 6)
