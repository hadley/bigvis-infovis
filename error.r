library(bigvis)
library(ggplot2)
library(grid)
library(reshape2)
library(scales)
source("1-load.r")

tweak <- list(
  scale_x_continuous(breaks = c(500, 1000, 1500, 2000, 2500)),
  ylab(NULL),
  xlab(NULL),
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    panel.margin = unit(0.25, "cm")
  )
)

# 1d ---------------------------------------------------------------------------

dist_sum <- condense(bin(dist, 20), z = speed, summary = "sd")
dist_sum$err <- dist_sum$.sd / sqrt(dist_sum$.count)
dist_sum$rel_err <- pmin(dist_sum$err / dist_sum$.mean, 0.1)

qplot(dist, rel_err, data = dist_sum)
qplot(dist, rel_err, data = subset(dist_sum, rel_err < 0.05))

ggplot(dist_sum, aes(dist, .mean)) + 
  geom_line(aes(colour = .count / 1e3), size = 3) +
  scale_colour_gradient("Count\n(x 1000)", low = "grey90", high = "black") +
  expand_limits(colour = 0) + 
  tweak
ggsave("images/1d-count.pdf", width = 6, height = 2)  

ggplot(dist_sum, aes(dist, .mean)) + 
  geom_line(aes(colour = err), size = 3) +
  scale_colour_gradient("Absolute\nError", low = "black", high = "grey90") +
  expand_limits(colour = 0) + 
  tweak
ggsave("images/1d-error.pdf", width = 6, height = 2)  

ggplot(dist_sum, aes(dist, .mean)) + 
  geom_line(aes(colour = rel_err), size = 3) +
  scale_colour_gradient("Relative\nError", low = "black", high = "grey90",
    breaks = c(0, 0.05, 0.10), labels = c("0%", "5%", ">10%")) +
  expand_limits(colour = c(0, 0.1)) + 
  tweak
ggsave("images/1d-relerr.pdf", width = 6, height = 2)  

# 2d ---------------------------------------------------------------------------

dsd <- condense(bin(dist, 20), bin(speed, 20), z = delay, summary = "sd")
dsd$.err <- dsd$.sd / sqrt(dsd$.count)
dsd$.rel_err <- pmin(abs(dsd$.err / dsd$.mean), 0.1)
dsd <- subset(dsd, !is.na(.mean) & !is.na(.rel_err))

dsd2 <- peel(dsd, .995)
smooth_err <- smooth(dsd2, c(150, 50), var = ".rel_err")

levels <- c(0.01, 0.025, 0.05, 0.09)
ggplot(dsd2, aes(dist, speed, fill = .rel_err)) + 
  geom_raster() + 
  geom_contour(data = smooth_err, aes(z = .rel_err, colour = ..level..), 
    breaks = levels, size = 1) + 
  scale_fill_gradient("Relative\nerror", 
    low = "grey90", high = "black",
    breaks = c(0, 0.05, 0.10), labels = c("0%", "5%", ">10%"),
    guide = guide_colorbar(nbin = 10, barheight = unit(4, "line"))
  ) + 
  scale_colour_gradient("Smoothed\nrelative\nerror", 
    low = "white", high = "red", guide = guide_legend(), 
    breaks = levels, labels = percent) + 
  tweak
ggsave("images/2d-error.pdf", width = 6, height = 3)

ggplot(dsd2, aes(dist, speed, fill = .mean, alpha = .rel_err)) +
  geom_raster() +
  scale_fill_gradient2("Mean\nDelay",
    trans = mt_trans(0.25),
    breaks = c(-50, -10, 0, 10, 40, 100, 200, 400)) +
  scale_alpha("Relative\nError", 
    range = c(1, 0),
    breaks = c(0, 0.05, 0.10), labels = c("0%", "5%", ">10%")
  ) + 
  guides(fill = guide_colorbar(order = 1), alpha = guide_legend(order = 2)) +
  tweak
ggsave("images/2d-alpha.pdf", width = 6, height = 3)
