library(bigvis)
library(ggplot2)
library(scales)
source("1-load.r")

# Explore 1d distributions -----------------------------------------------------
speed_sum <- condense(bin(speed, 5))
autoplot(speed_sum)
autoplot(smooth(speed_sum, 26))

dist_sum <- condense(bin(dist, 10))
autoplot(dist_sum)
autoplot(smooth(dist_sum, 55))

ds <- condense(bin(dist, 50), bin(speed, 25))
autoplot(ds)
autoplot(peel(ds))

teaser <- list(
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(0, 0.5, 0, 0.5), "lines"),
    legend.key.width = unit(1.45, "inches"),
    text = element_text(size = 24)
  ),
  labs(x = NULL, y = NULL, fill = NULL))

# Make teaser images -----------------------------------------------------------
dsd <- condense(bin(dist, 20), bin(speed, 20), z = delay)
dsd <- subset(dsd, dist < 2700)
autoplot(dsd) + teaser
ggsave("images/teaser-1.pdf", width = 8, height = 6)

dsd2 <- peel(dsd, .995)
autoplot(dsd2) + teaser
ggsave("images/teaser-2.pdf", width = 8, height = 6)

mt_trans <- function(lambda) {
  trans_new("modulo",
    function(x) mt(x, lambda),
    function(x) inv_mt(x, lambda)
  )
}
autoplot(dsd2) + scale_fill_gradient2(trans = mt_trans(0.25),
  breaks = c(-50, -10, 0, 10, 40, 100, 200, 400)) + teaser
ggsave("images/teaser-3.pdf", width = 8, height = 6)


# Other exploration ------------------------------------------------------------

dsdp <- peel(dsd)
ggplot(dsdp, aes(dist, speed)) +
  geom_point(aes(colour = .mean, size = .count)) +
  scale_size_area() +
  scale_colour_gradient2()

ggplot(dsdp, aes(dist, speed)) +
  geom_raster(aes(fill = mt(.mean, 0.25))) +
  scale_fill_gradient2()


ggplot(dsdp, aes(dist, speed)) +
  geom_raster(aes(fill = .mean)) +
  geom_contour(aes(z = .mean), colour = "grey50") +
  scale_fill_gradient2()

dsd_speed <- transform(dsd, speed = is.na(speed))
qplot(dist, .mean, data = subset(dsd_speed, .count > 100), geom = "line", colour = factor(speed))
