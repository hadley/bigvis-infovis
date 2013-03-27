library(bigvis)
library(ggplot2)
library(plyr)
library(grid)

delay <- readRDS("data/delay.rds")
dist <- readRDS("data/dist.rds")
time <- readRDS("data/time.rds")

speed <- dist / time * 60

# Determine number of missing values ---------------------

bad_speed <- speed < 0 | speed > 761
bad_time <- time < 0
bad_dist <- dist > 2724

tabulate(bad_speed + bad_time + bad_time + 1, 4)

# Compare distributions of missings to non-missings ---------------

dist[bad_dist] <- NA
speed[bad_speed] <- NA

sd <- condense(bin(dist, 10), bin(speed, 10))

sd2 <- transform(sd, dist = is.na(dist))
sd2 <- ddply(sd2, "dist", mutate, .count = .count / sum(.count))

qplot(speed, .count, data = sd2, geom = "line", colour = factor(dist)) + 
  scale_colour_hue("Distance", breaks = c(0, 1), 
    labels = c("Present", "Missing")) +
  theme(
    legend.position = c(0, 1), 
    legend.justification = c("left", "top"),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "lines")) +
  ylab("Proportion")
ggsave("images/speed-distance.pdf", width = 6, height = 3)
