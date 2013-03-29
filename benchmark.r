library(bigvis)
library(ggplot2)
library(grid)
library(plyr)

n <- 10 ^ c(6, 7, 8)
n2 <- paste0("x", 6:8)
names(n) <- n2

bins <- 10 ^ c(2, 3, 4, 5, 6)
stats <- c("count", "sd", "median")

combs <- expand.grid(n2 = n2, bin = bins, stat = stats)

as.data.frame.proc_time <- function(x) {
  t(x[1:3])
}

bench <- function(n2, bin, stat) {
  if (n[[n2]] / bin <= 10) return()
  xbin <- bin(data[[n2]], 1 / bin)
  system.time(condense(xbin, summary = stat))
}

if (file.exists("timing.rds")) {
  timing <- readRDS("timing.rds")
} else {
  data <- lapply(n, runif)
  names(data) <- n2
  
  timing <- mdply(combs, bench, .progress = "text")
  saveRDS(timing, "timing.rds")  
}

timing$stat <- factor(timing$stat, levels = c("count", "sd", "median"))

ggplot(timing, aes(bin, elapsed, colour = n2, linetype = stat, shape = stat)) +
  geom_line(aes(group = interaction(n2, stat)),
    size = 3, colour = "white", linetype = "solid", show_guide = T) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_log10("Number of bins",
    breaks = 10 ^ c(2, 3, 4, 5, 6), 
    labels = expression(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5, 10 ^ 6)) + 
  scale_y_log10("Time (s)",
    breaks = as.vector(c(0.5, 1) %o% 10 ^ c(-2, -1, 0, 1))
  ) + 
  scale_colour_brewer("Data size", 
    palette = "RdPu", 
    breaks = c("x6", "x7", "x8"),
    labels = expression(10 ^ 6, 10 ^ 7, 10 ^ 8)) +
  scale_linetype_manual(values = c("solid", "23", "42")) +
  guides(
    colour = guide_legend(reverse = T), 
    shape = guide_legend("Summary", reverse = T), 
    linetype = guide_legend("Summary", reverse = T)) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "line"),
    legend.position = c(1, 0),
    legend.justification = c("right", "bottom"))

ggsave("images/benchmark.pdf", width = 6, height = 4)
