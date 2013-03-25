if (!exists("delay", inherits = FALSE)) {
  delay <- readRDS("data/delay.rds")
}

if (!exists("dist", inherits = FALSE)) {
  dist <- readRDS("data/dist.rds")
  dist[dist > 2724] <- NA
}

if (!exists("time", inherits = FALSE)) {
  time <- readRDS("data/time.rds")
  time[time < 0] <- NA

  speed <- dist / time * 60
  speed[speed > 761] <- NA
}

