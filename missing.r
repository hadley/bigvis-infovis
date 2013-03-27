delay <- readRDS("data/delay.rds")
dist <- readRDS("data/dist.rds")
time <- readRDS("data/time.rds")

speed <- dist / time * 60

bad_speed <- speed < 0 | speed > 761
bad_time <- time < 0
bad_dist <- dist > 2724

table(bad_speed + bad_time + bad_time)
