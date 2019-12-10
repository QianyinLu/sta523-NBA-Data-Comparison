James <- readRDS("data/james_raw.rds")
Durant <- readRDS("data/durant_raw.rds")

saveRDS(James, file = "data/james.rds")
saveRDS(Durant, file = "data/durant.rds")