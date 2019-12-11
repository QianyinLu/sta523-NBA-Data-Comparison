suppressMessages(library(tidyverse))

# loading data
James <- readRDS("data/james_raw.rds")
Durant <- readRDS("data/durant_raw.rds")

J.shoot <- James$shoot
J.playoff <- James$playoff
J.avg <- James$avg
D.shoot <- Durant$shoot
D.playoff <- Durant$playoff
D.avg <- Durant$avg

# cleaning data
to.na <- function(string){
  ifelse(string %in% c("Not With Team", "Inactive", "Did Not Play", "Did Not Dress", ""),
         NA, string)
}

time.to.num <- function(string){
  ifelse(is.null(string), NULL,
         ifelse(substr(string, 2, 2) == ":",
                as.numeric(substr(string, 1, 1)) + as.numeric(substr(string, 3, 4))/60,
                as.numeric(substr(string, 1, 2)) + as.numeric(substr(string, 4, 5))/60))
}

## cleaning regular data
J.reg <- James$reg
colnames(J.reg)[c(6,8)] <- c("Home", "Res") # add colnames
J.reg <- J.reg %>% # redundant colnames
  filter(Date != "Date") %>%
  select(-Rk, -G, -Age) %>%
  mutate(Home = ifelse(Home == "@", "Away", "Home"))
J.reg[, 6:27] <- lapply(J.reg[, 6:27], to.na)
J.reg <- J.reg %>%
  mutate(Date = as.Date(Date), 
         Res.w = substr(Res, 1, 1),
         Res.diff = as.numeric(substr(str_remove_all(Res, " "), 3,
                                      str_length(str_remove_all(Res, " "))-1))) %>%
  select(-Res)
J.reg$MP <- sapply(J.reg$MP, time.to.num)
J.reg[, 5:26] <- lapply(J.reg[, 5:26], as.numeric)

D.reg <- Durant$reg
colnames(D.reg)[c(6,8)] <- c("Home", "Res") # add colnames
D.reg <- D.reg %>% # redundant colnames
  filter(Date != "Date") %>%
  select(-Rk, -G, -Age) %>%
  mutate(Home = ifelse(Home == "@", "Away", "Home"))
D.reg[, 6:27] <- lapply(D.reg[, 6:27], to.na)
D.reg <- D.reg %>%
  mutate(Date = as.Date(Date), 
         Res.w = substr(Res, 1, 1),
         Res.diff = as.numeric(substr(str_remove_all(Res, " "), 3,
                                      str_length(str_remove_all(Res, " "))-1))) %>%
  select(-Res)
D.reg$MP <- sapply(D.reg$MP, time.to.num)
D.reg[, 5:26] <- lapply(D.reg[, 5:26], as.numeric)

# saving data
James <- list(reg = J.reg, shoot = J.shoot, playoff = J.playoff, avg = J.avg)
Durant <- list(reg = D.reg, shoot = D.shoot, playoff = D.playoff, avg = D.avg)
saveRDS(James, file = "data/james.rds")
saveRDS(Durant, file = "data/durant.rds")