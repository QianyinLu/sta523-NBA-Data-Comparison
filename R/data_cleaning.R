suppressMessages(library(tidyverse))

# loading data
James <- readRDS("data/james_raw.rds")
Durant <- readRDS("data/durant_raw.rds")

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

split1 <- function(string){
  str_split(string, "-")[[1]][1]
}

split2 <- function(string){
  str_split(string, "-")[[1]][2]
}

shooting.clean <- function(df){
  df <- df %>%
    filter(Value != "Value")
  df[3:11] <- lapply(df[3:11], to.na)
  df[3:11] <- lapply(df[3:11], as.numeric)
  return(df)
}

season.make <- function(start, end = NULL){
  if(is.null(end)){
    end = start
  }
  sapply(start:end, function(x){
    paste0(x, "-", x + 1)
  })
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

## cleaning playoff data
J.playoff <- James$playoff
colnames(J.playoff)[c(3,6,8,9)] <- c("Date","Home", "Game", "Res")
J.playoff <- J.playoff %>%
  filter(Rk != "" & Rk != "Rk") %>%
  mutate(Home = ifelse(Home == "@", "Away", "Home"),
         Date = as.Date(Date), 
         Res.w = substr(Res, 1, 1),
         Res.diff = as.numeric(substr(str_remove_all(Res, " "), 3,
                                      str_length(str_remove_all(Res, " "))-1))) %>%
  select(-Rk, -G, -Res)
J.playoff[7:30] <- lapply(J.playoff[7:30], to.na)
J.playoff$MP <- sapply(J.playoff$MP, time.to.num)
J.playoff[6:28] <- lapply(J.playoff[6:28], as.numeric)

D.playoff <- Durant$playoff
colnames(D.playoff)[c(3,6,8,9)] <- c("Date","Home", "Game", "Res")
D.playoff[32] <- NULL
D.playoff <- D.playoff %>%
  filter(Rk != "" & Rk != "Rk") %>%
  mutate(Home = ifelse(Home == "@", "Away", "Home"),
         Date = as.Date(Date), 
         Res.w = substr(Res, 1, 1),
         Res.diff = as.numeric(substr(str_remove_all(Res, " "), 3,
                                      str_length(str_remove_all(Res, " "))-1))) %>%
  select(-Rk, -G, -Res)
D.playoff[7:30] <- lapply(D.playoff[7:30], to.na)
D.playoff$MP <- sapply(D.playoff$MP, time.to.num)
D.playoff[6:28] <- lapply(D.playoff[6:28], as.numeric)

## cleaning average data
J.avg <- James$avg[1:16,]
J.avg$FGA <- sapply(J.avg$FG, split2)
J.avg$FG <- sapply(J.avg$FG, split1)
J.avg$`3P` <- sapply(J.avg$`3PT`, split1)
J.avg$`3PA` <- sapply(J.avg$`3PT`, split2)
J.avg$FTA <- sapply(J.avg$FT, split2)
J.avg$FT <- sapply(J.avg$FT, split1)
J.avg <- J.avg[c("season", "Team", "GP", "GS", "MIN", "FG", "FGA", "FG%", "3P",
                 "3PA", "3P%", "FT", "FTA", "FT%", "OR", "DR", "REB", "AST", "BLK",
                 "STL", "PF", "TO","PTS")]
J.avg[3:23] <- lapply(J.avg[3:23], as.numeric)
J.avg$season <- season.make(2003, 2018)
rownames(J.avg) <- J.avg$season

D.avg <- Durant$avg
D.avg$FGA <- sapply(D.avg$FG, split2)
D.avg$FG <- sapply(D.avg$FG, split1)
D.avg$`3P` <- sapply(D.avg$`3PT`, split1)
D.avg$`3PA` <- sapply(D.avg$`3PT`, split2)
D.avg$FTA <- sapply(D.avg$FT, split2)
D.avg$FT <- sapply(D.avg$FT, split1)
D.avg <- D.avg[c("season", "Team", "GP", "GS", "MIN", "FG", "FGA", "FG%", "3P",
                 "3PA", "3P%", "FT", "FTA", "FT%", "OR", "DR", "REB", "AST", "BLK",
                 "STL", "PF", "TO","PTS")]
D.avg[3:23] <- lapply(D.avg[3:23], as.numeric)
D.avg$season <- season.make(2007, 2018)
rownames(D.avg) <- D.avg$season

## cleaning shooting data
J.shoot <- James$shoot
J.shoot <- lapply(J.shoot, shooting.clean)
names(J.shoot) <- season.make(2003, 2018)

D.shoot <- Durant$shoot
D.shoot <- lapply(D.shoot, shooting.clean)
names(D.shoot) <- season.make(2007, 2018)

# saving data
James <- list(reg = J.reg, shoot = J.shoot, playoff = J.playoff, avg = J.avg)
Durant <- list(reg = D.reg, shoot = D.shoot, playoff = D.playoff, avg = D.avg)
saveRDS(James, file = "data/james.rds")
saveRDS(Durant, file = "data/durant.rds")