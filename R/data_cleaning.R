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
J.reg.avg <- James$avg$regular[1:16,]
J.reg.avg$FGA <- sapply(J.reg.avg$FG, split2)
J.reg.avg$FG <- sapply(J.reg.avg$FG, split1)
J.reg.avg$`3P` <- sapply(J.reg.avg$`3PT`, split1)
J.reg.avg$`3PA` <- sapply(J.reg.avg$`3PT`, split2)
J.reg.avg$FTA <- sapply(J.reg.avg$FT, split2)
J.reg.avg$FT <- sapply(J.reg.avg$FT, split1)
J.reg.avg <- J.reg.avg[c("season", "Team", "GP", "GS", "MIN", "FG", "FGA", "FG%", "3P",
                 "3PA", "3P%", "FT", "FTA", "FT%", "OR", "DR", "REB", "AST", "BLK",
                 "STL", "PF", "TO","PTS")]
J.reg.avg[3:23] <- lapply(J.reg.avg[3:23], as.numeric)
J.reg.avg$season <- season.make(2003, 2018)
rownames(J.reg.avg) <- J.reg.avg$season
J.playoff.avg <- James$avg$playoff
J.playoff.avg$FGA <- sapply(J.playoff.avg$FG, split2)
J.playoff.avg$FG <- sapply(J.playoff.avg$FG, split1)
J.playoff.avg$`3P` <- sapply(J.playoff.avg$`3PT`, split1)
J.playoff.avg$`3PA` <- sapply(J.playoff.avg$`3PT`, split2)
J.playoff.avg$FTA <- sapply(J.playoff.avg$FT, split2)
J.playoff.avg$FT <- sapply(J.playoff.avg$FT, split1)
J.playoff.avg <- J.playoff.avg[c("season", "Team", "GP", "GS", "MIN", "FG", "FGA", "FG%", "3P",
                         "3PA", "3P%", "FT", "FTA", "FT%", "OR", "DR", "REB", "AST", "BLK",
                         "STL", "PF", "TO","PTS")]
J.playoff.avg[3:23] <- lapply(J.playoff.avg[3:23], as.numeric)
J.playoff.avg$season <- season.make(2005, 2017)
rownames(J.playoff.avg) <- J.playoff.avg$season
J.avg <- rbind(J.reg.avg, J.playoff.avg)
J.avg$regular <- rep(c("regular", "playoff"), c(16, 13))

D.reg.avg <- Durant$avg$regular
D.reg.avg$FGA <- sapply(D.reg.avg$FG, split2)
D.reg.avg$FG <- sapply(D.reg.avg$FG, split1)
D.reg.avg$`3P` <- sapply(D.reg.avg$`3PT`, split1)
D.reg.avg$`3PA` <- sapply(D.reg.avg$`3PT`, split2)
D.reg.avg$FTA <- sapply(D.reg.avg$FT, split2)
D.reg.avg$FT <- sapply(D.reg.avg$FT, split1)
D.reg.avg <- D.reg.avg[c("season", "Team", "GP", "GS", "MIN", "FG", "FGA", "FG%", "3P",
                         "3PA", "3P%", "FT", "FTA", "FT%", "OR", "DR", "REB", "AST", "BLK",
                         "STL", "PF", "TO","PTS")]
D.reg.avg[3:23] <- lapply(D.reg.avg[3:23], as.numeric)
D.reg.avg$season <- season.make(2007, 2018)
rownames(D.reg.avg) <- D.reg.avg$season
D.playoff.avg <- Durant$avg$playoff
D.playoff.avg$FGA <- sapply(D.playoff.avg$FG, split2)
D.playoff.avg$FG <- sapply(D.playoff.avg$FG, split1)
D.playoff.avg$`3P` <- sapply(D.playoff.avg$`3PT`, split1)
D.playoff.avg$`3PA` <- sapply(D.playoff.avg$`3PT`, split2)
D.playoff.avg$FTA <- sapply(D.playoff.avg$FT, split2)
D.playoff.avg$FT <- sapply(D.playoff.avg$FT, split1)
D.playoff.avg <- D.playoff.avg[c("season", "Team", "GP", "GS", "MIN", "FG", "FGA", "FG%", "3P",
                                 "3PA", "3P%", "FT", "FTA", "FT%", "OR", "DR", "REB", "AST", "BLK",
                                 "STL", "PF", "TO","PTS")]
D.playoff.avg[3:23] <- lapply(D.playoff.avg[3:23], as.numeric)
D.playoff.avg$season <- c(season.make(2009, 2013), season.make(2015, 2018))
rownames(D.playoff.avg) <- D.playoff.avg$season
D.avg <- rbind(D.reg.avg, D.playoff.avg)
D.avg$regular <- rep(c("regular", "playoff"), c(12, 9))

## cleaning shooting data
J.shoot <- James$shoot
J.shoot <- lapply(J.shoot, shooting.clean)
names(J.shoot) <- season.make(2003, 2018)
J.shoot <- lapply(J.shoot, function(y){
  split <- unique(y$Split)
  split <- split[split != ""&split != "Season"]
  new_list <- lapply(1:length(split), function(x){
    if(x == length(split)){
      row <- as.integer(rownames(y[y$Split == split[x],]))
      y[row:nrow(y), 2:ncol(y)]
    }
    else{
      start <- as.integer(rownames(y[y$Split == split[x],]))
      end <- as.integer(rownames(y[y$Split == split[x+1],]))
      y[start:(end-1), 2:ncol(y)]
    }
  })
  names(new_list) <- split
  return(new_list)
})
vars <- Reduce(function(x, y){union(x, names(y))}, J.shoot, init = c())
J.shoot <- lapply(vars, function(x){
  reorder <- lapply(J.shoot, function(y){
    y[[x]]
  })
})
names(J.shoot) <- vars
J.shoot <- lapply(J.shoot, function(x){
  n <- sapply(x, nrow)
  names <- names(x)
  df <- do.call(rbind, x)
  df$season <- rep(names, n)
  rownames(df) <- 1:nrow(df)
  return(df[,c(ncol(df), 1:(ncol(df)-1))])
})

D.shoot <- Durant$shoot
D.shoot <- lapply(D.shoot, shooting.clean)
names(D.shoot) <- season.make(2007, 2018)
D.shoot <- lapply(D.shoot, function(y){
  split <- unique(y$Split)
  split <- split[split != ""&split != "Season"]
  new_list <- lapply(1:length(split), function(x){
    if(x == length(split)){
      row <- as.integer(rownames(y[y$Split == split[x],]))
      y[row:nrow(y), 2:ncol(y)]
    }
    else{
      start <- as.integer(rownames(y[y$Split == split[x],]))
      end <- as.integer(rownames(y[y$Split == split[x+1],]))
      y[start:(end-1), 2:ncol(y)]
    }
  })
  names(new_list) <- split
  return(new_list)
})
vars <- Reduce(function(x, y){union(x, names(y))}, D.shoot, init = c())
D.shoot <- lapply(vars, function(x){
  reorder <- lapply(D.shoot, function(y){
    y[[x]]
  })
})
names(D.shoot) <- vars
D.shoot <- lapply(D.shoot, function(x){
  n <- sapply(x, nrow)
  names <- names(x)
  df <- do.call(rbind, x)
  df$season <- rep(names, n)
  rownames(df) <- 1:nrow(df)
  return(df[,c(ncol(df), 1:(ncol(df)-1))])
})

## getting record of face-to-face match between J and D
DR <- cbind(player = "Durant", D.reg)
JR <- cbind(player = "James", J.reg)
innR <- DR %>% 
  inner_join(JR, by = "Date") 
JvsD_R <- innR %>% 
  filter(Tm.x == Opp.y) %>% 
  filter(!is.na(GS.y)&!is.na(GS.x))
date_R <- JvsD_R[,'Date']
JD_R <-rbind( DR%>% filter(Date %in% date_R), JR%>% filter(Date %in% date_R)) %>%
  arrange(Date)
DP <- cbind(player = "Durant", D.playoff)
JP <- cbind(player = "James", J.playoff)
innP <- DP %>% inner_join(JP, by = "Date") 
JvsD_P <- innP %>% filter(Tm.x == Opp.y) %>% filter(!is.na(GS.y)&!is.na(GS.x))
date_P <- JvsD_P[,'Date']
JD_P <-rbind( DP%>% filter(Date %in% date_P), JP%>% filter(Date %in% date_P)) %>% arrange(Date)

# saving data
James <- list(avg = J.avg, shoot = J.shoot)
Durant <- list(avg = D.avg, shoot = D.shoot)
direct <- list(regular = JD_R, playoff = JD_P)
saveRDS(James, file = "data/james.rds")
saveRDS(Durant, file = "data/durant.rds")
saveRDS(direct, file = "data/direct.rds")