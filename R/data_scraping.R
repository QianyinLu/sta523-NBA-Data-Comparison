# package
library(rvest)
library(tidyverse)
library(RSelenium)
library(httr)
library(jsonlite)
library(rjson)

# Regular game data

## LeBron James
base_url_R.J <- "https://www.basketball-reference.com/players/j/jamesle01/gamelog/"
urls_R.J <- str_c(base_url_R.J, c(2004:2020))
J.Reg_raw<- map(urls_R.J, function(url){
  Sys.sleep(rexp(1)+2)
  url%>%
    read_html()%>%
    html_table(fill = T)%>%
    .[[8]]
})
J.Reg <- do.call(rbind, J.Reg_raw)

## Kevin Durant
base_url_R.D <- "https://www.basketball-reference.com/players/d/duranke01/gamelog/"
urls_R.D <- str_c(base_url_R.D, c(2008:2019))
D.Reg_raw<- map(urls_R.D, function(url){
  Sys.sleep(rexp(1)+2)
  url%>%
    read_html()%>%
    html_table(fill = T)%>%
    .[[8]]
})
D.Reg <- do.call(rbind, D.Reg_raw)

# Shooting data

## Lebron James
base_url_S.J <- "https://www.basketball-reference.com/players/j/jamesle01/shooting/"
urls_S.J <- str_c(base_url_S.J,c(2004:2020))
J.Shoot_raw <- map(urls_S.J, function(url){
  Sys.sleep(rexp(1)+2)
  url%>%
    read_html()%>%
    html_table(fill = T)
})
J.Shoot <- do.call(rbind, do.call(rbind, J.Shoot_raw))

## Kevin Durant
base_url_S.D <- "https://www.basketball-reference.com/players/d/duranke01/shooting/"
urls_S.D <- str_c(base_url_S.D,c(2008:2019))
D.Shoot_raw <- map(urls_S.D, function(url){
  Sys.sleep(rexp(1)+2)
  url%>%
    read_html()%>%
    html_table(fill = T)
})
D.Shoot <- do.call(rbind, do.call(rbind, D.Shoot_raw))

# Playoff data

urls_P.J <- "https://www.basketball-reference.com/players/j/jamesle01/gamelog-playoffs/"
J.Playoff <- urls_P.J %>%
  read_html() %>%
  html_table(fill = T) %>% .[[8]]

urls_P.D <- "https://www.basketball-reference.com/players/d/duranke01/gamelog-playoffs/"
D.Playoff <- urls_P.D %>% 
  read_html() %>% 
  html_table(fill = T) %>% .[[8]]

# Season data

## Lebron James
J.season <- read_html("https://www.espn.com/nba/player/stats/_/id/1966/lebron-james")
column <- J.season %>% 
  html_nodes(css = ".flex+ .pt4 .Table__TH") %>% 
  html_text()

season <- J.season %>%
  html_nodes(css = ".flex+ .pt4 .Table--fixed-left .Table__TD:nth-child(1)") %>% 
  html_text() 
season <- season[-length(season)]  
team <- J.season %>% 
  html_nodes(css = ".flex+ .pt4 .pl2") %>% 
  html_text() 
data <- J.season %>% 
  html_nodes(".flex+ .pt4 .Table__Scroller .Table__TD") %>% 
  html_text() %>%
  matrix(ncol = length(column) - 2, byrow = TRUE)
data <- data[-length(season),]
J.ave <- as.data.frame(cbind(season, team, data),
                           stringsAsFactors = FALSE)     
names(J.ave) <- column

## Kevin Durant
D.season <- read_html("https://www.espn.com/nba/player/stats/_/id/3202/kevin-durant")
column <- D.season %>% 
  html_nodes(css = ".flex+ .pt4 .Table__TH") %>% 
  html_text()
season <- D.season %>% 
  html_nodes(css = ".flex+ .pt4 .Table--fixed-left .Table__TD:nth-child(1)") %>% 
  html_text() 
season <- season[-length(season)]  
team <- D.season %>% 
  html_nodes(css = ".flex+ .pt4 .pl2") %>% 
  html_text() 
team <- c("SEA", team)
data <- D.season %>% 
  html_nodes(".flex+ .pt4 .Table__Scroller .Table__TD") %>% 
  html_text() %>%
  matrix(ncol = length(column) - 2, byrow = TRUE)
data <- data[-length(season),]
D.ave <- as.data.frame(cbind(season, team, data),
                            stringsAsFactors = FALSE)     
names(D.ave) <- column

# save RDS
James <- list(J.Reg, J.Shoot, J.Playoff, J.ave)
Durant <- list(D.Reg, D.Shoot, D.Playoff, D.ave)
saveRDS(James, file = "data/james_raw.rds")
saveRDS(Durant, file = "data/durant_raw.rds")