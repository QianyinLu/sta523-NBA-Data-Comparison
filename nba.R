library(jsonlite)
library(rjson)
library(tidyverse)

#function to transform list to dataframe
transform_list <- function(list, ...){
  vars <- Reduce(function(x, y){union(x, names(y))}, list, init = c()) ## obtain variable names
  clean_list <- lapply(list, function(x){
    x <- x[vars]
    names(x) <- vars
    x <- lapply(x, function(y){
      if (is.null(y)) {
        NA
      } else if (is.list(y)) {
        if (length(y) != 1) y <- list(y)
        I(y)
      } else {
        y
      }
    })
    as.data.frame(x, ...)
  })
  do.call(rbind, clean_list)
}

#player shooting request 
year <- 2015

if(!is.null(year)){
  if(year %% 1 != 0 | year <= 0 ){
    stop("Year must be positive integer.", call. = FALSE)
  }
}
if(is.null(year)){
  year <- 2019 #set default
}

base_url_shooting <- paste0("http://stats.nba.com/js/data/sportvu/", year, "/shootingData.json")

#curl_chinazi function 
.curl_chinazi <-
  function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=T&SeasonType=Regular%20Season&Sorter=DATE") {
    cookies = c(
      'AMCVS_7FF852E2556756057F000101%40AdobeOrg' = '1',
      's_vi' = '[CS]v1|2EE24D7A050788DB-4000011720008794[CE]',
      'AMCVS_248F210755B762187F000101%40AdobeOrg' = '1',
      's_ecid' = 'MCMID%7C25953548762710494002061391560771239414',
      'AMCV_7FF852E2556756057F000101%40AdobeOrg' = '2121618341%7CMCIDTS%7C18208%7CMCMID%7C25956765233563137992061161239589822345%7CMCAAMLH-1573770612%7C7%7CMCAAMB-1573770613%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1573173012s%7CNONE%7CMCAID%7C2EE24D7A050788DB-4000011720008794',
      'ug' = '5dc49af50145f20a3f95330016b22532',
      'ugs' = '1',
      '_ga' = 'GA1.2.933847444.1573165816',
      '_gid' = 'GA1.2.1733170427.1573165816',
      's_cc' = 'true',
      'AMCV_248F210755B762187F000101%40AdobeOrg' = '1585540135%7CMCIDTS%7C18208%7CMCMID%7C25953548762710494002061391560771239414%7CMCAAMLH-1573770615%7C7%7CMCAAMB-1573770615%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1573173015s%7CNONE%7CMCAID%7C2EE24D7A050788DB-4000011720008794%7CvVersion%7C4.4.0',
      '_gcl_au' = '1.1.1614741273.1573165817',
      '_fbp' = 'fb.1.1573165817535.300482637',
      '__gads' = 'ID=6abeeb7861dfac6c:T=1573165817:S=ALNI_MYFuFgzAgJ5hKGCW2indHcdSrFRzQ',
      'ak_bmsc' = '1ABD5CFE5EDE3100E66F0165BC453AE5B81975A443390000369EC45D58B3E633~plp2oHIsR2fj34/7me0V6ZARoA7pip8MRUg4trtmBEQyyRQFN1Sa9kN/ZaApLyUKikePPkCj2EwQALtFWkoMzaDnHRMD+dIvO/3Kr4hT3JJ25WZ8wvhDqOVrn9EwZBq2XOq45JtUHssOQ3Oc91MBGhBTV6yQ4qbT+5K9vvuyCihm48jLrDcoHdpwZCvtHd1a2uASC5iQfClMgHUqwacDyi9XjviEapNjg4bnFdk3zcv4o=',
      'bm_sv' = 'A27D2BDAE8DE323D591E34420B5876FF~aCnKvUaVDWkUBy2QOvq0NcJ9/qWIyMKc7Z5CzJtAiolEpgKKyUkGAXYvqRDjGlx35ibrE4DnVuitNVhHS8CppIu2AgBzTpmlC3R2q+wkxIu00hGOyLScw4yKhnEuVJOz5tu27H3k/cNc6tgLvjUdDg==',
      'gpv_pn' = 'stats%3Aplayer%3A202681',
      's_sq' = '%5B%5BB%5D%5D',
      's_tp' = '3328',
      's_ppv' = 'stats%253Aplayer%253A202681%2C100%2C20%2C3328',
      's_tps' = '1327',
      's_pvs' = '7593'
    )
    
    headers = c(
      `Connection` = 'close',
      `Pragma` = 'no-cache',
      `Cache-Control` = 'no-cache',
      `DNT` = '1',
      `Upgrade-Insecure-Requests` = '1',
      `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.29 Safari/537.36',
      `Sec-Fetch-User` = '?1',
      `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
      `Sec-Fetch-Site` = 'cross-site',
      `Sec-Fetch-Mode` = 'navigate',
      `Referer` = 'https://downwiththechinazis.com',
      `Accept-Encoding` = 'gzip, deflate, br',
      `Accept-Language` = 'en-US,en;q=0.9'
    )
    
    res <-
      httr::GET(url,
                httr::add_headers(.headers = headers),
                httr::set_cookies(.cookies = cookies))
    
    json <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(simplifyVector = T)
    
    json
    
  }
json <- .curl_chinazi()

jpg.list <- list(James.Heat <- c("http://www4.pictures.zimbio.com/gi/LeBron+James+Indiana+Pacers+v+Miami+Heat+Game+nL6-GL21Zp7l.jpg",
                            "https://media.npr.org/assets/img/2013/01/04/158756776_custom-a86c2b3186e8021d9361ba0426258870c64b67ea-s800-c85.jpg",
                            "https://media.npr.org/assets/img/2013/01/04/158756776_custom-a86c2b3186e8021d9361ba0426258870c64b67ea-s800-c85.jpg",
                            "https://statics.sportskeeda.com/wp-content/uploads/2014/05/489914833-2185397.jpg",
                            "https://www.gannett-cdn.com/media/USATODAY/USATODAY/2013/05/22/1369277574000-pacersheatgame1heatwin-1305222254_3_4.jpg?width=534&height=712&fit=crop"),
                 James.Lakers <- c("https://www.washingtonpost.com/resizer/jL_9MdgB_pkrWf2y6RWmeLFkcYs=/767x0/smart/arc-anglerfish-washpost-prod-washpost.s3.amazonaws.com/public/6PJF3QXATAI6TPT7JTEFAF6DN4.jpg",
                                   "https://cdn.images.express.co.uk/img/dynamic/4/590x/LeBron-James-net-worth-How-much-is-Lakers-star-worth-ahead-of-NBA-All-Star-Game-1088116.jpg?r=1550314144031",
                                   "https://specials-images.forbesimg.com/imageserve/5cfea6cc142c50000a32881f/416x416.jpg?background=000000&cropX1=1180&cropX2=3728&cropY1=256&cropY2=2805",
                                   "https://image-cdn.hypb.st/https%3A%2F%2Fhypebeast.com%2Fimage%2F2019%2F09%2Flebron-james-nike-lebron-17-2k-purple-first-look-teaser-1.jpg?q=75&w=800&cbr=1&fit=max",
                                   "5. https://cdn.images.express.co.uk/img/dynamic/4/590x/LeBron-James-1204417.jpg?r=1573741891243"),
                 James.Cavaliers <- c("https://cdn.britannica.com/s:700x500/82/212182-050-50D9F3CE/basketball-LeBron-James-Cleveland-Cavaliers-2018.jpg",
                                      "https://openskiesmagazine.com/wp-content/uploads/2017/09/Lebron-NEW-750x392.jpg",
                                      "https://s.abcnews.com/images/US/lebron-james-2-us-jt-180218_16x9_992.jpg",
                                      "http://cdn.gospelherald.com/data/images/full/17945/lebron-james.jpg",
                                      "https://www.billboard.com/files/styles/article_main_image/public/media/labron-james-cleveland-cavaliers-2014-billboard-650.jpg"),
                 Durant.Thunder <- c("https://www.gannett-cdn.com/media/USATODAY/USATODAY/2013/04/12/usp-nba_-oklahoma-city-thunder-at-golden-state-war-3_4.jpg?width=534&height=712&fit=crop",
                                     "https://cdn.britannica.com/s:700x500/88/181288-050-212FB60C/Kevin-Durant-NBA.jpg",
                                     "https://www.kosu.org/sites/kosu/files/styles/x_large/public/201410/durant_thunder.jpg",
                                     "http://www3.pictures.zimbio.com/gi/Kevin+Durant+Oklahoma+City+Thunder+v+New+York+6f5yyFNTv6Rl.jpg",           
                                     "https://static.rappler.com/images/kevin-durant-nba-20140130.jpg"),
                 Durant.Warriors <- c("https://02.imgmini.eastday.com/mobile/20180404/20180404171902_9ede9bf96fc333c156a5f3930482de1c_3.jpeg",
                                      "https://s.hdnux.com/photos/67/02/03/14426363/9/920x920.jpg",
                                      "https://images.complex.com/complex/images/c_limit,dpr_auto,q_90,w_720/fl_lossy,pg_1/xllde17yqfvpv4vgzw0n/kevin-durant",
                                      "https://static01.nyt.com/images/2017/04/07/sports/07durant-web1/07durant-web1-jumbo.jpg?quality=90&auto=webp",
                                      "https://cdn.hoopsrumors.com/files/2017/06/USATSI_10090886-900x1350.jpg"))
