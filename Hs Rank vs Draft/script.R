library(dplyr)
library(rvest)

##### Scrape Draftees #####
url <- read_html("https://www.basketball-reference.com/draft/NBA_2008.html")
dat <- html_table(url, header=FALSE)
dat <- data.frame(dat)
colnames(dat) <- dat[2,]
dat <- dat[2:5] %>% tail(nrow(dat)-2)
dat <- dat %>% subset(!dat["Player"] == "Player" & !dat["Player"] == "Round 2")
draftees <- dat

for (i in 2009:2021){
  url <- read_html(paste0("https://www.basketball-reference.com/draft/NBA_",i, ".html"))
  dat <- html_table(url, header=FALSE)
  dat <- data.frame(dat)
  colnames(dat) <- dat[2,]
  dat <- dat[2:5] %>% tail(nrow(dat)-2)
  dat <- dat %>% subset(!dat["Player"] == "Player" & !dat["Player"] == "Round 2")
  draftees <- rbind(draftees, dat)
  
} 


##### Scrape HS Rankings #####

rankings <- data.frame(RK = numeric(0),PLAYER = character(0),SCHOOL = character(0))

for (i in 2007:2021) {
  print(paste0("http://www.espn.com/college-sports/basketball/recruiting/playerrankings/_/class/" , i , "/order/true"))
  url <- read_html(paste0("http://www.espn.com/college-sports/basketball/recruiting/playerrankings/_/class/" , i , "/order/true"))
  dat <- html_table(url, header = TRUE)
  dat <- as.data.frame(dat) %>% select(RK, PLAYER, SCHOOL)
  dat <- dat %>% mutate(PLAYER = substr(PLAYER, 0,nchar(PLAYER)-21),
                        SCHOOL = str_remove(SCHOOL,"Signed"),
                        SCHOOL = gsub("\\Committed(.*)","",SCHOOL ))
  rankings <- rankings %>% rbind(dat)
  
}


joined.data <- left_join(draftees,rankings, by = c("Player" = "PLAYER"))

joined.data %>% mutate(Pk = as.numeric(Pk),
                       rank.diff = RK - Pk)

subset(data.frame(table(joined.data$Player) >1),table.joined.data.Player....1==TRUE)




