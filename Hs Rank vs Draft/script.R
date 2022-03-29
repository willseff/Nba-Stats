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

rankings$PLAYER <- gsub("Otto Porter", "Otto Porter Jr.", rankings$PLAYER)

##### Left join draftees with rankings #####

joined.data <- left_join(draftees,rankings, by = c("Player" = "PLAYER"))

# Get rid of duplicates
subset(data.frame(table(joined.data$Player) >1),table.joined.data.Player....1==TRUE)

joined.data <- joined.data[-c(209,77,558,647,678,339),]

joined.data <- joined.data %>% mutate(Pk = as.numeric(Pk),
                                      rank.diff = RK - Pk)

sum <- joined.data %>% group_by(SCHOOL) %>% summarise(sum = sum(rank.diff), count = n()) %>% drop_na()
sum <- sum %>% mutate(avg = sum/ count)
joined.data %>% subset(SCHOOL == "Boston College")

rankings %>% subset(SCHOOL == "Boston College")


##### Left join rankings with draftees #####

joined.data2 <- left_join(rankings,draftees, by = c("PLAYER" = "Player"))

joined.data2 <- joined.data2 %>% group_by(SCHOOL) %>% count(is.na(Pk)) 

colnames(joined.data2) <- c("School", "Drafted", "n")

top100draftoutcomes <- full_join(subset(joined.data2, Drafted == TRUE), 
                                 subset(joined.data2, Drafted == FALSE),
                                 by = c('School' = 'School')) %>%
  rename(Undrafted = n.x ,Drafted = n.y) %>% select(-c(Drafted.x, Drafted.y)) %>%
  replace_na(list(Drafted = 0)) %>% mutate(Percentage.Drafted = Drafted/(Drafted+Undrafted))


subset(joined.data, SCHOOL == "Georgetown")









