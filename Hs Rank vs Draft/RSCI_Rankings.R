library(dplyr)
library(ggplot2)


rankings <- data.frame(RSCI = numeric(0),
                       Player = character(0),
                       Pk = numeric(0),
                       College = character(0))


for(i in 2007:2020){
  
  url <- read_html(paste0("https://www.basketball-reference.com/awards/recruit_rankings_", i,".html"))
  dat <- html_table(url, header=FALSE)[[1]]
  colnames(dat) <- dat[2,]
  dat <- dat %>% select(RSCI,Player,Pk, College) %>% tail(nrow(dat)-2)
  dat <- dat %>% mutate(College = ifelse(College == "", "No College", College),
                        Player = gsub("(college)","",Player, fixed = TRUE),
                        RSCI = as.numeric(gsub("T","",RSCI)),
                        Pk = as.numeric(Pk)) %>%
    subset(RSCI != "RSCI" & RSCI != "")
  
  rankings <- rbind(rankings,dat)
}

avg.draft.pos <- rankings %>% group_by(RSCI) %>% summarise(average = mean(Pk, na.rm = TRUE))

rankings.drafted <- rankings %>% mutate(drafted = !is.na(Pk))

rankings.drafted.grouped <- rankings.drafted %>% 
  group_by(RSCI) %>% 
  summarise(average.draft.spot = mean(Pk, na.rm = TRUE), 
            number.drafted = sum(drafted), 
            total = n()) %>%
  mutate(draft.rate = number.drafted/total)


# percent drafted by college

drafted.rate.college <- rankings.drafted %>% 
  group_by(College) %>% 
  summarise(draft.rate = sum(drafted)/n(), n = n())

# weighted draft score

draft.joined <- rankings.drafted.grouped %>% 
  select(RSCI,draft.rate) %>% 
  right_join(rankings.drafted,
             by = c("RSCI" = "RSCI")) %>% mutate(weighted.draft.score = drafted-draft.rate)

# weighted draft score by college

draft.score <- draft.joined %>% 
  group_by(College) %>% 
  summarise(draft.score.total = sum(weighted.draft.score),
            n = n()) %>% mutate(draft.score.avg = draft.score.total/n)

draft.score.subset <- draft.score %>% subset(n>10)


# 

rankings.drafted.grouped




