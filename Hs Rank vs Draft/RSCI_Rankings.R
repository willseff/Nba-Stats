
rankings <- data.frame(RSCI = numeric(0),
                       Player = character(0),
                       Pk = numeric(0),
                       College = character(0))




url <- read_html("https://www.basketball-reference.com/awards/recruit_rankings_2020.html")
dat <- html_table(url, header=FALSE)[[1]]
colnames(dat) <- dat[2,]
dat <- dat %>% select(RSCI,Player,Pk, College) %>% tail(nrow(dat)-2)
dat <- dat %>% mutate(College = ifelse(College == "", "No College", College),
               Player = gsub("(college)","",Player, fixed = TRUE),
               RSCI = gsub("T","",RSCI)) %>%
  subset(RSCI != "RSCI" & RSCI != "")

rankings <- rbind(rankings,dat)
