library(dplyr)
library(rvest)

dat <- read_html('https://stathead.com/basketball/psl_finder.cgi?request=1&height_max=99&lg_id=NBA&order_by=ws&match=single&season_start=1&is_playoffs=N&as_val=0&order_by_asc=0&as_comp=gt&season_end=-1&height_min=0&age_min=26&locationMatch=is&age_max=27&type=per_game&offset=0') %>% html_table(fill=TRUE)


dat[1][1]
