library(tidyverse)

# loading the data --------------------------------------------------------

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')


# exploration -------------------------------------------------------------

poll_2022 <- poll %>% select(-c(rank, rq, year)) %>%
  distinct() %>% mutate( year = 2022) %>%
  rename(rank = `2022_rank`, rq = `2022_rq` )

poll_2022

poll_clean <- poll %>% select(-c(`2022_rank`,`2022_rq`)) %>% 
  bind_rows(., poll_2022) %>% arrange(company,  year ) 

 poll_clean %>% group_by(industry = fct_lump_n(industry, 8), year)%>% 
  summarise(mean_industry_rank = mean(rank, na.rm = T),
            mean_industry_rq = mean(rq, na.rm = T)) %>%
  
  ggplot(aes(year, mean_industry_rq,  color = industry))  +
  theme_bw() +
  geom_line() +
  scale_color_viridis_b(discrete = T) +
  labs(x = "Year",
       y = "Mean RQ score for each industry",
       color = "Industry")
