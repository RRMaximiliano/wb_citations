
# library -----------------------------------------------------------------

library(hrbrthemes)
library(tidyverse)

# Plot --------------------------------------------------------------------
df_all <- read_rds("data/citations.rds")

df_all %>% 
  group_by(main) %>% 
  summarise(
    sum = sum(cites, na.rm = TRUE) 
  ) %>% 
  arrange(desc(sum)) %>% 
  head(20) %>% 
  mutate(
    main = fct_reorder(main, sum)
  ) %>% 
  ggplot(
    aes(x = sum, y = main)
  ) +
  geom_col(color = "black") +
  scale_x_continuous(labels = scales::comma) + 
  labs(
    x = "Total citations", 
    y = "", 
    title = "Top 20 authors most cited at DECRG"
  ) + 
  theme_ipsum_rc()


ggsave(
  here::here("figs", "top_20.png"),
  dpi = 320, height = 12, width = 20, scale = 0.7, bg = "white"
)


df_all %>% 
  group_by(team) %>% 
  summarize(
    sum = sum(cites, na.rm = TRUE)
  ) %>% 
  mutate(
    team = fct_reorder(team, sum)
  ) %>% 
  ggplot(
    aes(x = sum, y = team)
  ) +
  geom_col(color = "black") +
  scale_x_continuous(labels = scales::comma) + 
  labs(
    x = "Total citations", 
    y = "", 
    title = "Total authors' citations by team at DECRG"
  ) + 
  theme_ipsum_rc()

ggsave(
  here::here("figs", "team_citations.png"),
  dpi = 320, height = 12, width = 20, scale = 0.7, bg = "white"
)

