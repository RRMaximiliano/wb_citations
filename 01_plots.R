
# library -----------------------------------------------------------------

library(hrbrthemes)
library(tidyverse)
library(wesanderson)

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


# Cites History -----------------------------------------------------------

cites_hist %>% 
  filter(main %in% c("Michael Woolcock", "Klaus Deininger", "David McKenzie", "Leora Klapper", "Sergio Schmukler")) %>% 
  ggplot(
    aes(x = year, y = cites, group = main, color = main)
  ) + 
  geom_line() +
  labs(
    x = "Year",
    y = "Citations",
    title = "Citation history of the Top 5 most cited authors at DECRG",
    color = ""
  ) + 
  scale_color_manual(values = wes_palette("Zissou1", 5)) + 
  theme_ipsum_rc() + 
  theme(
    legend.position = "top",
    legend.text = element_text(size = rel(1.1))
  )

ggsave(
  here::here("figs", "top_5_cites_history.png"),
  dpi = 320, height = 12, width = 20, scale = 0.7, bg = "white"
)
