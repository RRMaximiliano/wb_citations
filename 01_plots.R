
library(hrbrthemes)

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
