
# Load data ---------------------------------------------------------------

library(scholar)
library(tidyverse)

# Read IDs ----------------------------------------------------------------
ids <- read_csv("data/authors.csv")
ids <- filter(ids, !is.na(id)) %>% 
  rename(main = author)

# Get data ----------------------------------------------------------------

df <- ids %>% 
  mutate(
    data = map(id, ~ get_publications(.))
  )

df_all <- df %>% 
  unnest(data) 

# Save data ---------------------------------------------------------------

write_rds(df_all, "data/citations.rds")
write_csv(df_all, "data/citations.csv")  
