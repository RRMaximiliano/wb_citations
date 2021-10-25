
# Load data ---------------------------------------------------------------
packages <- 
  c(
    "scholar",
    "tidyverse",
    "here"
    )

pacman::p_load(packages,
               character.only = TRUE)

onedrive <- "C:/Users/wb501238/OneDrive - WBG/DIME Analytics/DEC Publications"

# Read IDs ----------------------------------------------------------------
ids <- read_csv(here(onedrive, 
                     "data", 
                     "decrg_staff.csv")) %>%
  select(-title)

ids <- filter(ids, !is.na(scholar_id)) %>% 
  rename(main = full_name)

# Get data ----------------------------------------------------------------

df <- ids %>% 
  mutate(
    data = map(scholar_id, ~ get_publications(.))
  )

df_all <- df %>% 
  unnest(data) 

# Save data ---------------------------------------------------------------

write_rds(df_all, here(onedrive, "data", "citations.rds"))
write_csv(df_all, here(onedrive, "data", "citations.csv")) 
