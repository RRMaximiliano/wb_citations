
# Set up ---------------------------------------------------------------
packages <- 
  c(
    "scholar",
    "tidyverse",
    "here",
    "fuzzyjoin"
  )

pacman::p_load(packages,
               character.only = TRUE)

onedrive <- "C:/Users/wb501238/OneDrive - WBG/DIME Analytics/DEC Publications"

# Load data ---------------------------------------------------------------

okr <-
  read_rds(here(onedrive, "data", "wb_clean.rds"))

rg <-
  read_csv(here(onedrive, "data", "decrg_staff.csv"))

scholar <-
  read_csv(here(onedrive, "data", "citations.csv")) %>%
  rename(full_name = main,
         all_authors = author)

# Process OKR data ------------------------------------------------------------

max_authors <-
  okr %>%
  mutate(n_authors = str_count(authors, ";") + 1) %>%
  summarise(max = max(n_authors)) %>%
  unlist

okr_papers <-
  okr %>%
  mutate(all_authors = authors) %>%
  separate(authors, paste0("author", 1:max_authors), sep = ";") %>%
  pivot_longer(cols = starts_with("author"),
               names_to = "no",
               values_to = "author") %>%
  filter(!is.na(author))

okr_authors <-
  okr_papers %>%
  select(author) %>%
  mutate(clean_name = 
           str_remove(author, ":") %>%
           str_replace("//[.*]", "") %>%
           str_replace("editor", "") %>%
           str_replace(":.*", "") %>%
           str_trim) %>%
  unique %>%
  mutate(first_name = str_replace(clean_name, ".*,", "") %>%
           str_trim,
         last_name = str_replace(clean_name, ",.*", "") %>%
           str_trim) %>%
  mutate(full_name = ifelse(clean_name == first_name,
                       clean_name,
                       paste(first_name, last_name))) %>%
  select(full_name, author)

rg_authors <-
  stringdist_left_join(
    rg, okr_authors, 
    by = "full_name",
    ignore_case = TRUE, 
    method = "jw", 
    max_dist = .15, 
    distance_col = "dist") %>%
  select(upi, author, full_name.x) %>%
  rename(full_name = full_name.x)

rg_okr <-
  okr_papers %>%
  filter(author %in% rg_authors$author | str_detect(origu, "DECRG")) %>%
  left_join(rg_authors) %>%
  select(upi,
         full_name,
         all_authors,
         id,
         title,
         download_count,
         year,
         contains("topic"),
         pdfurl) %>%
  rename(author = full_name,
         okr_id = id) %>%
  filter(!is.na(upi))

# Process scholar data ------------------------------------------------------------

papers <- 
  stringdist_full_join(
    scholar, rg_okr, 
    by = c("upi", "title"),
    ignore_case = TRUE, 
    method = "jw", 
    max_dist = .2, 
    distance_col = "dist") %>%
  mutate(title = coalesce(title.x, title.y),
         all_authors = coalesce(all_authors.x, all_authors.y))

# Individual papers downloads and citations
paper_indicators <-
  papers %>%
  select(title,
         all_authors,
         year.y,
         download_count,
         year.x,
         journal, 
         cites) %>%
  filter(year.x >= (2021 - 5) | year.y >= (2021 - 5))%>%
  mutate(cites = ifelse(is.na(year.x) & cites == 0 | 
                          is.na(journal) & cites == 0 ,
                        NA,
                        is.na(ye%>%
  rename(Title = title,_authors, year.y,ds` = download_count
  mutate(cites = ifels
  mutate(cites = ifels
  mutate(cites = ifels,
         `Google scholar year` = year.x,
         `Google scholar journal` = journal,
         `Google scholar citations` = cites) %>%
  unique %>%
  arrange(`Google scholar year`,
          Title) 

write_csv(paper_indicators,
          here(onedrive, "output", "Papers.csv"))

# By author
researchers_scholar <-
  papers %>%
  group_by(upi.x) %>%
  mutate(full_name = first(full_name)) %>%
  filter(year.x >= (2021 - 5)) %>%
  group_by(year.x, upi.x) %>%
  summarise(Citations = sum(cites, na.rm = TRUE),
            full_name = first(full_name)) %>%
  rename(Year = year.x,
         UPI = upi.x)

researchers_okr <-
  papers %>%
  group_by(upi.y) %>%
  mutate(full_name = first(full_name)) %>%
  group_by(year.y, upi.y) %>%
  filter(year.y >= (2021 - 5),
         !is.na(full_name)) %>%
  summarise(`PRWP downloads` = sum(download_count, na.rm = TRUE),
            full_name = first(full_name)) %>%
  rename(Year = year.y,
         UPI = upi.y,
         Name = full_name)


researcher_indicators <-
  researchers_okr %>%
  full_join(researchers_scholar) %>%
  select(UPI, Name, Year, `PRWP downloads`, Citations) %>%
  arrange(UPI, Year)

write_csv(researcher_indicators,
          here(onedrive, "output", "Researchers.csv"))
