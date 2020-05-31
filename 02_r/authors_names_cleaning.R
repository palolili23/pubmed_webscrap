library(tidyverse)
library(ggsci)

# Import files with bib info ----------------------------------------------

data_dir <- "01b_clean_data"

rda_files <- fs::dir_ls(data_dir, regexp = "\\.Rda$")

data <- rda_files %>% 
  map_dfr(rio::import)

data %>% 
  count(PMID, sort = TRUE) %>% 
  filter(n >1) %>% 
  tally() # check n_distict


data %>% distinct(PMID) %>% tally()

data <- data %>% 
  distinct(PMID, .keep_all = TRUE)


error <- data %>%
  filter(str_detect(bib, "error"))


# Extract authors name in multiple columns --------------------------------

data <- data %>% mutate(
  author = str_extract(bib, "(author = \\{)(.)+"),
  author = str_remove(author, "author = \\{"),
  author = str_remove(author, "\\},")
)


split_into_multiple <- function(column, pattern = ", ", into_prefix){
  cols <- str_split_fixed(column, pattern, n = Inf)
  cols[which(cols == "")] <- NA
  cols <- as_tibble(cols)
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}

data_authors <- data %>% 
  bind_cols(split_into_multiple(data$author, " and ", "author")) %>% 
  pivot_longer(
    cols = starts_with("author_"),
    names_to = "author_position",
    values_to = "author_name") %>% 
  drop_na(author_name)

# Some authors only have initials, these will be excluded
# Separate first and last name

data_authors <- data_authors %>% 
  mutate(author_clean = str_remove_all(author_name, "[:upper:][:punct:]"),
         author_clean = str_trim(author_clean, side = "both")) %>% 
  separate(author_clean, into = c("first_name", "last_name"), fill = "left")

data_filtered <- data_authors %>% 
  drop_na(first_name)


# Find gender for name ----------------------------------------------------

names <- data_filtered %>% count(first_name) %>% pull(first_name)

gender <- gender::gender(names, method = "ssa")

data_filtered <- data_filtered %>% 
  left_join(gender, by = c("first_name" = "name")) %>% 
  select(-starts_with("proportion"),
         -starts_with("year"))

count_gender <- data_filtered %>% 
  group_by(`Publication Year`) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2))

count_gender %>% 
  mutate(
    gender = ifelse(is.na(gender), "unknown", gender), 
    gender = str_to_title(gender)) %>% 
  ggplot(aes(`Publication Year`, prop, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c("#A8E6CE", "#DCEDC2", "#FFD3B5" )) +
  theme_minimal() +
  labs(
    title = "NEJM, author's gender proportion",
    y = "Proportion (%)%",
    fill = "Gender")

  
count_gender_first <- data_filtered %>% 
  filter(author_position == "author_1") %>% 
  group_by(`Publication Year`) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2))

count_gender_first %>% 
  mutate(
    gender = ifelse(is.na(gender), "unknown", gender), 
    gender = str_to_title(gender)) %>% 
  ggplot(aes(`Publication Year`, prop, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c("#A8E6CE", "#DCEDC2", "#FFD3B5" )) +
  theme_minimal() +
  labs(
    title = "NEJM, first author's gender proportion",
    y = "Proportion (%)%",
    fill = "Gender")

count_single <- data_filtered %>% 
  group_by(DOI) %>% 
  mutate(total_authors = last(author_position)) %>%
  ungroup() %>% 
  filter(total_authors == 1) %>% 
  group_by(`Publication Year`) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2))

count_last<- data_filtered %>% 
  group_by(DOI) %>% 
  filter(author_position == last(author_position)) %>%
  ungroup() %>% 
  group_by(`Publication Year`) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2))