library(tidyverse)
library(rcrossref)
library(tictoc)

## Import data
data_dir <- "01_data"

csv_files <- fs::dir_ls(data_dir)

data <- csv_files %>% 
  map_dfr(read_csv, .id = "source",
          col_types = cols(
            `Create Date` = col_character()))

data %>% 
  count(PMID, sort = TRUE) %>% 
  filter(n >1) %>% 
  tally() # check n_distict

data %>% distinct(PMID) %>% tally()

data <- data %>% 
  distinct(PMID, .keep_all = TRUE)

## Count how many papers per year and how many missing doi per year

data %>% 
  count(`Publication Year`) %>% 
  ggplot(aes(`Publication Year`, n)) +
  geom_line() +
  theme_minimal()

data %>% 
  group_by(`Publication Year`) %>% 
  count(is.na(DOI)) %>% 
  ungroup() %>% 
  ggplot(aes(`Publication Year`, n, color = `is.na(DOI)`)) +
  geom_line() +
  theme_minimal()


data_bib <- data %>% 
  filter(!is.na(DOI)) 

data_bib <- data_bib %>% 
  mutate(bib = cr_cn(DOI))

extract_bib <-
  function(data){
    tic()
    dt <- read_csv(data, col_types = cols(`Create Date` = col_character()))
    
    dt <- dt %>% 
      distinct(PMID, .keep_all = TRUE)
    
    dt <- dt %>% filter(!is.na(DOI))
    
    dt <- dt %>% 
      mutate(bib = cr_cn(DOI))

    file_name <- as.character(data)
    file_name <- str_replace(file_name, "01_data", "01b_clean_data")
    file_name <- str_replace(file_name, ".csv$", ".Rda")
    rio::export(dt, file_name)
    toc()
      }

map(csv_files, extract_bib)
