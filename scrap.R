library(rio)
library(tidyverse)
library(rcrossref)
library(bib2df)
library(rio)
library(tidyverse)
library(tictoc)
library(furrr)

## function to extract year and names of authors

extract_year_name <- function(data){
  data <- data %>% mutate(
    doi = str_extract(value, "(doi = \\{)(.)+"),
    doi = str_remove(doi, "doi = \\{"),
    doi = str_remove(doi, "\\},"),
    author = str_extract(value, "(author = \\{)(.)+"),
    author = str_remove(author, "author = \\{"),
    author = str_remove(author, "\\},"),
    year = str_extract(value, "(year =)(.)+"),
    year = str_remove(year, "year ="),
    year = str_remove(year, ","),
    )
}
  

## import csv
test <- read_csv("csv-TheNewEngl-set.csv") %>% 
  filter(!is.na(DOI))

## pull doi from csv
test_doi <- test %>%
  # sample_n(2) %>%
  pluck("DOI")

from1to500 <- test_doi[1:500] 

## Use DOI to extract bib info for each doi

tic()
sample <- future_map(from1to500, cr_cn)
toc()

from500to1000 <- test_doi[501:1000] 
tic()
sample2 <- future_map(from500to1000, cr_cn)
toc()

from1000to1500 <- test_doi[1001:1500] 
tic()
sample3 <- future_map(from1000to1500, cr_cn)
toc()

from1500to2000 <- test_doi[1501:2000] 

tic()
sample4 <- future_map(from1500to2000, cr_cn)
toc()

from2000to2500 <- test_doi[2001:2500] 

tic()
sample5 <- future_map(from2000to2500, cr_cn)
toc()

from2500to3000 <- test_doi[2501:3000] 

tic()
sample6 <- future_map(from2500to3000, cr_cn)
toc()

from3000to3500 <- test_doi[3001:3500] 

tic()
sample7 <- future_map(from3000to3500, cr_cn)
toc()

from3500to4000 <- test_doi[3501:4000] 

tic()
sample8 <- future_map(from2500to3000, cr_cn)
toc()

to_save <- list(sample,
                sample2,
                sample3,
                sample4,
                sample5,
                sample6,
                sample7,
                sample8)

saveRDS(to_save, file = "from1to4000.rds")

doi4000 <- test_doi[4000:length(test_doi)]
doi4000 <- c(4001:length(test_doi))

split_dois <- split(doi4000, sort(doi4000%%10))


sample1b <- test_doi[split_dois[[1]]]
sample1bib <- map(sample1b, cr_cn)

sample2b <- test_doi[split_dois[[2]]]

tic()
sample2bib <- map(sample2b, cr_cn)
toc()

sample3b <- test_doi[split_dois[[3]]]

tic()
sample3bib <- map(sample3b, cr_cn)
toc()

sample4b <- test_doi[split_dois[[4]]]

tic()
sample4bib <- map(sample4b, cr_cn)
toc()

sample5b <- test_doi[split_dois[[5]]]

tic()
sample5bib <- map(sample5b, cr_cn)
toc()

sample6b <- test_doi[split_dois[[6]]]

tic()
sample6bib <- map(sample6b, cr_cn)
toc()

sample7b <- test_doi[split_dois[[7]]]

tic()
sample7bib <- map(sample7b, cr_cn)
toc()

sample8b <- test_doi[split_dois[[8]]]

tic()
sample8bib <- map(sample8b, cr_cn)
toc()

sample9b <- test_doi[split_dois[[9]]]

tic()
sample9bib <- map(sample9b, cr_cn)
toc()

sample10b <- test_doi[split_dois[[10]]]

tic()
sample10bib <- map(sample10b, cr_cn)
toc()


save_to <- list(
  sample1bib, sample2bib, sample3bib, sample4bib, sample5bib,
  sample6bib, sample7bib, sample8bib, sample9bib, sample10bib)

saveRDS(save_to, "from40001toend.rds")
#

# clean lists -------------------------------------------------------------

part1 <- readRDS("from1to4000.rds")
part2 <- readRDS("from40001toend.rds")

part1and2 <- c(part1, part2)
a <- flatten_chr(map(part1and2, flatten_chr))

a <- as_tibble(a)
## Extract year and name from bib info

complete <- extract_year_name(a)

