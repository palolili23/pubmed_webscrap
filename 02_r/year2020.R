library(lubridate)

year2020 <- rio::import("./01b_clean_data/nejm/data_filtered_nejm.Rda") %>% 
  filter(`Publication Year` == 2020) %>% 
  mutate(`Create Date` == as_date(`Create Date`)) %>% 
  filter(`Create Date` >= as_date("2020/04/01")) %>% 
  mutate(month = month(`Create Date`)) %>% 
  mutate(month = ifelse(as.numeric(month) == 4, "April", "May"))

count_gender <- year2020 %>% 
  group_by(month) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2)) %>% 
  ungroup()

count_gender %>% 
  ggplot(aes(month, prop, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c("#FFC857", "#BDD9BF", "#929084")) +
  theme_minimal() +
  labs(
    title = "NEJM, gender proportion in April, May/2020",
    y = "Proportion (%)%",
    x = "Month",
    fill = "Gender") +
  theme(legend.position = "bottom")


count_gender_first <- year2020 %>% 
  group_by(DOI) %>% 
  mutate(total_authors = last(author_position)) %>%
  ungroup() %>% 
  filter(total_authors != 1) %>% 
  filter(author_position == 1) %>% 
  group_by(month) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2)) %>% 
  ungroup()

count_gender_first %>% 
  ggplot(aes(month, prop, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c("#FFC857", "#BDD9BF", "#929084")) +
  theme_minimal() +
  labs(
    title = "NEJM, first author's gender proportion in April, May/2020",
    y = "Proportion (%)%",
    x = "Month",
    fill = "Gender") +
  theme(legend.position = "bottom")


count_single <- year2020 %>% 
  group_by(DOI) %>% 
  mutate(total_authors = last(author_position)) %>%
  ungroup() %>% 
  filter(total_authors == 1) %>% 
  group_by(month) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2)) %>% 
  ungroup()

count_single %>% 
  ggplot(aes(month, prop, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c("#FFC857", "#BDD9BF", "#929084")) +
  theme_minimal() +
  labs(
    title = "NEJM, single author's gender proportion in April, May/2020",
    y = "Proportion (%)%",
    x = "Month",
    fill = "Gender") +
  theme(legend.position = "bottom")


count_last<- year2020 %>% 
  group_by(DOI) %>% 
  mutate(total_authors = last(author_position)) %>%
  ungroup() %>% 
  filter(total_authors != 1) %>% 
  group_by(DOI) %>% 
  filter(author_position == last(author_position)) %>%
  ungroup() %>% 
  group_by(month) %>% 
  count(gender) %>% 
  mutate(prop = round(100*n/sum(n), 2)) %>% 
  ungroup()

count_last %>% 
  ggplot(aes(month, prop, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c("#FFC857", "#BDD9BF", "#929084")) +
  theme_minimal() +
  labs(
    title = "NEJM, last author's gender proportion in April, May/2020",
    y = "Proportion (%)%",
    x = "Month",
    fill = "Gender") +
  theme(legend.position = "bottom")


year2020 %>% group_by(DOI) %>% tally()
