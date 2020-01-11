library(tidyverse)

load(hp_houses)

hp_houses <- hp_houses %>% 
  mutate(book=str_sub(book_name, 18, str_length(book_name)))

ggplot(all_books, aes(x = book_name, fill = house)) + 
  geom_bar(position = "dodge") +
  coord_flip()

all_books %>% 
  group_by(book_name) %>%
  count(house) %>% 
  mutate(percent=n/sum(n)) %>% 
  ggplot(aes(x = book_name, y = percent, fill = house)) + 
  geom_col() + 
  scale_y_continuous(labels=scales::percent)

# number of pages in books