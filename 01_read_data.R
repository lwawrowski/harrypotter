library(tidyverse)
library(tidytext)

# http://www.glozman.com/textpages.html

files <- str_c("txt/", list.files("txt"))

hp_houses <- function(book, selected_words){
  
  hp <- read_lines(book)
  
  df <- data.frame(org_line=as.character(hp)) %>% 
    filter(str_length(org_line) > 0) %>% 
    mutate(book_name = book %>% 
             str_replace_all("txt/", "") %>% 
             str_replace_all(".txt", ""),
           line = org_line %>% 
             str_replace_all("[^[:alnum:] ]", " ") %>% 
             str_squish()) %>% 
    unnest_tokens(output = words, input = line)
  
  houses <- df %>% 
    filter(words %in% selected_words)
  
  return(houses)
  
}

all_books <- map_df(files, hp_houses, selected_words = c("gryffindor", "hufflepuff", "ravenclaw", "slytherin"))

ggplot(all_books, aes(x = book_name, fill = words)) + 
  geom_bar(position = "dodge")

ggplot(all_books, aes(x = book_name, fill = words)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") + 
  scale_y_continuous(labels=scales::percent)
