library(tidyverse)
library(tidytext)

# http://www.glozman.com/textpages.html

books <- str_c("txt/", list.files("txt"))

words_in_book <- function(book, selected_words){
  
  hp <- read_lines(book)
  
  df <- data.frame(org_line=as.character(hp)) %>% 
    filter(str_length(org_line) > 0) %>% 
    mutate(book_name = book %>% 
             str_replace_all("txt/", "") %>% 
             str_replace_all(".txt", ""),
           line = org_line %>% 
             str_replace_all("[^[:alnum:] ]", " ")) %>% 
    unnest_tokens(output = words, input = line)
  
  houses <- df %>% 
    mutate(house=str_match(words, selected_words)) %>% 
    filter(!is.na(house))
  
  return(houses)
  
}

hp_houses <- map_df(books, words_in_book, selected_words = "gryffindor|hufflepuff|ravenclaw|slytherin")

save(hp_houses, file = "data/hp_houses.RData")