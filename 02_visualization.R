library(tidyverse)

# https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

load("data/hp_houses.RData")

hp_houses <- hp_houses %>% 
  mutate(book=str_replace(book_name, " - ", " \n"),
         house=str_c(str_to_upper(str_sub(house,1,1)), str_sub(house,2,str_length(house))))

hp_houses %>% 
  group_by(book, house) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(book_fct=factor(book, levels = unique(book), ordered = T),
         house_fct=factor(house, levels = unique(house), ordered = T)) %>% 
  ggplot(aes(x = fct_rev(book_fct), y = n, fill = fct_rev(house))) +
    geom_col(position = "dodge") +
    geom_text(aes(label = n), position = position_dodge(0.95), hjust = -0.2) +
    xlab("") +
    ylab("Count") +
    ylim(0,130) +
    scale_fill_manual(values = rev(c("#670001", "#FF9D0A", "#002E5F", "#2E751C")), name = "") +
    coord_flip() +
    labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
    theme_bw() +
    theme(legend.position = "top", 
          plot.caption = element_text(color = "grey80")) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    ggtitle("Occurence of house name in Harry Potter books") +
    ggsave(filename = "figs/num_houses.png", width = 8, height = 6, dpi = 600)

hp_houses %>% 
  group_by(book) %>%
  count(house) %>% 
  mutate(percent=n/sum(n),
         percent_label=round_preserve_sum(percent*100)) %>% 
  ungroup() %>% 
  mutate(book_fct=factor(book, levels = unique(book), ordered = T),
         house_fct=factor(house, levels = unique(house), ordered = T)) %>% 
  ggplot(aes(x = fct_rev(book_fct), y = percent, fill = fct_rev(house))) +
  geom_col() + 
  geom_text(aes(label = percent_label), position = position_stack(vjust = 0.5), color = "white") +
  xlab("") +
  ylab("Percentage") +
  scale_fill_manual(values = rev(c("#670001", "#FF9D0A", "#002E5F", "#2E751C")), name = "") +
  scale_y_continuous(labels=scales::percent) +
  coord_flip() +
  labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
  theme_bw() +
  theme(legend.position = "top", 
        plot.caption = element_text(color = "grey80")) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  ggtitle("Occurence of house name in Harry Potter books (in %)") +
  ggsave(filename = "figs/num_houses_percent.png", width = 8, height = 6, dpi = 600)

# number of pages in books

# https://dustyloft.wordpress.com/2007/07/12/number-of-pages-harry-potter/

pages <- hp_houses %>% 
  distinct(book) %>% 
  mutate(pages=c(223, 251, 317, 636, 766, 607, 607))

hp_houses %>% 
  count(book) %>% 
  inner_join(., pages, by = "book") %>% 
  mutate(house_per_page=round(n/pages,2),
         book_fct=factor(book, levels = unique(book), ordered = T)) %>% 
  ggplot(aes(x = fct_rev(book_fct), y = house_per_page)) +
    geom_col(fill = "#FFC001") +
    geom_text(aes(label = house_per_page), hjust = -0.2) +
    coord_flip() +
    xlab("") +
    ylab("House per page") +
    ylim(0,1.05) +
    labs(caption = "Łukasz Wawrowski - wawrowski.edu.pl") +
    theme_bw() +
    ggtitle("House name mentioned per page in Harry Potter books") +
    ggsave(filename = "figs/houses_page_percent.png", width = 8, height = 6, dpi = 600)

  