library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(readr)


voodoo_template <- function(rank, suit) {
  
  icon <- case_when(
    rank == "0" ~ "*",
    rank %in% c("5", "7") ~ "x2",
    TRUE ~ ""
  )
  
  text_col <- as.numeric(suit %in% c('yellow', 'green'))
  
  tribble(
    ~x, ~y, ~text, ~text_col, ~text_size, ~just,
    0.15, 0.1, icon, text_col, 2, 'center',
    0.15, 0.9, rank, text_col, 2, 'center',
    0.5, 0.5, rank, text_col, 20, 'center'
  )

}

generic_template <- function(rank, suit) {
  
  icon <- ''
  
  tribble(
    ~x, ~y, ~text, ~text_size, ~just,
    0.15, 0.1, icon, 3, 'center',
    0.15, 0.9, rank, 3, 'center',
    0.5, 0.5, rank, 20, 'center'
  )
  
}

texas_template <- function(rank, suit) {
  
  icon <- 10*(suit-1) + 12 - suit
  icon <- if_else(suit == 1, icon - 1, icon)
  
  tribble(
    ~x, ~y, ~text, ~text_size, ~just,
    0.15, 0.1, icon, 1, 'center',
    0.15, 0.9, rank, 3, 'center',
    0.5, 0.5, rank, 20, 'center'
  )
  
}



make_card <- function(rank, suit, template, dir, filetype = 'png', card_width = 1.03, card_height = 1.6, ...) {
  
  fill_cols <- c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'white')
  names(fill_cols) <- 1:8
  
  text_col <- if_else(fill_cols[suit] %in% c('black', 'red', 'blue', 'purple'), 'white', 'black')  
  
  template(rank, suit) %>%
    ggplot(aes(x = x, y = y)) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = fill_cols[suit]) +
    geom_text(aes(label = text, size = text_size, hjust = just), colour = text_col) +
    guides(size = F, colour = F) +
    scale_size_area(max_size = 20) +
    scale_colour_manual(values = c('0' = 'white', '1' = 'black')) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_void() 

  ggsave(glue('{dir}/card_{rank}_{suit}.{filetype}'), width = card_width, height = card_height, units = 'in', dpi = 100)
  
  
}


voodoo_card_list <- expand_grid(rank = 0:15, suit = c('blue', 'red', 'green', 'yellow', 'black'))

crew_card_list <- expand_grid(rank = 1:9, suit = c('blue', 'red', 'green', 'yellow')) 
crew_card_list <- bind_rows(crew_card_list, expand_grid(rank = 1:4, suit = 'black'))

crew_token_list <- tibble(rank = c(1:5, ">", ">>", ">>>", ">>>>", "\u2126"), 
                          suit = 'black',
                          label = c(1:5, "a", "b", "c", "d", "e"))

generic_card_list <- expand_grid(rank = c(as.character(0:20), ''), suit = 1:8)

pwalk(generic_card_list, make_card, template = generic_template, dir = 'generic_svg')

generic_card_list %>%
  transmute(label = glue('{rank}_{suit}'), 
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/generic_svg/card_{rank}_{suit}.svg')) %>%
  write_csv('generic_cards_svg.csv')

crew_token_list %>%
  transmute(label = glue('token_{label}'), 
            image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/crew/{label}.png')) %>%
  write_csv('crew_tokens.csv')


yokai_card_list <- expand_grid(suit = 1:7, rank = 1:7) %>% 
  mutate(rank = rank + suit - 1)
 
texas_card_list <- 
  expand_grid(rank = 1:11, suit = 1:8) %>% 
  filter(rank <= (12-suit)) %>%
  mutate(rank_display = ifelse(suit == 1, rank - 1, rank), 
         rank_display = 10*(suit-1) + rank_display,
         label = glue('{rank}_{suit}')) %>%
  group_by(suit) %>%
  mutate(max_rank = max(rank_display)) %>%
  ungroup() %>%
  arrange(suit, rank)

texas_card_list %>%
  transmute(label = glue('texas_{label}'), 
            image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/texas/{label}.png')) %>%
  write_csv('texas_cards.csv')

#using facets to produce everything in one image
generic_card_list <- expand_grid(rank = 1:10, suit = 1:8)
voodoo_card_list <- expand_grid(rank = 0:15, suit = 1:5)

generic_cards <- yokai_card_list %>%
  mutate(card = map2(rank, suit, generic_template)) %>%
  unnest(card)

fill_cols <- c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'white')
names(fill_cols) <- 1:8

generic_cards %>%
  #filter(rank <= 1, suit <= 1 ) %>%
  mutate(text_col = factor(if_else(fill_cols[suit] %in% c('black', 'red', 'blue', 'purple'), 0, 1))) %>%
    ggplot(aes(x = x, y = y)) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, color = 'grey', aes(fill = factor(suit))) +
    geom_text(aes(label = text, size = text_size, hjust = just, colour = text_col)) +
    facet_grid(rank ~ suit) +
    guides(size = F, colour = F, fill = F) +
    scale_size_area(max_size = 20) +
    scale_colour_manual(values = c('0' = 'white', '1' = 'black')) +
    scale_fill_manual(values = fill_cols) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_void() +
    theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(0, 'cm'))
 
ggsave('test_card.png', width = 7*1.03, height = 13*1.6, units = 'in', dpi = 100)

#try without template, split geom_text

make_texas_plot <- function(df){

  df %>%
    mutate(text_col = factor(if_else(fill_cols[suit] %in% c('black', 'red', 'blue', 'purple'), 0, 1))) %>%
    ggplot() +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(suit))) +
    geom_text(x = 0.15, y = 0.9, hjust = 'center', size = 5, aes(label = rank_display, colour = text_col)) +
    geom_text(x = 0.5, y = 0.5, hjust = 'center', size = 20, aes(label = rank_display, colour = text_col)) +
    geom_text(x = 0.15, y = 0.1, hjust = 'center', size = 3, aes(label = max_rank, colour = text_col)) +
    facet_grid(rank ~ suit) +
    guides(size = F, colour = F, fill = F) +
    scale_colour_manual(values = c('0' = 'white', '1' = 'black')) +
    scale_fill_manual(values = fill_cols) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_void() +
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.spacing = unit(0.1, 'mm'))
    
}

make_yokai_plot <- function(df){
  
  card <- df %>%
    mutate(text_col = factor(if_else(fill_cols[suit] %in% c('black', 'red', 'blue', 'purple'), 0, 1))) %>%
    ggplot() +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(suit))) +
    geom_text(x = 0.15, y = 0.9, hjust = 'center', size = 5, aes(label = rank, colour = text_col)) +
    geom_text(x = 0.5, y = 0.5, hjust = 'center', size = 20, aes(label = rank, colour = text_col)) +
    facet_grid(rank ~ suit) +
    scale_colour_manual(values = c('0' = 'white', '1' = 'black')) +
    scale_fill_manual(values = fill_cols)
  
  plot_template(card)

}


plot_template <- function(plot){
  plot +  
    guides(size = F, colour = F, fill = F) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_void() +
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.spacing = unit(0.1, 'mm'))
}

# would like to do this *within* the data frame pipe
texas_card_list$card_image <- texas_card_list %>%
  group_split(suit, rank) %>%
  map(make_texas_plot) 


yokai_card_list$card_image <- yokai_card_list %>%
  group_split(suit, rank) %>%
  map(make_yokai_plot) 

# saving options
save_plot <- function(label, card_image, dir, file_type, w = 1.03, h = 1.6, u = 'in', dpi = 100, ...){
  ggsave(glue('{dir}/{label}.{file_type}'), card_image, width = w, height = h, units = u, dpi = dpi)
}

# save
texas_card_list %>%
  pwalk(save_plot, dir = 'test2', file_type = 'png')

yokai_card_list %>%
  mutate(label = paste('yokai', rank, suit, sep = '_')) %>%
  pwalk(save_plot, dir = 'yokai', file_type = 'svg') %>%
  mutate(image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/yokai/{label}.svg')) %>%
  select(label, image) %>%
  write_csv('yokai/yokai_cards.csv')

deck_preview <- make_yokai_plot(yokai_card_list)

ggsave('yokai/yokai_deck.png', deck_preview, width = 7*1.03, height = 13*1.6, units = 'in', dpi = 100)
