library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(readr)

game <- 'yokai'

# generate card list from raw structure + derived values
yokai_card_list <- expand_grid(suit = 1:7, rank = 1:7) %>% 
  mutate(rank = rank + suit - 1) %>%
  mutate(label = paste(game, rank, letters[suit], sep = '_'))
 
texas_card_list <- expand_grid(suit = 1:8, rank = 1:11) %>% 
  filter(rank <= (12-suit)) %>%
  mutate(rank_display = ifelse(suit == 1, rank - 1, rank), 
         rank_display = 10*(suit-1) + rank_display,
         label = glue('{rank_display}_{letters[suit]}')) %>%
  group_by(suit) %>%
  mutate(max_rank = max(rank_display)) %>%
  ungroup()

generic_card_list <- expand_grid(suit = 1:8, rank = 1:20) #what needs to be done about character vs int vs factor?

voodoo_card_list <- expand_grid(suit = 1:5, rank = 0:15)

#palettes
fill_cols <- c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'white') %>%
  set_names(1:8)

text_cols <- c('white', 'white', 'black', 'black', 'black', 'white', 'white', 'black') %>%
  set_names(1:8)

#ggplot theme
theme_card <- theme_void() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        panel.spacing = unit(0.1, 'mm'),
        legend.position = 'none')

#texas template
make_texas_card <- function(df){

  ggplot(df) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(suit))) +
    geom_text(x = 0.15, y = 0.9, hjust = 'center', size = 5, aes(label = rank_display, colour = factor(suit))) +
    geom_text(x = 0.5, y = 0.5, hjust = 'center', size = 20, aes(label = rank_display, colour = factor(suit))) +
    geom_text(x = 0.15, y = 0.1, hjust = 'center', size = 3, aes(label = max_rank, colour = factor(suit))) +
    scale_colour_manual(values = text_cols) +
    scale_fill_manual(values = fill_cols) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    facet_grid(rank ~ suit) +
    theme_card
  
}

#yokai template
make_yokai_card <- function(df){
  
  ggplot(df) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(suit))) +
    geom_text(x = 0.15, y = 0.9, hjust = 'center', size = 5, aes(label = rank, colour = factor(suit))) +
    geom_text(x = 0.5, y = 0.5, hjust = 'center', size = 20, aes(label = rank, colour = factor(suit))) +
    scale_colour_manual(values = text_cols) +
    scale_fill_manual(values = fill_cols) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    facet_grid(rank ~ suit) +
    theme_card
}

# make the card images
# would ideally like to do this *within* the data frame pipe
texas_card_list$card_image <- texas_card_list %>%
  group_split(suit, rank) %>%
  map(make_texas_card) 

yokai_card_list$card_image <- yokai_card_list %>%
  group_split(suit, rank) %>%
  map(make_yokai_card) 

# save, preferably as svg - defaults for pc.io
save_plot <- function(card_image, dir, label, file_type = 'svg', w = 1.03, h = 1.6, u = 'in', dpi = 100, ...){
  ggsave(glue('{dir}/{label}.{file_type}'), card_image, width = w, height = h, units = u, dpi = dpi)
}

# save all cards and generate csv
yokai_card_list %>%
  pwalk(save_plot, dir = game) %>%
  mutate(image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/yokai/{label}.svg')) %>%
  select(label, image) %>%
  write_csv(glue('{game}/{game}_cards.csv'))
  
# generate and save deck
deck <- make_yokai_plot(yokai_card_list)
save_plot(deck, dir = game, label = paste(game, 'deck', sep = '_'), w = 7*1.03, h = 13*1.6)

