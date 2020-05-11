library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(readr)

make_card <- function(a, bgcolour, fontcolor) {
  
  icon <- case_when(
    
    a == "0" ~ "*",
    a %in% c("5", "7") ~ "x2",
    TRUE ~ ""
    
  )
  
  layout <- tibble(x = c(0,0.3,1,1,0.5), 
                   y = c(0.05,0.95,0,1,0.5), 
                   text = c(icon, a, '', '', a),
                   fontsize = c(3,3,1,1,20),
                   just = c('left', 'right', 'right', 'right', 'center'), 
                   colour = as.factor(c(1, 2, 3, 4, 5)))
  
  card <- layout %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = text, size = fontsize, hjust = just, colour = colour),
              alpha = 1) +
    guides(size = F, colour = F) +
    scale_size(range=c(1,20)) +
    scale_colour_manual(values=c(fontcolor, fontcolor, 'red', 'blue', fontcolor)) +
    theme_void() +
    theme(plot.background = element_rect(fill = bgcolour))
  
  ggsave(glue('voodoo/card_{a}_{bgcolour}.png'), width = 1.03, height = 1.60, units = 'in', dpi = 100)
}

make_card('15', 'blue', 'white')

card_list <- expand_grid(rank = 1:15, suit = c('blue', 'red', 'green', 'yellow', 'black'))

card_list <- card_list %>%
  mutate(textcol = ifelse(suit %in% c('yellow', 'green'), 'black', 'white'))

pwalk(list(card_list$rank, card_list$suit, card_list$textcol), make_card)

card_list %>%
  transmute(label = glue('{rank}_{suit}'), 
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/voodoo/card_{rank}_{suit}.png')) %>%
  write_csv('voodoo_cards.csv')


