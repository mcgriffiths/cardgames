library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)

make_card <- function(a, bgcolour, fontcolor) {
  layout <- tibble(x = c(0,0,1,1,0.5), 
                   y = c(0,0.95,0,1,0.5), 
                   text = c('', a, '', '', a),
                   fontsize = c(1,3,1,1,20),
                   just = c('left', 'left', 'right', 'right', 'center'), 
                   colour = as.factor(c(1, 2, 3, 4, 5)))
  
  card <- layout %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = text, size = fontsize, hjust = just, colour = colour),
              alpha = 1) +
    guides(size = F, colour = F) +
    scale_size(range=c(1,20)) +
    scale_colour_manual(values=c('black', fontcolor, 'red', 'blue', fontcolor)) +
    theme_void() +
    theme(plot.background = element_rect(fill = bgcolour))
  
  ggsave(glue('card_{a}_{bgcolour}.png'), width = 1.03, height = 1.60, units = 'in', dpi = 100)
}

make_card('9', 'blue', 'white')

card_list <- expand_grid(rank = 1:15, suit = c('blue', 'red', 'green', 'yellow', 'black'))

card_list <- card_list %>%
  mutate(textcol = ifelse(suit %in% c('yellow', 'green'), 'black', 'white'))

pwalk(list(card_list$rank, card_list$suit), make_card)
