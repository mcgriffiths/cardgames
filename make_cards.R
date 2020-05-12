library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(readr)

make_card <- function(rank, suit, ...) {
  
  icon <- case_when(
    
    rank == "0" ~ " *",
    rank %in% c("5", "7") ~ "x2",
    TRUE ~ ""
    
  )
  
  textcol <- ifelse(suit %in% c('yellow', 'green'), 'black', 'white')
  
  layout <- tibble(x = c(0,0.15,0.5,1,1), 
                   y = c(0.05,0.95,0.5,1,0), 
                   text = c(icon, rank, rank,"",""),
                   fontsize = c(3,3,20,1,1),
                   just = c('left', 'center', 'center','right','right'))
  
  card <- layout %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = text, size = fontsize, hjust = just), colour = textcol) +
    guides(size = F, colour = F) +
    scale_size(range=c(1,20)) +
    scale_colour_manual(values=c(textcol)) +
    theme_void() +
    theme(plot.background = element_rect(fill = suit, colour = suit))
  
  ggsave(glue('voodoo/card_{rank}_{suit}.png'), width = 1.03, height = 1.60, units = 'in', dpi = 100)
}

make_card('11', 'blue', 'white')

card_list <- expand_grid(rank = 0:15, suit = c('blue', 'red', 'green', 'yellow', 'black'))

pwalk(card_list, make_card)

card_list %>%
  transmute(label = glue('{rank}_{suit}'), 
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/voodoo/card_{rank}_{suit}.png')) %>%
  write_csv('voodoo_cards.csv')


