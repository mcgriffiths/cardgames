library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
make_card <- function(a, bgcolour) {
  layout <- tibble(x = c(0,0,1,1,0.5), 
                   y = c(0,1,0,1,0.5), 
                   text = c(a, 'Baba', 'Coco', 'D', 'my text'),
                   fontsize = c(10,6,7,8,10),
                   just = c('left', 'left', 'right', 'right', 'center'), 
                   colour = c('red', 'blue', 'red', 'blue', 'green'))
  
  card <- layout %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = text, size = fontsize, hjust = just, colour = colour)) +
    guides(size = F, colour = F) +
    scale_size(range=c(5,10)) +
    theme_void() +
    theme(plot.background = element_rect(fill = bgcolour, colour = 'red'))
  
  ggsave(glue('card_{a}.png'), width = 60, height = 100, units = "mm")
}

walk(letters, make_card)
