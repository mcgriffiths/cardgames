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
  
  layout <- tibble(x = c(0.15,0.15,0.5), 
                   y = c(0.05,0.95,0.5), 
                   text = c(icon, rank, rank),
                   fontsize = c(3,3,20),
                   just = c('center', 'center', 'center'))
  
  card <- layout %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = text, size = fontsize, hjust = just), colour = textcol) +
    guides(size = F, colour = F) +
    scale_size_area(max_size = 20) +
    scale_colour_manual(values=c(textcol)) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = suit, colour = suit))
  
  ggsave(glue('card_{rank}_{suit}.png'), width = 1.03, height = 1.6, units = 'in', dpi = 100)
}


make_card('5', 'black')


make_token <- function(rank, suit, label,...) {

  icon <- ""
  
  textcol <- ifelse(suit %in% c('yellow', 'green'), 'black', 'white')
  
  layout <- tibble(x = c(0,0.05,0.5,1,1), 
                   y = c(0.05,0.75,0.5,1,0), 
                   text = c(icon, rank, "","",""),
                   fontsize = c(3,3,20,1,1),
                   just = c('left', 'left', 'center','right','right'))
  
  card <- layout %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = text, size = fontsize, hjust = just), colour = textcol) +
    guides(size = F, colour = F) +
    scale_size(range=c(1,20)) +
    scale_colour_manual(values=c(textcol)) +
    theme_void() +
    theme(plot.background = element_rect(fill = suit, colour = suit))
  
  ggsave(glue('crew/token_{label}.png'), width = 1.03, height = 0.5, units = 'in', dpi = 100)
}



voodoo_card_list <- expand_grid(rank = 0:15, suit = c('blue', 'red', 'green', 'yellow', 'black'))

crew_card_list <- expand_grid(rank = 1:9, suit = c('blue', 'red', 'green', 'yellow')) 

crew_card_list <- bind_rows(crew_card_list, expand_grid(rank = 1:4, suit = 'black'))

crew_token_list <- tibble(rank = c(1:5, ">", ">>", ">>>", ">>>>", "\u2126"), 
                          suit = 'black',
                          label = c(1:5, "a", "b", "c", "d", "e"))

pwalk(crew_token_list, make_token)

crew_card_list %>%
  transmute(label = glue('{rank}_{suit}'), 
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/crew/task_{rank}_{suit}.png')) %>%
  write_csv('crew_tasks.csv')

crew_token_list %>%
  transmute(label = glue('token_{label}'), 
            image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/crew/{label}.png')) %>%
  write_csv('crew_tokens.csv')

