library(magick)
library(pdftools)
library(glue)
library(purrr)
library(tidyr)
library(dplyr)
library(readr)

cards <- image_read_pdf('2f-spiele_thefight_cards_us.pdf', density = 150)

image_crop(cards[2], "313x495+155+135")
image_crop(cards, "313x495+459+135")
image_crop(cards, "313x495+772+135")

image_crop(cards, "313x495+146+630")
image_crop(cards, "313x495+459+630")
image_crop(cards, "313x495+772+630")

image_crop(cards, "313x495+146+1125")
image_crop(cards, "313x495+459+1125")
image_crop(cards[1], "313x495+772+1126")

get_image <- function(page, x, y, ...){
  image_crop(cards[page], glue("313x495+{x}+{y}")) %>%
    image_resize('x160')
}

x <- c(146, 459, 772)
y <- c(135, 630, 1126)
page <- 1:10

card_locs <- expand_grid(page, y, x) %>%
  mutate(x = ifelse(page %in% c(2,4,6,8,10), x+9, x),
         y = ifelse(page %in% c(2,4,6,8,10), y+1, y))

image_list <- pmap(card_locs, get_image)

monsters <- c(1:9, 57:62) %>% set_names(1:15)
weapons <- c(73, 19:26, 76) %>% set_names(0:9)
specials <- c(37, 38, 42, 55, 56) %>% set_names(1:5)
walk2(monsters, names(monsters), ~image_write(image_list[[.x]], path = glue('fight/monster_{.y}.png')))
walk2(weapons, names(weapons), ~image_write(image_list[[.x]], path = glue('fight/weapon_{.y}.png')))
walk2(specials, names(specials), ~image_write(image_list[[.x]], path = glue('fight/special_{.y}.png')))

image_write(image_list[[10]], path = glue('fight/monster_back.png'))
image_write(image_list[[28]], path = glue('fight/weapon_back.png'))   
image_write(image_list[[47]], path = glue('fight/special_back.png'))   

texas_card_list %>%
  transmute(label = glue('{rank}_{suit}'), 
            image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/texas/card_{rank}_{suit}.png')) %>%
  write_csv('texas_cards.csv')

tibble(label = 1:15) %>%
  mutate(label = glue('monster_{label}'),
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/fight/{label}.png')) %>%
  write_csv('fight_monsters.csv')

tibble(label = 0:9) %>%
  mutate(label = glue('weapon_{label}'),
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/fight/{label}.png')) %>%
  write_csv('fight_weapons.csv')

tibble(label = 1:5) %>%
  mutate(label = glue('special_{label}'),
         image = glue('https://raw.githubusercontent.com/mcgriffiths/cardgames/master/fight/{label}.png')) %>%
  write_csv('fight_specials.csv')
