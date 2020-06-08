library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(readr)

source('theme_card.R')

game <- 'zimbabwe'
source(paste(game, 'template.R', sep = '_'))

# make the card images
card_list$card_image <- card_list %>%
  rowwise() %>%
  group_split() %>%
  map(make_card) 

# save all cards and generate csv
card_list %>%
  pwalk(save_plot, dir = game) %>%
  mutate(image = glue('{repo}/{game}/{label}.svg')) %>%
  select(label, image) %>%
  write_csv(glue('{game}/{game}_cards.csv'))

# generate and save deck (nsuits and nranks isn't always correct for width/height)
deck <- make_card(card_list) %>% 
  save_plot(dir = game, label = paste(game, 'deck', sep = '_'), w = nsuits*1.03, h = nranks*1.6)
