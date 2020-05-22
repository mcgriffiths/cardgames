nsuits <- 5
nranks <- 16

# generate card list from raw structure + derived values
card_list <- expand_grid(suit = 1:nsuits, rank = 0:15) %>% 
  mutate(icon = case_when(
    rank == 0 ~ '*',
    rank %in% c(5,7) ~ 'x2',
    TRUE ~ ''
  )) %>%
  mutate(label = glue('{game}_{rank}_{letters[suit]}'))

# template
make_card <- function(df){
  
  ggplot(df) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(suit))) +
    geom_text(x = 0.15, y = 0.9, hjust = 'center', size = 5, aes(label = rank, colour = factor(suit))) +
    geom_text(x = 0.5, y = 0.5, hjust = 'center', size = 20, aes(label = rank, colour = factor(suit))) +
    geom_text(x = 0.15, y = 0.1, hjust = 'center', size = 5, aes(label = icon, colour = factor(suit))) +
    scale_colour_manual(values = text_cols) +
    scale_fill_manual(values = fill_cols) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    facet_grid(rank ~ suit) +
    theme_card
}

# palettes for mapping suits into colours
fill_cols <- c('black', 'red', 'yellow', 'green', 'blue') %>%
  set_names(1:5)

text_cols <- c('white', 'white', 'black', 'black', 'white') %>%
  set_names(1:5)

