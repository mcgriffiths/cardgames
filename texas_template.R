nsuits <- 8
nranks <- 11

card_list <- expand_grid(suit = 1:nsuits, rank = 1:nranks) %>% 
  filter(rank <= (12-suit)) %>%
  mutate(rank_display = ifelse(suit == 1, rank - 1, rank), 
         rank_display = 10*(suit-1) + rank_display) %>%
  group_by(suit) %>%
  mutate(max_rank = max(rank_display)) %>%
  ungroup() %>%
  mutate(label = glue('{game}_{rank_display}_{letters[suit]}'))

#texas template
make_card <- function(df){
  
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

# palettes for mapping suits into colours
fill_cols <- c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'white') %>%
  set_names(1:8)

text_cols <- c('white', 'white', 'black', 'black', 'black', 'white', 'white', 'black') %>%
  set_names(1:8)

