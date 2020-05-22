nsuits <- 7
nranks <- 7

# generate card list from raw structure + derived values
card_list <- expand_grid(suit = 1:nsuits, rank = 1:nranks) %>% 
  mutate(rank = rank + suit - 1) %>%
  mutate(label = glue('{game}_{rank}_{letters[suit]}'))

#yokai template
make_card <- function(df){
  
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