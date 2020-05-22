nranks <- 5
nsuits <- 4

# generate card list from raw structure + derived values
card_list <- tibble(rank = 2:20) %>% 
  mutate(label = glue('{game}_{rank}'))

# template
make_card <- function(df){
  
  ggplot(df) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(rank))) +
    geom_text(x = 0.15, y = 0.9, hjust = 'center', size = 5, colour = 'black', aes(label = rank)) +
    geom_text(x = 0.5, y = 0.5, hjust = 'center', size = 20, colour = 'black', aes(label = rank)) +
    scale_fill_manual(values = set_names(rainbow(19), 2:20)) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    facet_wrap(~ rank, ncol = 4) +
    theme_card
}