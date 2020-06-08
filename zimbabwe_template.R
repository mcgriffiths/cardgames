nranks <- 10
nsuits <- 1

# generate card list from raw structure + derived values
card_list <- tibble(rank = 1:10) %>% 
  mutate(label = glue('{game}_{rank}'))

# template
make_card <- function(df){
  
  ggplot(df) +
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, aes(fill = factor(rank))) +
    geom_label(x = 0.15, y = 0.9, hjust = 'center', size = 5, colour = 'black', aes(label = ifelse(rank==10, 0, rank))) +
    geom_label(x = 0.85, y = 0.9, hjust = 'center', size = 5, colour = 'black', aes(label = ifelse(rank==10, 0, rank))) +
    geom_label(x = 0.5, y = 0.5, hjust = 'center', size = 20, colour = 'black', aes(label = rank)) +
    scale_fill_manual(values = set_names(rainbow(10), 1:10)) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    facet_wrap(~ rank, ncol = nsuits) +
    theme_card
}
