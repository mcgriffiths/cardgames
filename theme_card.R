# palettes for mapping suits into colours - can these be included in the theme? could by having a separate script
fill_cols <- c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'white') %>%
  set_names(1:8)

text_cols <- c('white', 'white', 'black', 'black', 'black', 'white', 'white', 'black') %>%
  set_names(1:8)

#ggplot theme (get rid of legends, get rid of spacing between facets) - think about fonts?
theme_card <- theme_void() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        panel.spacing = unit(0.1, 'mm'),
        legend.position = 'none')

# save, preferably as svg - defaults for pc.io
save_plot <- function(card_image, dir, label, file_type = 'svg', w = 1.03, h = 1.6, u = 'in', dpi = 100, ...){
  ggsave(glue('{dir}/{label}.{file_type}'), card_image, width = w, height = h, units = u, dpi = dpi)
}

repo <- 'https://raw.githubusercontent.com/mcgriffiths/cardgames/master'