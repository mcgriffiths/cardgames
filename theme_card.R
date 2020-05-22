#ggplot theme - think about fonts?
theme_card <- theme_void() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        panel.spacing = unit(0.1, 'mm'),
        legend.position = 'none')

# save, preferably as svg - default sizes for pc.io
save_plot <- function(card_image, dir, label, file_type = 'svg', w = 1.03, h = 1.6, u = 'in', dpi = 100, ...){
  ggsave(glue('{dir}/{label}.{file_type}'), card_image, width = w, height = h, units = u, dpi = dpi)
}

repo <- 'https://raw.githubusercontent.com/mcgriffiths/cardgames/master'