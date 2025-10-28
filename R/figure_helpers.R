# install.packages('scico')

ggsave_and_return <- function(plot, fname, width=7.2, height=3.6, units = "in", scale = 1.6){
  ggsave(
    here::here(file.path("fig/",fname)),
    plot = plot,
    width = width,
    height = height,
    units = units,
    scale = scale)
  return(plot)
}


ggtext <- theme(text = element_text(family = "Helvetica"))
