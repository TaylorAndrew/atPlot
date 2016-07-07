theme_HFHS <- function() {
  ggthemes::theme_hc() +
  theme(axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(colour = "gray78"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 8, face = "italic"),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8))
}
