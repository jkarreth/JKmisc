# Custom ggplot2 theme, based on theme_bw()
# Johannes Karreth

library(ggplot2)
theme_jk <- function(base_size = 11, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_rect(fill = NA, color = "black", size = 0.75),
      strip.text.x = element_text(size = rel(1), margin = margin(t = base_size/2 * 0.6, b = base_size/2 * 0.6), face = "bold"),
      panel.border = element_rect(fill = NA, color = "black", size = 0.75),
      plot.title = element_text(size = rel(1.2), margin = margin(b = base_size/2 * 1.2), face = "bold"),
      panel.grid.major = element_line(colour = "grey85", size = 0.4, linetype = 3), 
      panel.grid.minor = element_line(colour = "grey90", size = 0.2, linetype = 3),
      axis.text = element_text(color = "black")
    )
}