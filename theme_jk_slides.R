theme_jk_slides <- function(base_size = 11, base_family = "Roboto")
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
            strip.background = element_rect(fill = NA, color = NA, linewidth = 0),
            strip.text.x = element_text(size = rel(1.25), margin = margin(t = base_size/2 * 0.6, b = base_size/2 * 0.6), face = "bold"),
            panel.border = element_rect(fill = NA, color = "black", size = 0.75),
            plot.title = element_text(size = rel(1.2), margin = margin(b = base_size/2 * 1.2), face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = rel(1.2), margin = margin(b = base_size/2 * 1.2), hjust = 0.5),
            plot.title.position = "plot",
            panel.grid.major = element_line(color = "grey90", linewidth = 0.4, linetype = 3), 
            panel.grid.minor = element_line(color = "grey85", linewidth = 0.2, linetype = 3),
            axis.text = element_text(color = "black"),
            axis.title.y = element_text(angle = 0, vjust = 0),
            axis.title.x = element_text(angle = 0, hjust = 0)
        )
}