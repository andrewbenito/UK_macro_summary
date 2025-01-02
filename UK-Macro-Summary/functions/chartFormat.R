
library(sysfonts)
library(showtext)


sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(theme_bw(base_size = 20, base_family = "Roboto Condensed") +
            theme(
              plot.subtitle = element_text(size = rel(1.0),
                                           margin=margin(4,0,0,0)),
              text = element_text(family = "Roboto Condensed", size = 30), # size=30 for pdf
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(linewidth = .5),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "dodgerblue"),
              legend.position = "bottom"              ))
showtext_auto()

