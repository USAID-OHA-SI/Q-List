# Utilities Functions

fy_months <- function(start = 10) {
  if(!start %in% 1:12) return(NULL)

  c(start:12, 1:(start-1))
}

#' @title Viz - fa-icons
#'
#'
fa_icons <- function(icon = "fa-users",
                     fsize = 100,
                     fcolor = "red",
                     label = NULL,
                     lsize = 20,
                     lcolor = "#212721", # usaid_black
                     loc = "bottom",
                     save_as = NULL,
                     iwidth = 1,
                     iheight = 1) {

  # Placement Guide
  df_pos <- tibble(
    pos   = c("top", "right", "bottom", "left"),
    x     = c(0,     1.25,    0,    -1.25),
    y     = c(1,     0,       -1.25,    -0),
    hjust = c(0.5,   0,       0.5,      1),
    vjust = c(0,     0.5,     1,        0.5)
  )

  df_pos_sel <- df_pos %>% filter(pos == loc)

  viz_icon <- ggplot() +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1),
              fill = NA, color = NA) +
    # geom_rect(aes(xmin = -1.25, xmax = 1.25, ymin = -1.25, ymax = 1),
    #           fill = NA, color = NA) +
    geom_text(aes(0, 0),
              label = emojifont::fontawesome(icon),
              family = "fontawesome-webfont",
              size = fsize,
              color = fcolor)

  # Label
  if (!is.null(label)) {

    viz_icon <- viz_icon +
      geom_text(aes(df_pos_sel$x, df_pos_sel$y),
                label = label,
                size = lsize,
                color = fcolor,
                hjust = df_pos_sel$hjust,
                vjust = df_pos_sel$vjust)

  }

  #Theme
  viz_icon <- viz_icon +
    #coord_equal(clip = "off") +
    theme_void() +
    theme(text = element_text(family = "Source Sans Pro"),
          #plot.margin = grid::unit(c(1, 1, 1, 1), "mm"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent', color = NA),
          legend.box.background = element_rect(fill='transparent', color = NA))

  if (!is.null(save_as)) {
    ggsave(filename = save_as,
           plot = viz_icon,
           dpi = 320,
           width = 1.5,
           height = 1.5,
           bg = "transparent")
  }

  return(viz_icon)
}
