# ggplot theme -----

# TODO: build this out as an actual theme
legacy_theme <-
  theme_council_open() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 3),
        strip.placement = "outside",
        legend.spacing= unit(c(0, 0, 0, 0), "cm"),
        plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"),
        axis.title.y = element_text(
          angle = 0,
          vjust = .5
        ),
        text = element_text(family = "Avenir"),
        axis.text = element_text(size = 12),
        plot.caption = element_text(size = 12, color = "grey30"),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        plot.subtitle = element_text(size = 14)
  )

# mn legacy colors -----
legacy_blue <- "#4461ab"
legacy_green <- "#2da44a"
legacy_orange <- "#eea22a"

# variations on mn legacy colors -----
legcay_dkgreen <- "#4c822e"
legacy_ltblue <- "#0185ca"

# common colors -----
vehicle_col <- "#79b9db"
ped_col <- "#49a2d6"
bike_col <- "#2079ba"
active_col <- "#023d78"
weekday_color <- "#7D84B2"
weekend_color <- "#F7AD64"
park_map_color <- "#729B79"
inset_state_color <- "#BACDB0"
inset_marker_color <- "#F7AD64"

# color palettes -----
system_label_cols <- c("DNR State" = legacy_blue,
                       "Greater MN Regional" = legacy_orange,
                       "Metro Regional" = councilR::colors$cdGreen)


