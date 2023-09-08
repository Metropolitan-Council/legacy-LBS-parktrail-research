demo_compare <- function(x) {
  x %>%
    # filter(variable == quo_name(quo(!!(.variable)))) %>%
    mutate(
      flag = if_else(sig == "ns" & source == "survey", 1, 0),
      source = case_when(
        sig == "ns" ~ "No significant difference",
        source == "survey" ~ "Intercept survey",
        source == "LBS" ~ "LBS data"
      )
    ) %>%
    filter(flag != 1) %>%
    ggplot(aes(
      y = forcats::fct_rev(group),
      x = percent,
      col = source, shape = source, fill = source
    )) + # forcats::fct_rev(source))) +
    geom_errorbar(
      aes(
        xmin = if_else(percent - moe_95 < 0, 0, percent - moe_95),
        xmax = if_else(percent + moe_95 > 1, 1, percent + moe_95)
      ),
      width = 0,
      position = position_dodge(width = .2)
    ) +
    geom_point(position = position_dodge(width = .2)) +
    facet_wrap(~unit,
      ncol = 2, scales = "free_x",
      labeller = labeller(park = label_wrap_gen(20))
    ) +
    theme_council_open() +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + # percent_format(scale = 1)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
    labs(y = "", x = "", col = "", shape = "", fill = "") +
    # geom_vline(xintercept = 0) +
    # annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey92")+
    annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, color = "grey92") +
    theme(
      panel.grid.major.y = element_line(linetype = 3),
      legend.position = "bottom",
      strip.text.x = element_text(size = rel(.8)),
      strip.text.y = element_text(size = rel(.8)),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = rel(.6)),
      axis.text.x = element_text(size = rel(.6)),
      legend.text = element_text(size = rel(.8)),
      legend.key.size = unit(.1, "lines")
    ) +
    scale_colour_manual(values = c(
      "No significant difference" = "black",
      "Intercept survey" = "#a6611a",
      "LBS data" = "#018571"
    )) +
    scale_fill_manual(values = c(
      "No significant difference" = "white",
      "Intercept survey" = "#a6611a",
      "LBS data" = "#018571"
    )) +
    scale_shape_manual(values = c(
      "No significant difference" = 21,
      "Intercept survey" = 22,
      "LBS data" = 23
    ))
}
