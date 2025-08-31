

########################### Test simulation
test_results <- simNSSI(
  time_vec = 1:900,
  # 30 mins
  stepsize = 0.01,
  PS = pars_default,
  INI = initial_default,
  seed = 112
)


########################### Plot simulation output

# Create dataframe with results in correct form, so that it can be plotted
df <- as.data.frame(test_results$outmat)
df_long <- df %>%
  pivot_longer(
    cols = c(E, A, U, P, PN, N),
    names_to = "variable",
    values_to = "value"
  )

# Create Plot
ggplot(df_long, aes(x = timepoints, y = value, color = variable)) +
  annotate(
    "rect",
    # shaded block to represent E
    xmin = 300,
    xmax = 345,
    ymin = 0,
    ymax = 1,
    fill = "#B6DBFF",
    alpha = 0.5
  ) +
  geom_line(linewidth = 0.7) +
  labs(x = "Time in Minutes",
       y = "Intensity",
       color = "Variable changes over time") +
  scale_color_manual(
    values = c(
      "A" = "#D55E00",
      "E" = "transparent",
      # dont display line for E
      "N" = "#920000",
      "P" = "#009E73",
      "PN" = "#332288",
      "U" = "#AA4499"
    ),
    labels = c(
      "HANE (A)",
      "NSSI (N)",
      "Pain (P)",
      "Probability of NSSI (PN)",
      "Urge (U)"
    ),
    breaks = c("A", "N", "P", "PN", "U"),
    name = "Variable changes over time"
  ) +
  scale_x_continuous(breaks = seq(0, 900, 60),
                     labels = as.character(seq(0, 15, by = 1))) + #,
  #limits = c(300,495)) + # to zoom into graph
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 18,
                                margin = unit(c(4, 0, 0, 0), "mm")),
    axis.title.y = element_text(size = 18,
                                margin = unit(c(0, 4, 0, 0), "mm")),
    legend.text = element_text(size = 13),
    legend.title = element_text(
      size = 18,
      margin = unit(c(4, 0, 4, 0), "mm"),
      hjust = 0.5
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical"
  ) +
  # Add invisible geom for the shaded area legend
  geom_point(aes(fill = "Distressing event (E)"),
             alpha = 0,
             size = 0) +
  scale_fill_manual(
    values = c("Distressing event (E)" = "#8BB7E0"),
    name = "",
    guide = guide_legend(
      override.aes = list(
        shape = 22,
        size = 4,
        alpha = 0.7,
        color = NA
      ),
      order = 2
    )
  ) +
  guides(color = guide_legend(order = 1))