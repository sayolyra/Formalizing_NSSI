# SETUP: installing packages & libraries

library(ggplot2)
library(tidyr)
library(dplyr)

# E1: FUNCTIONAL FORM BETWEEN URGE (U) AND HANE (A)
# This plot visulizes how urge varies as a function as HANE, depending on the chosen parameter values.

# parameter values
k_SSvals <- c(1, 5, 10, 50, 100) # standard values for stress susceptibility
A <- seq(0, 1, by = 0.01) # HANE, bounded between 0 and 1
m_U <- 0.5 # Threshold to engage in urge

# functional form
prob_NSSI <- function(k_SS, A, m_U) {
  pNSSI_model <- 1 / (1 + exp(-k_SS * (A - m_U)))
  return(pNSSI_model)
}

# Prepare data in tidy format for ggplot
df <- expand.grid(A = A, k_SS = k_SSvals) %>%
  mutate(pNSSI = 1 / (1 + exp(-k_SS * (A - m_U))))

# Plot
p1 <- ggplot(df, aes(x = A, y = pNSSI, color = factor(k_SS), linetype = factor(k_SS))) +
  geom_line(size = 0.8) +
  labs(x = "HANE (A)",
       y = "Urge (U)",
       color = "Indvidual differences \nin levels stress susceptibility (k_SS)") +
  guides(linetype = "none") +
  theme_minimal() +
  scale_linetype_manual(values = c(2, 2, 1, 2, 2)) +
  scale_color_manual(values = c("#6ece58","#1f9e89","#000000", "#0072B2","#9c179e"))

# Print plot
print(p1)

# parameter values
k_SS <- 10 # impulsivity
A <- seq(0, 1, by = 0.01) # urge, bounded between 0 and 1
k_TNSSI_vals <- c(0.25, 0.5, 0.75) # Threshold to engage in NSSI

# functional form
prob_NSSI <- function(k_SS, A, m_U) {
  pNSSI_model <- 1 / (1 + exp(-k_SS * (A - m_U)))
  return(pNSSI_model)
}

# Prepare data in tidy format for ggplot
df <- expand.grid(A = A, m_U = k_TNSSI_vals) %>%
  mutate(pNSSI = 1 / (1 + exp(-k_SS * (A - m_U))))


# Plot
p2 <- ggplot(df, aes(x = A, y = pNSSI, color = factor(m_U), linetype = factor(m_U))) +
  geom_line(size = 0.8) +
  labs(x = "HANE (A)",
       y = "Urge (U)",
       color = "Indvidual differences in threshold \n to experience NSSI urge (m_U)") +
  theme_minimal() +
  scale_color_manual(values = c("#D55E00","#000000","#920000")) +
  scale_linetype_manual(values = c(2, 1, 2)) +
  guides(linetype = "none") +

  # add midpoints
  annotate("point", x = 0.25, y = 0.5, colour = "#D55E00", size = 2) +
  annotate("point", x = 0.5, y = 0.5, colour = "black", size = 2) +
  annotate("point", x = 0.75, y = 0.5, colour = "#920000", size = 2)

# Print plot
print(p2)

library("cowplot")
group_plot3 <-
  plot_grid(p1, p2,
            nrow = 2,
            rel_heights = c(1,1) #proportion of the plot
  )

group_plot3


#ggsave(group_plot3, file="FF_Urge_HANE.png", width=7, height=8)