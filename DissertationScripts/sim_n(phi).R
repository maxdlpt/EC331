library(tidyverse)
library(numDeriv)
library(patchwork)

# set parameter values
w <- 100
delta <- 1
N <- 100
NA_t <- 100   #set to either 0 or 100
psi <- 1

# n(phi) function
n_func <- function(phi) {
  num <- delta / (1 + delta)
  denom <- w * phi * (N - NA_t) + psi * NA_t
  num * w / denom
}

# partial derivative function
ndot_func <- function(phi) {
  grad(n_func, phi)
}

# set simulation bounds and step
phi_seq <- seq(0.001, 0.1, by = 0.001)

# simulate n and ndot over phi
sim_data <- tibble(phi = phi_seq) %>%
  mutate(
    n = map_dbl(phi, n_func),
    ndot = map_dbl(phi, ndot_func)
  )

# define theme
big_text_theme <- theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.ticks.y = element_line(size = 1),
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # major grid lines
    panel.grid.minor = element_line(color = "gray90", size = 0.3)   # minor grid lines
  )

# arrowed axes
add_arrowed_axes <- function(p) {
  p + 
    coord_cartesian(clip = "off") +
    theme(
      axis.line = element_line(arrow = arrow(type = "open", length = unit(0.2, "inches")))
    )
}

# plot 1: n(phi)
p1 <- ggplot(sim_data, aes(x = phi, y = n)) +
  geom_line(aes(color = "n(phi)"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  scale_color_manual(
    values = c("n(phi)" = "red"),
    labels = c(expression(n(phi)))
  ) +
  labs(x = expression(phi)) +
  big_text_theme

# plot 2: partial derivative
p2 <- ggplot(sim_data, aes(x = phi, y = ndot)) +
  geom_line(aes(color = "partial"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  scale_color_manual(
    values = c("partial" = "purple"),
    labels = c(expression(frac(partialdiff * n, partialdiff * phi)))
  ) +
  labs(x = expression(phi)) +
  big_text_theme

# print plots
(add_arrowed_axes(p1) | add_arrowed_axes(p2))

