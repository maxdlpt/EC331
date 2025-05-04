library(tidyverse)
library(numDeriv)
library(patchwork)

# set parameter values
w <- 1000
delta <- 1
N <- 100
phi <- 0.001
psi <- 0.1   # Set to either 10, 1, or 0.1

# n(NA_t) function
n_func <- function(NA_t) {
  num <- delta / (1 + delta)
  denom <- w * phi * (N - NA_t) + psi * NA_t
  num * w / denom
}

# partial derivative function
ndot_func <- function(NA_t) {
  grad(n_func, NA_t)
}

# set simulation bounds and step
NA_t_seq <- seq(0, 100, by = 1)

# simulate n and ndot over NA_t
sim_data <- tibble(NA_t = NA_t_seq) %>%
  mutate(
    n = map_dbl(NA_t, n_func),
    ndot = map_dbl(NA_t, ndot_func)
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
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_line(color = "gray90", size = 0.3))

# arrowed axes
arrowed_axes <- function(p) {
  p + 
    coord_cartesian(clip = "off") +
    theme(
      axis.line = element_line(arrow = arrow(type = "open", length = unit(0.2, "inches"))))
}

# plot 1: n(NA_t)
p1 <- ggplot(sim_data, aes(x = NA_t, y = n)) +
  geom_line(aes(color = "n(NA_t)"), size = 1.2) +
  geom_vline(aes(xintercept = N, color = "N"), linetype = "dashed", size = 1) +
  scale_color_manual(
    breaks = c("n(NA_t)", "N"),  # << control legend order
    values = c("n(NA_t)" = "red", "N" = "black"),
    labels = c(expression(n(N[A])), expression(N))) +
  labs(x = expression(N[A])) +
  big_text_theme

# plot 2: partial derivative
p2 <- ggplot(sim_data, aes(x = NA_t, y = ndot)) +
  geom_line(aes(color = "partial"), size = 1.2) +
  geom_vline(aes(xintercept = N, color = "N"), linetype = "dashed", size = 1) +
  scale_color_manual(
    breaks = c("partial", "N"),  # << control legend order
    values = c("partial" = "purple", "N" = "black"),
    labels = c(expression(frac(partialdiff * n, partialdiff * N[A])), expression(N))) +
  labs(x = expression(N[A])) +
  big_text_theme

# print plots
(arrowed_axes(p1) | arrowed_axes(p2))
