library(tidyverse)
library(numDeriv)
library(patchwork)

# set parameter values
w <- 100
delta <- 1
N <- 100
NA_t <- 100  #set to either 0 or 100
phi <- 0.001

# n(psi) function
n_func <- function(psi) {
  num <- delta / (1 + delta)
  denom <- w * phi * (N - NA_t) + psi * NA_t
  num * w / denom
}

# partial derivative function
ndot_func <- function(psi) {
  grad(n_func, psi)
}

# set simulation bounds and step
psi_seq <- seq(0.001, 0.1, by = 0.001)

# simulate n and ndot over psi
sim_data <- tibble(psi = psi_seq) %>%
  mutate(
    n = map_dbl(psi, n_func),
    ndot = map_dbl(psi, ndot_func)
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
      axis.line = element_line(arrow = arrow(type = "open", length = unit(0.2, "inches")))
    )
}

# plot 1: n(psi)
p1 <- ggplot(sim_data, aes(x = psi, y = n)) +
  geom_line(aes(color = "n(psi)"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  scale_color_manual(
    values = c("n(psi)" = "red"),
    labels = c(expression(n(psi)))) +
  labs(x = expression(psi)) +
  big_text_theme

# plot 2: partial derivative
p2 <- ggplot(sim_data, aes(x = psi, y = ndot)) +
  geom_line(aes(color = "partial"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  scale_color_manual(
    values = c("partial" = "purple"),
    labels = c(expression(frac(partialdiff * n, partialdiff * psi)))) +
  labs(x = expression(psi)) +
  big_text_theme

# print plots
(arrowed_axes(p1) | arrowed_axes(p2))
