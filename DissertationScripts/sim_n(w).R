library(tidyverse)
library(numDeriv)
library(patchwork)

# set parameter values
delta <- 1
phi <- 0.001
N <- 100
NA_t <- 100  #set to either 0, 20, or 100
psi <- 1

# n(w) function
n_func <- function(w) {
  num <- delta / (1 + delta)
  denom <- w * phi * (N - NA_t) + psi * NA_t
  num * w / denom
}

# partial derivative function
ndot_func <- function(w) {
  C <- phi * (N - NA_t)
  D <- psi * NA_t
  (delta / (1 + delta)) * (D / (w * C + D)^2)
}


# set simulation bounds and step
w_seq <- seq(1, 2000, by = 1)

# simulate n and ndot over w
sim_data <- tibble(w = w_seq) %>%
  mutate(
    n = map_dbl(w, n_func),
    ndot = map_dbl(w, ndot_func)
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

# plot 1: n(w)
p1 <- ggplot(sim_data, aes(x = w, y = n)) +
  geom_line(aes(color = "n(w)"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  scale_color_manual(
    values = c("n(w)" = "red"),
    labels = c(expression(n(w)))) +
  labs(x = expression(w)) +
  big_text_theme

# plot 2: partial derivative
p2 <- ggplot(sim_data, aes(x = w, y = ndot)) +
  geom_line(aes(color = "partial"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  scale_color_manual(
    values = c("partial" = "purple"),
    labels = c(expression(frac(partialdiff * n, partialdiff * w)))) +
  labs(x = expression(w)) +
  big_text_theme

# print plots
(arrowed_axes(p1) | arrowed_axes(p2))
