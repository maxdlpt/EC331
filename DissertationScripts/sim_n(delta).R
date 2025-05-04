library(tidyverse)
library(numDeriv)

# set parameter values
w <- 1000
phi <- 0.001
N <- 100
NA_t <- 100
psi <- 1

# n(delta) function
n_func <- function(delta) {
  num <- delta / (1 + delta)
  denom <- w * phi * (N - NA_t) + psi * NA_t
  num * w / denom
}

# partial derivative function
ndot_func <- function(delta) {
  grad(n_func, delta)
}

# set simulation bounds and step
delta_seq <- seq(0.01, 25, by = 0.1)

# simulate n and ndot over delta
sim_data <- tibble(delta = delta_seq) %>%
  mutate(
    n = map_dbl(delta, n_func),
    ndot = map_dbl(delta, ndot_func)
  )

# prepare data
plot_data <- sim_data %>%
  pivot_longer(cols = c(n, ndot), names_to = "func", values_to = "value") %>%
  mutate(func = recode(func,
                       n = "n_t(delta)",
                       ndot = "partial"))


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

# plot
p <- ggplot(plot_data, aes(x = delta, y = value, color = func)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    name = "Function",
    values = c("n_t(delta)" = "red", "partial" = "purple"),
    labels = c(expression(n(delta)), expression(frac(partialdiff * n, partialdiff * delta)))
  ) +
  labs(x = expression(delta)) +
  big_text_theme

print(arrowed_axes(p))