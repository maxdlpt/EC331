library(tidyverse)

# set parameters
delta <- 1
w_m <- 1
w_f <- 1
psi <- 1
phi <- 0.5
N <- 100

# critical threshold
N_A_star <- N * w_m * phi / (psi + w_m * phi)

# define the partial derivative of n_t w.r.t. w_f as a function of N_A
dn_dwf <- function(N_A) {
  numerator <- N_A * psi - (N - N_A) * w_m * phi
  denominator <- (N_A * psi + (N - N_A) * w_f * phi)^2
  (delta / (1 + delta)) * (numerator / denominator)
}

# simulate over N_A
sim_data <- tibble(N_A = seq(0.01, N, by = 0.01)) %>%
  mutate(derivative = map_dbl(N_A, dn_dwf))

big_text_theme <- theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
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

# plot
p1<-ggplot(sim_data, aes(x = N_A, y = derivative)) +
  geom_line(color = "purple", size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = N_A_star, linetype = "dashed", color = "gray40", linewidth = 1) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = N - N_A_star+5, y = 0.0025, 
           label = expression(frac(partialdiff * n[t], partialdiff * w[f])), color = "purple4", size = 6) +
  labs(
    x = expression(N["A,t"]),
    y = expression("Effect of " %up%w[f] * "  on  " * n[t])
  ) +
  scale_x_continuous(breaks = c(0, N_A_star, N), labels = c("0", expression(N[A]^"*"), expression(N))) +
  big_text_theme +
  theme(axis.text.x = element_text(size = 16))


arrowed_axes(p1)