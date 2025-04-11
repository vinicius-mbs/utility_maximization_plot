library(dplyr)
library(ggplot2)

##### Cobb-Douglas utility function ####

# Parameters
P_y <- 30 # Price of good y
P_x <- 10 # Price of good x
I <- 200 # Income
alpha <- 0.4 # Share of income spent on good x
beta <- 0.6 # Share of income spent on good y

# Utility function
utility <- function(x, y) {
  x^alpha * y^beta
}

# Budget constraint (BC) function
bc <- function(x) {
  -(P_x / P_y) * x + I / P_y
}

# Using the Lagrangian method and first-order conditions, 
# we derive the optimal bundle (x*, y*) that maximizes utility subject to 
# the budget constraint:
x_star = alpha * I / P_x
y_star = beta * I / P_y
# Note that the result above result is sensitive to the choice 
# of the utility function

# Substituting in the utility function, we find this indifference curve (IC) 
# value
U2 = utility(x_star, y_star)
U1 = U2 - 2 # Lower indifference curve (below the optimum)
U3 = U2 + 2 # Higher indifference curve (above the optimum, not attainable)

# As the value for the utility do not change in the entire indifference 
# curve, we can solve the utility equation for y and express the IC as 
# a function of x:
y_ic = function(x, U){
  (U / x^alpha)^(1 / beta)
}

# Indifference curve data
x_range = seq(0.1, 20, 0.1) # avoiding 0 to prevent division by zero

df_ic1 = data.frame(x = x_range) %>% mutate(y = y_ic(x, U1)) %>% filter(y <= 15)
df_ic2 = data.frame(x = x_range) %>% mutate(y = y_ic(x, U2)) %>% filter(y <= 15)
df_ic3 = data.frame(x = x_range) %>% mutate(y = y_ic(x, U3)) %>% filter(y <= 15)

# Budget constraint data
df_bc = data.frame(x_bc = seq(0, 20, 0.1))
df_bc = df_bc %>% mutate(y_bc = bc(x_bc))

# Ploting BC and IC
ggplot() +
  geom_line(data = df_bc, aes(x = x_bc, y = y_bc), color = "blue", size = 1.2) +
  geom_line(data = df_ic2, aes(x = x, y = y), color = 'orange', size = 1) +
  geom_line(data = df_ic1, aes(x = x, y = y), color = 'orange', size = 1) +
  geom_line(data = df_ic3, aes(x = x, y = y), color = 'orange', size = 1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  geom_point(aes(x = x_star, y = y_star), color = "black", size = 3) +  
  geom_segment(aes(x = x_star, xend = x_star, y = 0, yend = y_star), linetype = "dashed", color = "grey40") +  
  geom_segment(aes(x = 0, xend = x_star, y = y_star, yend = y_star), linetype = "dashed", color = "grey40") +  
  scale_x_continuous(
    breaks = c(0, 5, 10, 15, 20, round(x_star, 1)),
    labels = c("0", "5", "10", "15", "20", paste0("x* = ", round(x_star, 1)))
  ) +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, round(y_star, 1)),
    labels = c("0", "5", "10", "15", "20", paste0("y* = ", round(y_star, 1)))
  ) +
  annotate("text", x = head(df_ic1$x, 1), y = head(df_ic1$y, 1), label = "U1", hjust = 1.1, color = "black") +
  annotate("text", x = head(df_ic2$x, 1), y = head(df_ic2$y, 1), label = "U2", hjust = 1.1, color = "black") +
  annotate("text", x = head(df_ic3$x, 1), y = head(df_ic3$y, 1), label = "U3", hjust = 1.1, color = "black") +
  annotate("text", x = tail(df_bc$x_bc, 1), y = tail(df_bc$y_bc, 1), label = "RO", hjust = -0.1, color = "black") +
  labs(
    title = NULL,
    x = 'Good x',
    y = 'Good y'
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )
