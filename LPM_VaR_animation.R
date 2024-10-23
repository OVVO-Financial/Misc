# Load required packages
library(NNS)
library(ggplot2)
library(gganimate)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate data
N <- 1000
red <- rnorm(N, mean = 1.03, sd = 0.29)
green <- rnorm(N, mean = 1.18, sd = 0.33)
blue <- rnorm(N, mean = 1.92, sd = 0.54)

# Create a sequence of degrees for animation
degrees <- seq(0, 3, length.out = 50)

# Prepare a function to calculate LPM VaR for a given degree
calculate_LPM_VaR <- function(deg) {
  red_LPM_VaR <- NNS::LPM.VaR(percentile = seq(0, 1, length.out = 100), degree = deg, x = red)
  green_LPM_VaR <- NNS::LPM.VaR(percentile = seq(0, 1, length.out = 100), degree = deg, x = green)
  blue_LPM_VaR <- NNS::LPM.VaR(percentile = seq(0, 1, length.out = 100), degree = deg, x = blue)
  
  # Combine the data into a single data frame for plotting
  data <- data.frame(
    Value = c(red_LPM_VaR, green_LPM_VaR, blue_LPM_VaR),
    Color = rep(c("Red", "Green", "Blue"), each = length(red_LPM_VaR)),
    Degree = deg
  )
  
  return(data)
}

# Apply the function for each degree in the sequence
LPM_data <- do.call(rbind, lapply(degrees, calculate_LPM_VaR))

# Plot using ggplot2
p <- ggplot(LPM_data, aes(x = Value, fill = Color)) +
  geom_histogram(bins = 100, alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("Red" = "red", "Green" = "green", "Blue" = "blue")) +
  labs(title = 'LPM VaR Histograms: Degree = {round(frame_time, 2)}', x = 'LPM VaR Values', y = 'Density') +
  theme_minimal() +
  transition_time(Degree) +  # Animate over Degree
  ease_aes('linear')  # Smooth transition between frames

# Save the animation
animate(p, nframes = 100, fps = 10, width = 1000, height = 900)

# Optionally, save it as a gif
anim_save("LPM_VaR_animation.gif")
