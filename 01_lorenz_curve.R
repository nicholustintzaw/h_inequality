# Load necessary library
library(ineq)
library(dplyr)  # For data manipulation

################################################################################
# Prepare the dataset 
set.seed(123)  # Set seed for reproducibility

# Set A: Moderate inequality
set_A <- c(rep(1000, 5), rep(2000, 10), rep(3000, 15), rep(4000, 10), rep(5000, 5))

# Set B: Same mean and SD as Set A, but different distribution
set_B <- c(rep(500, 10), rep(2500, 15), rep(3500, 10), rep(4500, 5))

# Set C: Perfect equality
set_C <- rep(3000, 35)

# Set D: Extreme inequality (mostly concentrated at the top)
set_D <- c(rep(100, 30), 5000, 6000, 7000, 8000, 9000)

################################################################################
# (1): SumStat Table 
# Combine datasets into a data frame
df <- data.frame(
  Set = c(rep("A", length(set_A)), rep("B", length(set_B)), rep("C", length(set_C)), rep("D", length(set_D))),
  Income = c(set_A, set_B, set_C, set_D)
)

# Calculate summary statistics
summary_stats <- df %>%
  group_by(Set) %>%
  summarize(
    Mean = mean(Income),
    Median = median(Income),
    SD = sd(Income),
    Min = min(Income),
    Max = max(Income), 
    Percentile_80 = quantile(Income, 0.8)
  )

# Print the summary statistics table
print(summary_stats)

################################################################################
# (2): Lorenz Curve - example 

# Function to plot Lorenz curve
plot_lorenz <- function(data, title) {
  Lc_obj <- Lc(data)  # Calculate Lorenz curve object
  plot(Lc_obj, main = title, xlab = "Cumulative % of Population", ylab = "Cumulative % of Income")
  abline(0, 1, lty = 2)  # Add line of equality
}

# Plot Lorenz curves for each dataset
par(mfrow = c(2, 2))  # Set up 2x2 plotting grid
plot_lorenz(set_A, "Set A: Moderate Inequality")
plot_lorenz(set_B, "Set B: Same Mean/SD, Different Distribution")
plot_lorenz(set_C, "Set C: Perfect Equality")
plot_lorenz(set_D, "Set D: Extreme Inequality")

################################################################################
# (3): Lorenz curve point explination 

# Function to plot Lorenz curve with a point
plot_lorenz_with_point <- function(data, point, title) {
  Lc_obj <- Lc(data)  # Calculate Lorenz curve object
  plot(Lc_obj, main = title, xlab = "Cumulative % of Population", ylab = "Cumulative % of Income")
  abline(0, 1, lty = 2)  # Add line of equality
  
  # Add the specified point to the plot
  points(point[1], point[2], pch = 16, col = "red", cex = 1.2)  # Add red point
  
  # Add text label for interpretation
  text(point[1] + 0.05, point[2] - 0.05, paste0("(", point[1], ", ", point[2], "): ", point[2]*100, "% of income held by bottom ", point[1]*100, "% of population"), pos = 4)
}

# Plot Lorenz curve for Set A with the point (0.25, 0.10)
plot_lorenz_with_point(set_A, c(0.25, 0.125), "Set A: Moderate Inequality with Point (0.25, 0.125)")
