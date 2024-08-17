
# Gini index

incomes <- c(10, 20, 30, 50, 80, 100, 150, 200)

# Load necessary library
library(ineq)

# Calculate and plot Lorenz curve
lorenz_curve <- Lc(incomes)  # Calculate Lorenz curve object
plot(lorenz_curve)           # Plot the Lorenz curve

# Calculate Gini coefficient
gini_coef <- ineq(incomes, type = "Gini")
cat("Gini Coefficient:", gini_coef, "\n")  # Display the result

# Step-by-step calculation of Area A (trapezoidal rule)
sorted_incomes <- sort(incomes)
n <- length(sorted_incomes)
cumulative_incomes <- cumsum(sorted_incomes)
cumulative_population_share <- seq(1/n, 1, 1/n)

# Trapezoidal Rule: The area under the Lorenz curve (and hence, area A) is approximated using the trapezoidal rule, where each segment is treated as a trapezoid.
area_A <- 0.5 - sum((cumulative_population_share[-1] - cumulative_population_share[-n]) * (cumulative_incomes[-1] + cumulative_incomes[-n])) / (2 * sum(incomes))
cat("Area A:", area_A, "\n")

# Calculate Gini coefficient using Area A
gini_coef_manual <- area_A / 0.5  # A / (A + B), where B = 0.5 - A
cat("Manual Gini Coefficient:", gini_coef_manual, "\n")
