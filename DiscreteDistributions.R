# 1. Let X ~ Bin(12, 1/3). Calculate:
# P(X = 0)
# P(X = 5)
# P(X <= 5)
# P(X < 8)
# P(X > 8)
# P(X >= 2)
# Find at least k for which P(X <= k) >= 1/2
# Find at least k for which P(X >= k) <= 1/3
# Evaluate EX and VarX through simulation.

n <- 12
p <- 1/3

simulation <- rbinom(10000, n, p)

prob_0 <- dbinom(0, n, p)
prob_5 <- dbinom(5, n, p)
prob_less_than_5 <- pbinom(5, n, p)
prob_less_than_8 <- pbinom(7, n, p)
prob_greater_than_8 <- 1 - pbinom(8, n, p)
prob_at_least_2 <- 1 - pbinom(1, n, p)

k_half <- qbinom(1/2, n, p)
k_third <- qbinom(2/3, n, p)

mean_sim <- mean(simulation)
var_sim <- var(simulation)

hist(simulation, breaks = seq(-0.5, n + 0.5, by = 1), col = "skyblue",
     xlab = "Value", ylab = "Frequency",
     main = "Simulation Histogram")

abline(v = c(0, 5, k_half, k_third), col = c("red", "green", "blue", "purple"), lty = 2)
legend("topright", legend = c("P(X=0)", "P(X=5)", "P(X<=k_half)", "P(X>=k_third)"),
       col = c("red", "green", "blue", "purple"), lty = 2)

cat("P(X = 0):", prob_0, "\n")
cat("P(X = 5):", prob_5, "\n")
cat("P(X <= 5):", prob_less_than_5, "\n")
cat("P(X < 8):", prob_less_than_8, "\n")
cat("P(X > 8):", prob_greater_than_8, "\n")
cat("P(X >= 2):", prob_at_least_2, "\n")
cat("P(X <= k) >= 1/2:", k_half, "\n")
cat("P(X >= k) <= 1/3:", k_third, "\n")
cat("Mean:", mean_sim, "\n")
cat("Variance:", var_sim, "\n")

# 2. Let X ~ Geom(1/5). Calculate:
# P(X = 1)
# P(X = 65)
# P(X <= 25)
# P(X < 38)
# P(X > 52)
# P(X >= 2)
# Find at least k 2 N for which P(X <= k) >= 1/2
# Find at least k 2 N for which P(X >= k) <= 1/3
# Evaluate EX and VarX through simulation.

p <- 1/5

simulation <- rgeom(10000, p)

prob_1 <- dgeom(1, p)
prob_65<- dgeom(65, p)
prob_less_than_25 <- pgeom(25, p)
prob_less_than_38 <- pgeom(37, p)
prob_greater_than_52 <- 1 - pgeom(52, p)
prob_at_least_2 <- 1 - pgeom(1, p)

k_half <- qgeom(1/2, p)
k_third <- qgeom(2/3, p)

mean_sim <- mean(simulation)
var_sim <- var(simulation)

hist(simulation, breaks = seq(-0.5, max(simulation) + 0.5, by = 1), col = "lightcoral",
     xlab = "Value", ylab = "Frequency",
     main = "Simulation Histogram")

abline(v = c(1, 65, k_half, k_third), col = c("red", "green", "blue", "purple"), lty = 2)
legend("topright", legend = c("P(X=1)", "P(X=65)", "P(X<=k_half)", "P(X>=k_third)"),
       col = c("red", "green", "blue", "purple"), lty = 2)

cat("P(X = 1):", prob_1, "\n")
cat("P(X = 65):", prob_65, "\n")
cat("P(X <= 25):", prob_less_than_25, "\n")
cat("P(X < 38):", prob_less_than_38, "\n")
cat("P(X > 52):", prob_greater_than_52, "\n")
cat("P(X >= 2):", prob_at_least_2, "\n")
cat("P(X<= k) >= 1/2:", k_half, "\n")
cat("P(X >= k) <= 1/3:", k_third, "\n")
cat("Mean:", mean_sim, "\n")
cat("Variance:", var_sim, "\n")

# 3. In a call center, the number of received calls during a day can be modeled by a Poisson 
# random variable. We know that on average 0.5% of the time the call center does not receive 
# any calls. What is the estimated number of calls during the day?

p <- 0.005
lambda <- -log(p)

simulation <- rpois(10000, lambda)
mean_calls <- mean(simulation)

cat("Average number of calls during the day:", lambda, "\n")
cat("Simulated number of calls during one day:", mean_calls, "\n")

hist(simulation, breaks = seq(-0.5, max(simulation) + 0.5, by = 1), col = "skyblue",
     xlab = "Number of calls", ylab = "Frequency",
     main = "Histogram of simulated calls during one day")
