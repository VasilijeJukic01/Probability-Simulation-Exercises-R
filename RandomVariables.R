# 1. We toss a coin five times. For every fallen Head you pay me 1 euro, and for every fallen Letter 
# I pay you 1 euro. Let X represent MY winnings at the end of the game.
# Determine the possible values of X and its probability distribution law.

simulate_game <- function() {
  outcome <- sample(c("Head", "Letter"), 5, replace = TRUE)
  my_winnings <- sum(outcome == "Head") - sum(outcome == "Letter")
  return(my_winnings)
}

simulation_results <- numeric(1000)

for (i in 1:1000) {
  simulation_results[i] <- simulate_game()
}

hist(simulation_results, main = "Histogram", xlab = "Win", ylab = "Simulations")

# Distribution
possible_values <- c(-5, -3, -1, 1, 3, 5)
real_probabilities <- c(1/32, 5/32, 10/32, 10/32, 5/32, 1/32)

# Real values
points(possible_values, real_probabilities * 1000, col = "red")

# 2. There are 4 red and 3 green balls in one box. 2 balls are drawn randomly. Let Z represent 
# the number of green balls in the sample, when they are drawn without return. Determine the possible 
# values for Z and its probability distribution law.

simulate_draw <- function() {
  box <- c("red", "red", "red", "red", "green", "green", "green")
  sample_result <- sample(box, 2, replace = FALSE)
  count_green <- sum(sample_result == "green")
  return(count_green)
}

simulation_results <- numeric(1000)

for (i in 1:1000) {
  simulation_results[i] <- simulate_draw()
}

table_results <- table(simulation_results)
barplot(table_results, main = "Green balls histogram", xlab = "Green balls number", ylab = "Frequency")

# 3. There are 16 letters in the sentence SOME DOGS ARE BROWN. Choose one of these letters at random. 
# Let X represent the length of the string containing that letter. Determine the possible values and the 
# probability distribution law of X.

sentence <- "SOME DOGS ARE BROWN"
letters <- gsub(" ", "", strsplit(sentence, "")[[1]])

simulation_results <- numeric(1000)

for (i in 1:1000) {
  selected_letter <- sample(letters, 1)
  
  words_with_selected_letter <- grep(selected_letter, strsplit(sentence, " ")[[1]], value = TRUE)
  length_of_words <- nchar(words_with_selected_letter)
  
  simulation_results[i] <- length_of_words
}

table_results <- table(simulation_results)
barplot(table_results, main = "Word length histogram", xlab = "Word length", ylab = "Frequency")

# 4. A team of three representatives is selected from one office where 2 men and 4 women work. Let 
# X be the number of women on that team. Find the law of distribution of X.

simulate_team_selection <- function() {
  m_number <- 2
  f_number <- 4
  
  team <- sample(c("M", "F"), 3, replace = TRUE, prob = c(m_number/(m_number+f_number), f_number/(m_number+f_number)))
  while (sum(team == "F") == 0) {
    team <- sample(c("M", "F"), 3, replace = TRUE, prob = c(m_number/(m_number+f_number), f_number/(m_number+f_number)))
  }
  
  females_in_team <- sum(team == "F")
  return(females_in_team)
}

simulation_results <- numeric(1000)

for (i in 1:1000) {
  simulation_results[i] <- simulate_team_selection()
}

table_results <- table(simulation_results)
barplot(table_results, main = "Females in team histogram", xlab = "Number of females", ylab = "Frequency")

# 5. Let X be a discrete random variable with values {0, 1, ...} and with the following distribution 
# law: P(X = 0) = 4/5 and for k = 1, 2, 3, ...P (X = k) = 1/10*(2/3)^k. Check that this is really a 
# law of probability distribution (that is, that the sum of all possible p(k) is equal to 1. 
# Calculating the sum of the geometric series. Using a calculator, find the sum of p(0)+.....+p(k) for 
# some large values of k. Plot the obtained sums.

probability_function <- function(k) {
  if (k == 0) return(4/5)
  else return((1/10) * (2/3)^k)
}

check_probability_sum <- function(max_k) {
  sum_probabilities <- 0
  for (k in 0:max_k) {
    sum_probabilities <- sum_probabilities + probability_function(k)
  }
  
  return(sum_probabilities)
}

max_k <- 1000
sum_of_probabilities <- check_probability_sum(max_k)

cat("Probability sum for k =", max_k, ":", sum_of_probabilities)

# Plot
k_values <- 0:max_k
sum_values <- sapply(k_values, function(k) check_probability_sum(k))

plot(k_values, sum_values, type = "l", col = "red", lwd = 2,
     main = "Probability sum P(X=k)", xlab = "k", ylab = "Probability sum")


# 6. In one city, 20% of the population has blonde hair. One day a visitor enters a local 
# cafe where he finds only 14 locals.
# a) What is the probability that this visitor finds exactly 10 people with blond hair?
# b) What is the probability that that visitor finds at most two people with blond hair?

library(ggplot2)

p <- 0.20
n <- 14

# a)
x_10 <- rbinom(10000, n, p)
x_10_data_frame <- data.frame(number_of_people = x_10)

# b)
x_le_2 <- rbinom(10000, n, p)
x_le_2_data_frame <- data.frame(number_of_people = x_le_2)

ggplot(x_10_data_frame, aes(x = number_of_people)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Distribution of people with blond hair (X = 10)",
       x = "Number of people with blond hair",
       y = "Frequency") +
  theme_minimal()

ggplot(x_le_2_data_frame, aes(x = number_of_people)) +
  geom_bar(stat = "count", fill = "lightcoral", color = "black") +
  labs(title = "Distribution of people with blond hair (X <= 10)",
       x = "Number of people with blond hair",
       y = "Frequency") +
  theme_minimal()

# 7. A clinical study tests a new drug on a sample of 80 patients. This drug is expected to 
# be effective on each of the patients with probability p and independently of the others. 
# You have two friends in that sample. If we know that the drug was successful in 55 patients, 
# what is the probability that your 2 friends are among them?

library(ggplot2)

n <- 80
p <- 55 / n

success <- rbinom(10000, n, p)
both_friends_success <- sum(success == 2) / 10000

cat("Probability that both friends are among the successful patients:", both_friends_success, "\n")

ggplot(data = data.frame(success), aes(x = success)) +
  geom_bar(stat = "count", width = 0.7, fill = "skyblue", color = "black") +
  labs(title = "Distribution of successful patients",
       x = "Number of successful patients",
       y = "Frequency") +
  theme_minimal()

