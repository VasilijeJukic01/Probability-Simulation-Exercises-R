# 1. Two dice are rolled. If we know that the sum of the received numbers is 8, find the conditional 
# probability that at least one of the numbers is even.

N <- 100000
counter1 <- 0
counter2 <- 0

for (i in 1:N) {
  first <- sample(1:6, 1, replace = TRUE)
  second <- sample(1:6, 1, replace = TRUE)
  
  if (first + second == 8) {
    counter1 <- counter1 + 1
    if (first %% 2 == 0 || second %% 2 == 0) {
      counter2 <- counter2 + 1
    }
  }
}

conditional_probability <- counter2 / counter1
cat("Conditional_probability:", conditional_probability, "\n")

# 2. A coin is tossed three times. What is the probability that in the second toss a Letter 
# fell, if we know that at least one Letter fell in total?

N <- 100000
counter1 <- 0
counter2 <- 0

for (i in 1:N) {
  first <- sample(c("Head", "Letter"), 1, replace = TRUE)
  second <- sample(c("Head", "Letter"), 1, replace = TRUE)
  three <- sample(c("Head", "Letter"), 1, replace = TRUE)
  
  if ("Letter" %in% c(first, second, three)) {
    counter1 <- counter1 + 1
    if (second == "Letter") {
      counter2 <- counter2 + 1
    }
  }
}

conditional_probability <- counter2 / counter1
cat("Conditional_probability:", conditional_probability, "\n")

# 3. What is the probability that a randomly chosen number between 1 and 100 is divisible 
# by 3, if we know that at least one of its digits is 5?

B <- grep("5", as.character(1:100), value = TRUE)
A <- sum(as.numeric(B) %% 3 == 0)

conditional_probability <- A / length(B)
cat("Conditional_probability:", conditional_probability, "\n")

# 4. There are two boxes. In the first box there are two balls marked with 1 and 2. 
# In the second box there are three balls, marked with 3, 4 and 5. We choose one box at random 
# (with equal probabilities) and then from that box we choose again at random one ball . What is 
# the probability that we will pick ball number 5?

N <- 100000
counter <- 0

for (i in 1:N) {
  chosen_box <- sample(1:2, 1)
  
  if (chosen_box == 1)  chosen_ball <- sample(1:2, 1)
  else chosen_ball <- sample(3:5, 1)
  
  if (chosen_ball == 5) counter <- counter + 1
}

conditional_probability <- counter / N
cat("Conditional_probability:", conditional_probability, "\n")

# 5. There are two boxes. In the first box there are three balls marked with 1, 2 and 3. 
# In the second box there are four balls, marked with 2, 3, 4 and 5. We choose one box at random 
# so that the probability of choosing the first box is 1/5 and the probability of choosing other boxes 4/5. 
# Then we choose one ball from that box (uniformly at random). 
# a) What is the probability that we will choose the ball marked with the number 2?
# b) Suppose we chose the ball marked with the number 2. What is the probability that it was drawn from second box?

# a)
N <- 100000
counter <- 0

for (i in 1:N) {
  chosen_box <- ifelse(runif(1) <= 1/5, "B1", "B2")
  
  if (chosen_box == "B1") chosen_ball <- sample(1:3, 1)
  else chosen_ball <- sample(2:5, 1)
  
  if (chosen_ball == 2) counter <- counter + 1
}

conditional_probability <- counter / N
cat("Conditional_probability:", conditional_probability, "\n")

# b)
N <- 100000
counter1 <- 0
counter2 <- 0

for (i in 1:N) {
  chosen_box <- ifelse(runif(1) <= 1/5, "B1", "B2")
  
  if (chosen_box == "B1") chosen_ball <- sample(1:3, 1)
  else {
    chosen_ball <- sample(2:5, 1)
    if (chosen_ball == 2) 
      counter2 <- counter2 + 1
  }
  
  if (chosen_ball == 2)
    counter1 <- counter1 + 1
}

conditional_probability <- counter2 / counter1
cat("Conditional_probability:", conditional_probability, "\n")

# 6. We have a bag of 3 dice. One is four-sided, one is six-sided and one is 12-sided. 
# We open the bag, take out one die at random and throw it away. We got the number 4. What is 
# the probability that we rolled a six-sided die?

N <- 100000
counter1 <- 0
counter2 <- 0

for (i in 1:N) {
  chosen <- sample(c(4, 6, 12), 1, prob = c(1/3, 1/3, 1/3))
  roll_result <- sample(1:chosen, 1)
  
  if (roll_result == 4) {
    counter1 <- counter1 + 1
    if (chosen == 6)
      counter2 <- counter2 + 1
  }
}

conditional_probability <- counter2 / counter1
cat("Conditional_probability:", conditional_probability, "\n")

# 7. Insurance company Marta has two types of customers, careful and careless. A careful 
# client experiences an accident within one year with a probability of 0.01. A careless client 
# experiences an accident within one year with a probability of 0.04. 80% of all clients are careful and 
# 20% are careless. Suppose a randomly selected client has an accident during this year. What is the 
# probability that it is one of the careful ones?

N <- 100000
counter1 <- 0
counter2 <- 0

for (i in 1:N) {
  client <- ifelse(runif(1) <= 0.8, "careful", "careless")
  
  if (client == "careful") {
    if (runif(1) <= 0.01)
      counter2 <- counter2 + 1
  } 
  else {
    if (runif(1) <= 0.04)
      counter1 <- counter1 + 1
  }
}

conditional_probability <- counter2 / (counter2 + counter1)
cat("Conditional_probability:", conditional_probability, "\n")

