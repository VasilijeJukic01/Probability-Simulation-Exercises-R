# 1. 5 cards are dealt from a deck of 52 cards. What is the probability that all 5 cards are hertz?

N <- 10000
card_number <- 52
draw_number <- 5
hertz_number <- 13

counter <- 0

for (i in 1:N) {
  draw <- sample(1:card_number, draw_number, replace = FALSE)
  if(all(draw <= hertz_number)) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N
real_probability <- choose(hertz_number, draw_number) / choose(card_number, draw_number)

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", real_probability, "\n")

# 2. 5 cards are dealt from a deck of 52 cards. What is the probability that 3 cards are of the same sign 
# and the other 2 cards are of the same sign?

N <- 10000
card_number <- 52
draw_number <- 5

counter <- 0

for (i in 1:N) {
  draw <- sample(1:card_number, draw_number, replace = FALSE)
  hertz <- sum(draw <= 13)
  club <- sum(draw >= 14 & draw <= 26)
  if (hertz == 3 && club == 2) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N
real_probability <- (choose(13, 3) * choose(13, 2)) / choose(card_number, draw_number)

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", real_probability, "\n")

# 3. A bouquet of 7 flowers should be made from 15 tulips and 20 roses. How many different bouquets can 
# we make? What is the probability that there will be 4 tulips and 3 roses in that bouquet?

N <- 10000
tulips_number <- 15
roses_number <- 20
bouquet_flowers <- 7

counter <- 0

for (i in 1:N) {
  draw <- sample(c(rep("tulip", tulips_number), rep("rose", roses_number)), bouquet_flowers, replace = FALSE)
  tulips_in_bouquet <- sum(draw == "tulip")
  roses_in_bouquet <- bouquet_flowers - tulips_in_bouquet
  if (tulips_in_bouquet == 4 && roses_in_bouquet == 3) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N
real_probability <- (choose(tulips_number, 4) * choose(roses_number, 3)) / choose(tulips_number + roses_number, bouquet_flowers)

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", real_probability, "\n")

# 4. What is the probability that in a group of n people at least two have the same birthday? 
# And for which at least n is this probability greater than 0.5?

N <- 10000
n <- 23
counter <- 0

for (i in 1:N) {
  bdays <- sample(1:365, n, replace = TRUE)
  if (any(duplicated(bdays))) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N

p_no_same_bdays <- 1
for (i in 1:n) {
  p_no_same_bdays <- p_no_same_bdays * (365 - i + 1) / 365
}

p_min_two_same_bday <- 1 - p_no_same_bdays

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", p_min_two_same_bday, "\n")

# 5. At the entrance to the cinema, 15 people lined up in a random manner are waiting in line, 
# and Jelena and Petar are among them. What is the probability that the two will be standing next to each other?

N <- 10000
people_number <- 15
counter <- 0

for (i in 1:N) {
  pos <- sample(1:people_number, replace = FALSE)
  jelena_index <- which(pos == 1)
  petar_index <- which(pos == 2)
  if (abs(jelena_index - petar_index) == 1) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N
real_probability <- 2 / (people_number - 1)

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", real_probability, "\n")

# 6. 5 cards are dealt from a deck of 52 cards. What is the probability that there are exactly two aces among them?

N <- 10000
card_number <- 52
draw_number <- 5
aces_number <- 4

counter <- 0

for (i in 1:N) {
  draw <- sample(1:card_number, draw_number, replace = FALSE)
  aces <- sum(draw <= aces_number)
  if (aces == 2) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N
real_probability <- choose(aces_number, 2) * choose(card_number - aces_number, draw_number - 2) / choose(card_number, draw_number)

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", real_probability, "\n")

# 7. 5 cards are dealt from a deck of 52 cards. What is the probability that there are at least two aces among them?

N <- 10000
card_number <- 52
draw_number <- 5
aces_number <- 4

counter <- 0

for (i in 1:N) {
  draw <- sample(1:card_number, draw_number, replace = FALSE)
  aces <- sum(draw <= aces_number)
  if (aces >= 2) {
    counter <- counter + 1
  }
}

simulation_probability <- counter / N

two_aces <- choose(4, 2) * choose(48, 3)
three_aces <- choose(4, 3) * choose(48, 2)
four_aces <- choose(4, 4) * choose(48, 1)
total_cases <- two_aces + three_aces + four_aces

total_combinations <- choose(52, 5)

real_probability <- total_cases / total_combinations

cat("Approximate probability (simulation):", simulation_probability, "\n")
cat("Real probability:", real_probability, "\n")

# 8. Two dice are rolled simultaneously. Determine the probability that
# a) the product of the resulting numbers is 8.
# b) the sum of the numbers obtained is greater than the product.

N <- 10000
counter_a <- 0
counter_b <- 0

for (i in 1:N) {
  first <- sample(1:6, 1)
  second <- sample(1:6, 1)
  
  multiplication <- first * second
  sum <- first + kocka2
  
  # (a)
  if (multiplication == 8) {
    counter_a <- counter_a + 1
  }
  
  # (b)
  if (sum > multiplication) {
    counter_b <- counter_b + 1
  }
}

simulation_probability_a <- counter_a / N
simulation_probability_b <- counter_b / N

cat("a) Approximate probability (simulation):", simulation_probability_a, "\n")
cat("b) Approximate probability (simulation):", simulation_probability_b, "\n")
