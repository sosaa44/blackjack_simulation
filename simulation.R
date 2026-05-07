# Blackjack Monte Carlo Simulation
# ZHAW — Semesterarbeit Wahrscheinlichkeitsrechnung FS26
#
# Research Question:
# Which Blackjack strategy survives the longest —
# and how likely is it to beat the casino?

library(ggplot2)

# ── Step 1: Create Deck ───────────────────────────────────────
one_suit <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 11)
deck <- rep(one_suit, times = 4)

# ── Step 2: Calculate Hand Sum ────────────────────────────────
calculate_sum <- function(hand) {
  total <- sum(hand)
  aces <- sum(hand == 11)
  while (total > 21 && aces > 0) {
    total <- total - 10
    aces <- aces - 1
  }
  total
}

# ── Step 3: Play One Round ────────────────────────────────────
play_round <- function(strategy = "basic") {
  shuffled <- sample(deck)
  player_hand <- shuffled[1:2]
  dealer_hand <- shuffled[3:4]
  player_sum <- calculate_sum(player_hand)
  dealer_sum <- calculate_sum(dealer_hand)
  
  if (strategy == "basic" || strategy == "martingale") {
    while (player_sum < 17) {
      player_hand <- c(player_hand, sample(deck, 1))
      player_sum <- calculate_sum(player_hand)
    }
  } else if (strategy == "aggressive") {
    while (player_sum < 19) {
      player_hand <- c(player_hand, sample(deck, 1))
      player_sum <- calculate_sum(player_hand)
    }
  } else if (strategy == "random") {
    while (player_sum < 21) {
      if (sample(c(TRUE, FALSE), 1)) {
        player_hand <- c(player_hand, sample(deck, 1))
        player_sum <- calculate_sum(player_hand)
      } else break
    }
  }
  
  while (dealer_sum < 17) {
    dealer_hand <- c(dealer_hand, sample(deck, 1))
    dealer_sum <- calculate_sum(dealer_hand)
  }
  
  if (player_sum > 21) return("loss")
  if (dealer_sum > 21) return("win")
  if (player_sum > dealer_sum) return("win")
  if (player_sum < dealer_sum) return("loss")
  "draw"
}

# ── Step 4: Simulate One Player ───────────────────────────────
simulate_player <- function(strategy = "basic",
                            start_capital = 100,
                            bet = 5,
                            max_rounds = 200) {
  capital <- start_capital
  rounds <- 0
  current_bet <- bet
  capital_history <- c(start_capital)
  
  while (capital >= current_bet && rounds < max_rounds) {
    if (strategy == "martingale") {
      result <- play_round("martingale")
      if (result == "win") {
        capital <- capital + current_bet
        current_bet <- bet
      } else if (result == "loss") {
        capital <- capital - current_bet
        current_bet <- min(current_bet * 2, capital)
      }
    } else {
      result <- play_round(strategy)
      if (result == "win") capital <- capital + bet
      if (result == "loss") capital <- capital - bet
    }
    
    rounds <- rounds + 1
    capital_history <- c(capital_history, capital)
  }
  
  list(
    final_capital = capital,
    rounds_played = rounds,
    capital_history = capital_history
  )
}

# ── Step 5: Monte Carlo Simulation ────────────────────────────
set.seed(42)
n <- 10000
strategies <- c("basic", "aggressive", "random", "martingale")

results <- data.frame()
example_histories <- list()

for (strat in strategies) {
  cat("Simulating:", strat, "\n")
  final_capitals <- numeric(n)
  rounds_played <- numeric(n)
  
  for (i in 1:n) {
    sim <- simulate_player(strat)
    final_capitals[i] <- sim$final_capital
    rounds_played[i] <- sim$rounds_played
    if (i == 1) example_histories[[strat]] <- sim$capital_history
  }
  
  results <- rbind(results, data.frame(
    strategy = strat,
    capital = final_capitals,
    rounds_played = rounds_played
  ))
}

# ── Step 6: Summary Table ─────────────────────────────────────
summary_stats <- data.frame(
  Strategy            = tapply(results$capital, results$strategy, mean) |> names(),
  Avg_Capital         = round(tapply(results$capital, results$strategy, mean), 1),
  Bankruptcy_Rate     = round(tapply(results$capital == 0, results$strategy, mean) * 100, 1),
  Win_Probability     = round(tapply(results$capital > 100, results$strategy, mean) * 100, 1),
  Avg_Rounds_Survived = round(tapply(results$rounds_played, results$strategy, mean), 1)
)
print(summary_stats)

# ── Step 7: Visualisations ────────────────────────────────────

# Plot 1: Final capital distribution per strategy
p1 <- ggplot(results, aes(x = capital)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~strategy, scales = "free_y") +
  labs(title    = "Final Capital Distribution",
       subtitle = "Green dashed line = starting capital (CHF 100)",
       x = "Final Capital (CHF)", y = "Density") +
  theme_minimal()

# Plot 2: Bankruptcy rate (Bernoulli outcome per player)
p2 <- ggplot(summary_stats, aes(x = Strategy, y = Bankruptcy_Rate, fill = Strategy)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Bankruptcy_Rate, "%")), vjust = -0.5, size = 4) +
  labs(title    = "Bankruptcy Rate per Strategy",
       subtitle = "Share of players who ended with 0 CHF",
       x = "Strategy", y = "Bankruptcy Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 3: Average rounds survived (Geometric distribution)
p3 <- ggplot(summary_stats, aes(x = Strategy, y = Avg_Rounds_Survived, fill = Strategy)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Avg_Rounds_Survived), vjust = -0.5, size = 4) +
  labs(title    = "Average Rounds Survived per Strategy",
       subtitle = "Higher values mean longer survival",
       x = "Strategy", y = "Average Rounds") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 4: Capital path of one example player per strategy
plot4_data <- data.frame()
for (strat in names(example_histories)) {
  h <- example_histories[[strat]]
  plot4_data <- rbind(plot4_data, data.frame(
    round    = 0:(length(h) - 1),
    capital  = h,
    strategy = strat
  ))
}

p4 <- ggplot(plot4_data, aes(x = round, y = capital, color = strategy)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  facet_wrap(~strategy, ncol = 2) +
  labs(title    = "Capital Path: One Example Player per Strategy",
       subtitle = "Dashed line = starting capital (CHF 100)",
       x = "Round", y = "Capital (CHF)") +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)