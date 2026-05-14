# Blackjack Strategy as a Bernoulli Process
# ZHAW — Semesterarbeit Wahrscheinlichkeitsrechnung FS26
#
# Research Question:
# Which Blackjack strategy achieves the highest win probability over 200 rounds?

library(ggplot2)

# ── Step 1: Create Deck ───────────────────────────────────────
# 13 card values per suit (J/Q/K = 10, Ace = 11), 4 suits = 52 cards
one_suit <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 11)
deck <- rep(one_suit, times = 4)

# ── Step 2: Calculate Hand Sum ────────────────────────────────
# Ace = 11, switches to 1 if bust
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
# Returns 1 (win) or 0 (non-win) — Bernoulli trial
# draw counts as non-win (0)
play_round <- function(strategy = "basic") {
  shuffled <- sample(deck)
  player_hand <- shuffled[1:2]
  dealer_hand <- shuffled[3:4]
  player_sum <- calculate_sum(player_hand)
  dealer_sum <- calculate_sum(dealer_hand)
  
  if (strategy == "basic") {
    # Basic: hit below 17, stand at 17+
    while (player_sum < 17) {
      player_hand <- c(player_hand, sample(deck, 1))
      player_sum <- calculate_sum(player_hand)
    }
  } else if (strategy == "martingale") {
    # Martingale uses same hit/stand as basic
    # bet doubling is handled in simulate_player()
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
  
  # Bernoulli outcome: win = 1, everything else = 0
  if (player_sum > 21) return(0)
  if (dealer_sum > 21) return(1)
  if (player_sum > dealer_sum) return(1)
  return(0)  # loss or draw
}

# ── Step 4: Simulate One Player ───────────────────────────────
# Every player plays exactly 200 rounds (fixed)
# Martingale: same hit/stand as basic, but doubles bet after loss
simulate_player <- function(strategy = "basic",
                            start_capital = 100,
                            bet = 5,
                            n_rounds = 200) {
  capital <- start_capital
  current_bet <- bet
  capital_history <- c(start_capital)
  wins <- 0
  
  for (r in 1:n_rounds) {
    result <- play_round(strategy)
    
    if (strategy == "martingale") {
      if (result == 1) {
        capital <- capital + current_bet
        current_bet <- bet          # reset bet after win
        wins <- wins + 1
      } else {
        capital <- capital - current_bet
        current_bet <- current_bet * 2  # double bet after loss
        # cap bet at remaining capital to avoid negative
        if (current_bet > capital) current_bet <- max(capital, bet)
      }
    } else {
      if (result == 1) {
        capital <- capital + bet
        wins <- wins + 1
      } else {
        capital <- capital - bet
      }
    }
    
    capital_history <- c(capital_history, capital)
  }
  
  list(
    final_capital   = capital,
    wins            = wins,
    win_rate        = wins / n_rounds,
    capital_history = capital_history
  )
}

# ── Step 5: Monte Carlo Simulation ────────────────────────────
# 10,000 players per strategy, each plays exactly 200 rounds
set.seed(42)
n <- 10000
n_rounds <- 200
strategies <- c("basic", "aggressive", "random", "martingale")

results <- data.frame()
avg_paths <- list()

for (strat in strategies) {
  cat("Simulating:", strat, "\n")
  final_capitals <- numeric(n)
  win_rates      <- numeric(n)
  all_paths      <- matrix(0, nrow = n, ncol = n_rounds + 1)
  
  for (i in 1:n) {
    sim <- simulate_player(strat, n_rounds = n_rounds)
    final_capitals[i] <- sim$final_capital
    win_rates[i]      <- sim$win_rate
    all_paths[i, ]    <- sim$capital_history
  }
  
  # Average capital path across all 10,000 players
  avg_paths[[strat]] <- colMeans(all_paths)
  
  results <- rbind(results, data.frame(
    strategy  = strat,
    capital   = final_capitals,
    win_rate  = win_rates
  ))
}

# ── Step 6: Summary Table ─────────────────────────────────────
summary_stats <- data.frame(
  Strategie          = tapply(results$win_rate, results$strategy, mean) |> names(),
  Gewinnwahrsch      = round(tapply(results$win_rate, results$strategy, mean) * 100, 1),
  Avg_Capital        = round(tapply(results$capital, results$strategy, mean), 1),
  Pct_above_100      = round(tapply(results$capital > 100, results$strategy, mean) * 100, 1)
)
colnames(summary_stats) <- c("Strategie", "Ø Gewinnwahrsch. (%)",
                             "Ø Endkapital (CHF)", "Anteil > 100 CHF (%)")
print(summary_stats)

# ── Step 7: Visualisations ────────────────────────────────────

# Plot 1: Win probability per strategy (Bernoulli p estimate)
p1 <- ggplot(summary_stats, aes(x = Strategie, y = `Ø Gewinnwahrsch. (%)`, fill = Strategie)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(`Ø Gewinnwahrsch. (%)`, "%")), vjust = -0.5, size = 4) +
  labs(title    = "Geschätzte Gewinnwahrscheinlichkeit pro Strategie",
       subtitle = "Bernoulli-Parameter p: Anteil gewonnener Runden über 200 Runden",
       x = "Strategie", y = "Gewinnwahrscheinlichkeit (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Final capital distribution per strategy
p2 <- ggplot(results, aes(x = capital)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~strategy, scales = "free_y") +
  labs(title    = "Empirische Endkapitalverteilung pro Strategie",
       subtitle = "Grüne Linie = Startkapital CHF 100",
       x = "Endkapital (CHF)", y = "Dichte") +
  theme_minimal()

# Plot 3: Average capital path over 200 rounds
avg_path_data <- data.frame()
for (strat in names(avg_paths)) {
  avg_path_data <- rbind(avg_path_data, data.frame(
    round    = 0:n_rounds,
    capital  = avg_paths[[strat]],
    strategy = strat
  ))
}

p3 <- ggplot(avg_path_data, aes(x = round, y = capital, color = strategy)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  labs(title    = "Durchschnittlicher Kapitalverlauf über 200 Runden",
       subtitle = "Gestrichelte Linie = Startkapital CHF 100 | Mittelwert über 10'000 Spieler",
       x = "Runde", y = "Durchschnittliches Kapital (CHF)") +
  theme_minimal()

print(p1)
print(p2)
print(p3)
