# Blackjack Strategy as a Bernoulli Process
# ZHAW — Semesterarbeit Wahrscheinlichkeitsrechnung FS26
#
# Fragestellung: Welche Strategie hat die höchste Gewinnwahrscheinlichkeit über 200 Runden?

library(ggplot2)

# Deck aufbauen: 13 Werte pro Farbe, 4 Farben = 52 Karten
# J/Q/K zählen als 10, Ass als 11
one_suit <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 11)
deck <- rep(one_suit, times = 4)

# Handsumme berechnen
# Ass auf 1 reduzieren wenn Bust, solange nötig
calculate_sum <- function(hand) {
  total <- sum(hand)
  aces <- sum(hand == 11)
  while (total > 21 && aces > 0) {
    total <- total - 10
    aces <- aces - 1
  }
  total
}

# Eine Runde spielen, Ergebnis als Bernoulli-Variable zurückgeben
# 1 = Gewinn, 0 = alles andere (Verlust oder Unentschieden)
play_round <- function(strategy = "basic") {
  shuffled <- sample(deck)
  player_hand <- shuffled[1:2]
  dealer_hand <- shuffled[3:4]
  player_sum <- calculate_sum(player_hand)
  dealer_sum <- calculate_sum(dealer_hand)

  if (strategy == "basic") {
    # unter 17 ziehen, ab 17 stehen
    while (player_sum < 17) {
      player_hand <- c(player_hand, sample(deck, 1))
      player_sum <- calculate_sum(player_hand)
    }
  } else if (strategy == "martingale") {
    # gleiche Hit/Stand-Logik wie basic
    # Einsatzanpassung passiert in simulate_player()
    while (player_sum < 17) {
      player_hand <- c(player_hand, sample(deck, 1))
      player_sum <- calculate_sum(player_hand)
    }
  } else if (strategy == "aggressive") {
    # erst ab 19 stehen
    while (player_sum < 19) {
      player_hand <- c(player_hand, sample(deck, 1))
      player_sum <- calculate_sum(player_hand)
    }
  } else if (strategy == "random") {
    # zufällig ziehen oder stehen
    while (player_sum < 21) {
      if (sample(c(TRUE, FALSE), 1)) {
        player_hand <- c(player_hand, sample(deck, 1))
        player_sum <- calculate_sum(player_hand)
      } else break
    }
  }

  # Dealer zieht immer bis 17
  while (dealer_sum < 17) {
    dealer_hand <- c(dealer_hand, sample(deck, 1))
    dealer_sum <- calculate_sum(dealer_hand)
  }

  # Ergebnis auswerten
  if (player_sum > 21) return(0)  # Spieler bust
  if (dealer_sum > 21) return(1)  # Dealer bust
  if (player_sum > dealer_sum) return(1)
  return(0)  # Verlust oder Unentschieden
}

# Einen Spieler über n_rounds Runden simulieren
# Martingale: Einsatz nach Verlust verdoppeln, nach Gewinn zurücksetzen
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
        current_bet <- bet  # Einsatz zurücksetzen
        wins <- wins + 1
      } else {
        capital <- capital - current_bet
        current_bet <- current_bet * 2  # Einsatz verdoppeln
        if (current_bet > capital) current_bet <- max(capital, bet)  # nicht ins Minus
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

# Monte-Carlo: 10'000 Spieler pro Strategie, je 200 Runden
set.seed(42)
n <- 10000
n_rounds <- 200
strategies <- c("basic", "aggressive", "random", "martingale")

results <- data.frame()
avg_paths <- list()

for (strat in strategies) {
  cat("Simuliere:", strat, "\n")
  final_capitals <- numeric(n)
  win_rates      <- numeric(n)
  all_paths      <- matrix(0, nrow = n, ncol = n_rounds + 1)

  for (i in 1:n) {
    sim <- simulate_player(strat, n_rounds = n_rounds)
    final_capitals[i] <- sim$final_capital
    win_rates[i]      <- sim$win_rate
    all_paths[i, ]    <- sim$capital_history
  }

  # Durchschnittlichen Kapitalverlauf über alle Spieler berechnen
  avg_paths[[strat]] <- colMeans(all_paths)

  results <- rbind(results, data.frame(
    strategy = strat,
    capital  = final_capitals,
    win_rate = win_rates
  ))
}

# Kennzahlen zusammenfassen
summary_stats <- data.frame(
  Strategie     = tapply(results$win_rate, results$strategy, mean) |> names(),
  Gewinnwahrsch = round(tapply(results$win_rate, results$strategy, mean) * 100, 1),
  Avg_Capital   = round(tapply(results$capital, results$strategy, mean), 1),
  Pct_above_100 = round(tapply(results$capital > 100, results$strategy, mean) * 100, 1)
)
colnames(summary_stats) <- c("Strategie", "Ø Gewinnwahrsch. (%)",
                             "Ø Endkapital (CHF)", "Anteil > 100 CHF (%)")
print(summary_stats)

# Visualisierungen

# Plot 1: Gewinnwahrscheinlichkeit pro Strategie
p1 <- ggplot(summary_stats, aes(x = Strategie, y = `Ø Gewinnwahrsch. (%)`, fill = Strategie)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(`Ø Gewinnwahrsch. (%)`, "%")), vjust = -0.5, size = 4) +
  labs(title    = "Geschätzte Gewinnwahrscheinlichkeit pro Strategie",
       subtitle = "Bernoulli-Parameter p: Anteil gewonnener Runden über 200 Runden",
       x = "Strategie", y = "Gewinnwahrscheinlichkeit (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Endkapitalverteilung pro Strategie
p2 <- ggplot(results, aes(x = capital)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~strategy, scales = "free_y") +
  labs(title    = "Empirische Endkapitalverteilung pro Strategie",
       subtitle = "Grüne Linie = Startkapital CHF 100",
       x = "Endkapital (CHF)", y = "Dichte") +
  theme_minimal()

# Plot 3: Durchschnittlicher Kapitalverlauf über 200 Runden
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
