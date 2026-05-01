# Blackjack Simulation
# Semesterarbeit Wahrscheinlichkeit FS26


# Step 1: Create Deck
one_suit <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 11)
deck <- rep(one_suit, times = 4)

# Step 2: Calculate Hand Sum
calculate_sum <- function(hand) {
  total <- sum(hand)
  aces <- sum(hand == 11)
  
  # if bust and ace in hand: count ace as 1 instead of 11
  while (total > 21 && aces > 0) {
    total <- total - 10
    aces <- aces - 1
  }
  
  return(total)
}

# Step 3: Play One Round
play_round <- function(strategy = "basic") {
  
  # shuffle deck and deal 2 cards each
  shuffled <- sample(deck)
  player_hand <- shuffled[1:2]
  dealer_hand <- shuffled[3:4]
  
  # calculate starting sums
  player_sum <- calculate_sum(player_hand)
  dealer_sum <- calculate_sum(dealer_hand)
  
  # player decision based on strategy
  if (strategy == "basic") {
    # hit if under 17, stand at 17 or above
    while (player_sum < 17) {
      player_hand <- c(player_hand, sample(deck, 1) )
      player_sum  <- calculate_sum(player_hand)
    }
    
  } else if (strategy == "random") {
    # hit or stand randomly (50/50)
    while (player_sum < 21) {
      if (sample(c(TRUE, FALSE), 1)) {
        player_hand <- c(player_hand, sample(deck, 1))
        player_sum  <- calculate_sum(player_hand)
      } else {
        break
      }
    }
  }
  
  # dealer logic: hits until 17 (done by teammate)
  # ...
  
  # compare and return result (done by teammate)
  # ...
  
}