# Blackjack Strategies as a Simplified Bernoulli Model
**ZHAW — Probability Theory Semester Project FS26**

## Research Question
Which simplified Blackjack strategy yields the highest estimated win rate over 200 rounds?

## Method
We simulate 10'000 independent players for each strategy. Each player plays exactly 200 rounds, starting with an initial capital of CHF 100. The base stake is CHF 5 per round; for the Martingale strategy, the stake is doubled after each loss. Each round is simplified to a Bernoulli-type outcome: win (1) or non-win (0), where a win increases capital and a non-win decreases capital. A draw is counted as a non-win.

We compare the strategies using the estimated win rate, the average final capital, and the average capital path over 200 rounds.

**Note:** This is a deliberately simplified model. The full complexity of Blackjack is not modelled. Instead, each round is abstracted to a binary outcome in order to compare strategies within a clear probabilistic framework.

## Strategies
- **Basic** — Hit below 17, stand at 17 or above
- **Aggressive** — Hit below 19, stand at 19 or above
- **Random** — Random hit/stand choice (reference strategy)
- **Martingale** — Basic hit/stand decisions, but doubles the stake after every loss

## Distributions Used
- Round outcome → Bernoulli-type variable (win = 1, non-win = 0)
- Number of wins in 200 rounds → sum of Bernoulli-type trials
- Final capital → empirical distribution from simulation

## Technologies
- R
- ggplot2
- Quarto

## How to Run
1. Install R and RStudio
2. Clone this repository
3. Open `simulation.R` in RStudio
4. Run the script

## Authors
- Aisosa Omokaro
- Thiveja Thirukumar