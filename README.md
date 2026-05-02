# Blackjack Monte Carlo Simulation
**Probability Theory Semester Project FS26**

## Research Question
Which Blackjack strategy survives the longest — and how likely is it 
to beat the casino?

## Description
We simulate 10,000 Blackjack players each starting with CHF 100. 
Every player follows one of 4 strategies and plays until they go 
broke or reach 200 rounds. The simulation uses a real 52-card deck 
and realistic game rules.

## Strategies
- **Basic** — Hit if under 17, Stand at 17 or above
- **Random** — random Hit/Stand decisions
- **Aggressive** — Hit if under 19, Stand at 19 or above
- **Martingale** — double the bet after every loss

## Distributions Used
- Game outcome → Bernoulli
- Number of rounds until ruin → Geometric
- Total profit over many rounds → Normal
- Time between wins → Exponential

## Technologies
- R
- ggplot2
- Quarto

## How to Run
1. Install R and RStudio
2. Clone this repository
3. Open `simulation.R` in RStudio
4. Press Ctrl+A → Ctrl+Enter

## Authors
- Aisosa Omokaro
- Thiveja Thirukumar 
