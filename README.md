# Factor Timing in Currency Markets

This repository contains the codes for my Master thesis in Quantitative Finance at WU Vienna: "Factor Timing in Currency Markets".

Script [`main.R`](./main.R) contains the code for replicating the thesis' results, while the scripts in folder [`R`](./R) contain all the coded functions used to do so.

## Abstract

This thesis examines whether simple, real-time timing rules improve currency factor portfolios. Using bid–ask quotes for spot and forward exchange rates for 76 currencies (1990–2025), I build monthly net log excess returns for 12 price-based factors — cross-sectional and time series carry and momentum (1–12m), dollar and dollar carry — and an equal-weighted multifactor portfolio. I time each factor with (i) factor-level momentum scaled by long-horizon volatility and (ii) realized variance/volatility from daily spot moves, with capped exposures. Performance is assessed versus untimed benchmarks using mean returns, volatility, Sharpe ratios, and regression alphas. Timing generally detracts: short-horizon momentum timing raises volatility, lowering Sharpe ratios; variance/volatility scaling has muted effects; statistically significant and positive regression alphas relative to untimed strategies are absent while many timed variants produce significantly negative values. Limited, statistically weak improvements appear for dollar carry under longer-horizon momentum timing. Results are weaker in emerging than in developed markets. Overall, simple univariate, equity-style timing rules do not reliably enhance currency factors.

## Untimed Strategies
- **Dollar**: $1/N$ of all currencies
- **Dollar Carry**: long (short) all currencies when the average forward discount $s_{t} - f_{t}$ is positive (negative).
- **CS-Carry**: long (short) currencies with high (low) forward discount (quintile sorted).
- **TS-Carry**: long (short) all currencies with positive (negative) forward discount.
- **CS-Momentum**: long (short) currencies with high (low) past performance (quintile sorted).
- **TS-Momentum**: long (short) all currencies with positive (negative) past performance.

## Timing Signals
- **Momentum**: weights given by the annualized past $m$ months return, $\tilde r_{i,t}^{(m)}$, scaled by the annualized past return volatilty over $v$ months, $\tilde \sigma_{i,t}^{(v)}$, capped at $\pm 2$, where $m\in$ { $1, 3, 6, 12$ } and $v \in$ { $12, 36, 60$ }, i.e.:

$$
w_{i,t} = \frac{ \tilde r_{i,t}^{(m)} }{ \tilde \sigma_{i,t}^{(v)}}
$$

- **Realized Variance (Volatility)**: weights given by the inverse of the variance (volatility) of daily returns in month $t$, $RV_{i,t}$, scaled by the average of all monthly variances (volatility) of daily returns until $t$, $\overline{RV}\_{i,t}$, i.e.:

$$
w_{i,t} = \frac{ \overline{RV}\_{i,t} }{ RV_{i,t} }
$$

