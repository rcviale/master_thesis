# Factor Timing in Currency Markets

This repository contains the codes for my Master thesis in Quantitative Finance at WU Vienna: "Factor Timing in Currency Markets".

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

