# Factor Timing in Currency Markets (provisional)

This repository contains the codes for my Master thesis in Quantitative Finance at WU Vienna: "Factor Timing in Currency Markets" (provisional title).

## Untimed Strategies
- **Dollar**: $1/N$ of all currencies
- **Dollar Carry**: long (short) all currencies when the average forward discount $s_{i,t} - f_{i,t}$ is positive (negative).
- **CS-Carry**: long (short) currencies with high (low) forward discount (quintile sorted).
- **TS-Carry**: long (short) all currencies with positive (negative) forward discount.
- **CS-Momentum**: long (short) currencies with high (low) past performance (quintile sorted).
- **TS-Momentum**: long (short) all currencies with positive (negative) past performance.
