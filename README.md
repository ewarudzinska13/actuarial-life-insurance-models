# actuarial-life-insurance-models
Actuarial mathematics analysis of life insurance models (exponential and de Moivre) with premium and annuity calculations in R

# Actuarial Models for Life Insurance

Analysis of theoretical actuarial models for life insurance calculations, including survival functions, net single premiums (NSP), and annuity values. Project for Actuarial Methods in Life Insurance I course at University of Warsaw.

## Overview

This project implements and visualizes two classical actuarial models:
1. **Exponential distribution model** (constant force of mortality)
2. **de Moivre's model** (linear survival function)

## Models Analyzed

### 1. Exponential Distribution Model

**Survival function**: S(x) = e^(-μx)

Calculations performed:
- Survival probability for age x
- 10-year survival probability from age x
- Net single premium (JSN) - whole life insurance
- Net single premium - term life insurance  
- Annuity values (immediate and deferred)

**Parameters tested**: μ ∈ {0.03, 0.05, 0.08}, i ∈ {0.03, 0.05, 0.08}

### 2. de Moivre's Model

**Survival function**: S(x) = (ω - x) / ω, for x < ω

Where ω is the maximum age (typically 100 or 110 years)

Calculations performed:
- Survival probability for age x
- Net single premium (JSN) for various ages and interest rates
- Annuity values under different mortality assumptions
- Comparison with exponential model

## Key Calculations

### Net Single Premium (JSN)

**Whole life insurance**:
```
JSN_x = μ / (δ + μ)
```
where δ = log(1 + i)

**Term life insurance (n years)**:
```
JSN_x:n = μ · (1 - e^(-n(δ+μ))) / (δ + μ)
```

### Annuity Values

**Immediate life annuity**:
```
ä_x = 1 / (δ + μ)
```

**Deferred annuity** (n years):
```
n|ä_x = e^(-n(δ+μ)) / (δ + μ)
```

## Visualizations

All calculations illustrated with plots showing:
- Survival functions vs. age
- JSN values by age for different parameters (μ, i)
- JSN values by interest rate for fixed age
- JSN values by insurance period (term insurance)
- Annuity values under various scenarios
- Comparison between exponential and de Moivre models

## Parameters

- **Force of mortality (μ)**: 0.03, 0.05, 0.08
- **Interest rate (i)**: 0.03, 0.05, 0.08
- **Age range**: 0-100 years
- **Insurance periods**: 1-30 years
- **Maximum age (ω)**: 100 years (de Moivre)

## Tools

- **R** with `ggplot2` for visualization

