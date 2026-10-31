# BLP Demand Estimation: US Automobile Market (1971-1990)

Estimating consumers' willingness to pay (WTP) for car specs and price elasticities using the Berry, Levinsohn & Pakes (1995) logit-IV approach in R.

~ Millo Dacha

## Results and takeaways

![Willingness to pay for car features](<WTP by feature.png>)

| Feature | OLS | IV (BLP instruments) | 95% confidence interval (IV) |
|---|---|---|---|
| Air conditioning (yes vs no) | -$407 | $3,979 | $1,260 to $6,700 |
| Size (+1 SD) | $7,641 | $4,283 | $600 to $7,960 |
| Performance, hp/weight (+1 SD) | $709 | $1,657 | $350 to $2,970 |
| Fuel economy (+1 SD, ~5.8 mpg) | $3,433 | $1,257 | -$1,750 to $4,260 |

*WTP in 1983 $. +1 SD = one standard deviation increase, i.e. a typical step up in that spec.*

| Model | Price coefficient | Avg. own-price elasticity | Cars with inelastic demand |
|---|---|---|---|
| OLS (year FE) | -0.089 | -1.05 | 67.4% |
| IV / 2SLS (year FE) | -0.155 | -1.82 | 21.2% |

![Own-price elasticities, OLS vs IV](<price elasticities.png>)

- OLS underestimates price sensitivity since better cars (unobserved quality) are both pricier and sell more. After instrumenting price, consumers come out ~75% more price sensitive and the share of cars with inelastic demand (not possible for a profit maximising firm) drops from 67% to 21%.
- OLS even gives a negative WTP for AC (-$407), IV corrects it to ~$4,000.
- Size and AC are the most valued specs, WTP for fuel economy is not statistically significant.
- These estimates can be used for pricing a spec, deciding discounts (using elasticities) and to back out marginal costs of rival firms from their prices (assuming Bertrand-Nash pricing).
- Limitation: simple logit assumes the same taste for everyone, so elasticities rise mechanically with price and implied markups are almost flat (~$6,500 per car), giving negative marginal costs for the cheapest cars. Next step is the full BLP random coefficients model.
- **Note:** the estimates are based on 1971-1990 US car data (in 1983 $), so the actual numbers are unlikely to be relevant for today's market. The aim is to show the method, which can be applied to any recent dataset.

## Data

- Classic BLP (1995) US car dataset (`productData_cars` from the `BLPestimatoR` R package): 2217 car models, 20 yearly markets (1971-1990), 26 manufacturers, no missing values.
- Variables: `price` ($1000s, 1983 $), `hpwt` (hp per 10 lbs of weight), `air` (AC dummy), `mpg` (tens of miles per gallon), `space` (length x width), `share` (market share), `cdid` (market/year), `firmid` (manufacturer).
- Each year ~89% of households don't buy a new car, which is taken as the outside option.

| Average price by market (year) | Distribution of prices |
|---|---|
| ![Average price by market](<Avg price vs market.png>) | ![Distribution of prices](<Frequency distribution of prices.png>) |

*Average price rises from ~$9k in 1971 to $14-17k by the late 1980s, and prices are right-skewed with most cars at $5-12k and a long luxury tail.*

## Method

**1. Logit demand model** (Berry, 1994)

```
ln(s_j) - ln(s_0) = -α·p_j + β1·hpwt_j + β2·air_j + β3·mpg_j + β4·space_j + ξ_j
```

- Generated the outside good share `s_0 = 1 - sum of shares` for each year and the dependent variable `ln(s_j) - ln(s_0)`.
- Year fixed effects absorb macro shocks common to all cars in a year. Standard errors are clustered by manufacturer.

**2. Instrumental variables (2SLS)**

- Price is endogenous since ξ (unobserved quality) affects both the price and the market share.
- BLP instruments: number of other cars of the same firm and the sum of their specs in the same year. Rival firms' sums are collinear with these once year FE are included, so they were dropped.
- Diagnostics: first stage F = 69.4 (strong), Wu-Hausman p < 0.001 (price is endogenous), Sargan rejects (likely due to the simple logit).

**3. Outputs**

- WTP for a spec = β_k / α (delta-method standard errors)
- Own-price elasticity = -α·p_j·(1 - s_j)
- Markup (multi-product Bertrand) = 1 / (α·(1 - S_f)), marginal cost = price - markup

## Repository contents

| File | Description |
|---|---|
| `code base.R` | Full R script: data, EDA, OLS, IV, WTP, elasticities, markups and charts |
| `auto_data.xlsx` | Dataset exported to Excel |
| `BLP_Car_Demand_Report.pdf` | Full report |
| `US_Car_Demand_BLP_overview.pdf` | Short overview |
| `*.png` | Charts used in the reports |

## How to run

1. Install R (4.3 or newer) and RStudio.
2. Open `code base.R` and run it top to bottom. Missing packages (`BLPestimatoR`, `dplyr`, `ggplot2`, `fixest`, `modelsummary`, `writexl`) are installed automatically.

## References

- Berry, S., Levinsohn, J. and Pakes, A. (1995). Automobile Prices in Market Equilibrium. *Econometrica*, 63(4), 841-890.
- Berry, S. (1994). Estimating Discrete-Choice Models of Product Differentiation. *RAND Journal of Economics*, 25(2), 242-262.
