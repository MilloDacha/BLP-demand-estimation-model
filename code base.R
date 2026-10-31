# ==============================================================
#  BLP DEMAND ESTIMATION - US AUTOMOBILE MARKET (1971-1990)
#  Replicating the logit benchmarks of Berry, Levinsohn & Pakes (1995)
#
#  Goal: estimate how much consumers are willing to pay (WTP) for
#        car features, and how sensitive demand is to price
#        (price elasticities).
#
#  Run the script from top to bottom, one section at a time.
# ==============================================================


# --------------------------------------------------------------
# STEP 0. INSTALL AND LOAD PACKAGES
# --------------------------------------------------------------
# Installs a package only if it is not already on your computer
packages <- c("BLPestimatoR",  # contains the BLP car dataset (and the BLP estimator)
              "dplyr",         # data manipulation
              "ggplot2",       # charts
              "fixest",        # OLS / IV regressions with fixed effects
              "modelsummary",  # regression tables
              "writexl")       # export to Excel

for (pkg in packages) {
  if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
}

library(BLPestimatoR)
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(writexl)


# --------------------------------------------------------------
# STEP 1. LOAD THE DATA
# --------------------------------------------------------------
data("productData_cars")
d <- productData_cars
rm(productData_cars)

# Save a copy as Excel (optional, only needed once)
write_xlsx(d, "auto_data.xlsx")

# What each variable means:
#   cdid   : market = one year of the US new-car market (1 = 1971 ... 20 = 1990)
#   firmid : manufacturer (26 firms)
#   id     : product = one car model in one year (every row is unique)
#   const  : constant, always 1
#   price  : price in $1,000s (1983 dollars)
#   hpwt   : horsepower per 10 lbs of weight (performance)
#   air    : 1 if the car has air conditioning, 0 otherwise
#   mpg    : fuel economy in tens of miles per gallon (2.1 = 21 mpg)
#   space  : length x width / 1,000 sq. inches (size)
#   share  : market share = cars sold / all US households
#
# Data structure: 20 yearly cross-sections stacked together
# All variables are kept NUMERIC because the regressions need numbers.


# --------------------------------------------------------------
# STEP 2. INSPECT THE DATA
# --------------------------------------------------------------
dim(d)                 # 2,217 rows, 10 columns
str(d)                 # variable types
summary(d)             # min / mean / max of every variable
colSums(is.na(d))      # missing values (none)

cat("Markets (years):", n_distinct(d$cdid),   "\n")
cat("Firms:",           n_distinct(d$firmid), "\n")
cat("Products:",        n_distinct(d$id),     "\n")

# Summary statistics of the main variables
d %>%
  summarise(across(c(price, hpwt, air, mpg, space, share),
                   list(mean = mean, sd = sd, min = min, max = max)))

# Number of cars with / without air conditioning
table(d$air)


# --------------------------------------------------------------
# STEP 3. EXPLORATORY CHARTS
# --------------------------------------------------------------
# 3a. Average price by market (year)
d %>%
  group_by(cdid) %>%
  summarise(mean_price = mean(price)) %>%
  ggplot(aes(x = factor(cdid), y = mean_price)) +
  geom_col() +
  labs(title = "Average Automobile Price by Market",
       x = "Market (1 = 1971, 20 = 1990)", y = "Average price ($1,000s)") +
  theme_minimal()

# 3b. Distribution of prices
ggplot(d, aes(x = price)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Automobile Prices",
       x = "Price ($1,000s)", y = "Number of cars") +
  theme_minimal()

# 3c. Price vs market share (correlation only - NOT causal)
ggplot(d, aes(x = price, y = share)) +
  geom_point(alpha = 0.6) +
  labs(title = "Automobile Price versus Market Share",
       x = "Price ($1,000s)", y = "Market share") +
  theme_minimal()

# 3d. Price by air conditioning (factor() only for the chart labels)
ggplot(d, aes(x = factor(air, labels = c("No AC", "AC")), y = price)) +
  geom_boxplot() +
  labs(title = "Prices by Air-Conditioning Availability",
       x = "Air conditioning", y = "Price ($1,000s)") +
  theme_minimal()


# --------------------------------------------------------------
# STEP 4. BUILD THE DEPENDENT VARIABLE (LOGIT MODEL)
# --------------------------------------------------------------
# Each year, households either buy one of the cars or buy nothing
# (the "outside good"). Its share s0 = 1 - sum of all car shares.
# The logit model (Berry 1994) can then be written as a regression:
#
#   ln(s_j) - ln(s_0) = -alpha*price + beta*features + xi
#
# xi = car quality we cannot observe (e.g. reliability, styling).

d <- d %>%
  group_by(cdid) %>%
  mutate(s0 = 1 - sum(share)) %>%        # outside-good share, per year
  ungroup() %>%
  mutate(y = log(share) - log(s0))       # dependent variable

summary(d$s0)    # about 89% of households buy no new car each year


# --------------------------------------------------------------
# STEP 5. BUILD THE INSTRUMENTS (BLP INSTRUMENTS)
# --------------------------------------------------------------
# Problem: price is "endogenous" - better cars (high xi) are both
# priced higher AND sell more, so plain OLS underestimates how much
# consumers dislike high prices.
#
# Solution (BLP 1995): use the features of OTHER cars as instruments.
#  - own_ : sum of features of the same firm's other cars that year
#           (more own substitutes -> firm can charge a higher markup)
#  - riv_ : sum of features of rival firms' cars that year
#           (more rival competition -> lower markup)
# These move a car's price but do not directly change how much a
# buyer likes that car.

d <- d %>%
  # totals for the whole market (year)
  group_by(cdid) %>%
  mutate(n_mkt     = n(),
         mkt_hpwt  = sum(hpwt),
         mkt_air   = sum(air),
         mkt_mpg   = sum(mpg),
         mkt_space = sum(space)) %>%
  # totals for each firm within the year
  group_by(cdid, firmid) %>%
  mutate(n_firm     = n(),
         firm_hpwt  = sum(hpwt),
         firm_air   = sum(air),
         firm_mpg   = sum(mpg),
         firm_space = sum(space)) %>%
  ungroup() %>%
  # instruments
  mutate(
    own_n     = n_firm - 1,                # number of the firm's other cars
    own_hpwt  = firm_hpwt  - hpwt,         # same firm, excluding the car itself
    own_air   = firm_air   - air,
    own_mpg   = firm_mpg   - mpg,
    own_space = firm_space - space,
    riv_hpwt  = mkt_hpwt  - firm_hpwt,     # all rival firms' cars
    riv_air   = mkt_air   - firm_air,
    riv_mpg   = mkt_mpg   - firm_mpg,
    riv_space = mkt_space - firm_space
  )
# Note: with year fixed effects the riv_ variables add no extra information
# (riv_x = market total - own_x - x, and the market total is absorbed by
# the year FE), so the main IV model uses only the own_ instruments.


# --------------------------------------------------------------
# STEP 6. ESTIMATE THE DEMAND MODELS
# --------------------------------------------------------------
# All models: standard errors clustered by firm (cars of the same
# manufacturer share unobserved shocks).
# "| cdid" adds year fixed effects (removes shocks common to all cars
# in a year, e.g. recessions, interest rates).

# Model 1: OLS logit, no fixed effects
ols1 <- feols(y ~ price + hpwt + air + mpg + space,
              data = d, cluster = ~firmid)

# Model 2: OLS logit + year fixed effects
ols2 <- feols(y ~ price + hpwt + air + mpg + space | cdid,
              data = d, cluster = ~firmid)

# Model 3: IV logit + year fixed effects  (MAIN MODEL)
# Syntax: y ~ exogenous variables | fixed effects | endogenous ~ instruments
iv1 <- feols(y ~ hpwt + air + mpg + space | cdid |
               price ~ own_n + own_hpwt + own_air + own_mpg + own_space,
             data = d, cluster = ~firmid)

# Results table (fit_price = instrumented price)
modelsummary(list("OLS" = ols1, "OLS + year FE" = ols2, "IV + year FE" = iv1),
             coef_rename = c("fit_price" = "price"),
             stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")
# Expected: the price coefficient becomes more negative with IV
# (about -0.09 OLS -> -0.155 IV), i.e. OLS understated price sensitivity.


# --------------------------------------------------------------
# STEP 7. CHECK THE INSTRUMENTS
# --------------------------------------------------------------
#  F-test (1st stage) : are instruments strong?          want F > 10
#  Wu-Hausman         : is price really endogenous?      p < 0.05 -> yes, IV needed
#  Sargan             : do instruments agree?            p < 0.05 -> some disagreement
fitstat(iv1, ~ ivf + wh + sargan)



# --------------------------------------------------------------
# STEP 8. WILLINGNESS TO PAY (WTP)
# --------------------------------------------------------------
# WTP for a feature = its coefficient / price coefficient (sign flipped)
#   WTP_k = beta_k / (-alpha)        -> measured in $1,000s (1983 $)
# Standard errors use the delta method (approximate SE of a ratio).

wtp_table <- function(model, price_name) {
  b <- coef(model)          # estimated coefficients
  V <- vcov(model)          # their variance-covariance matrix
  a <- b[price_name]        # price coefficient (alpha)
  results <- data.frame()

  for (k in c("hpwt", "air", "mpg", "space")) {
    wtp <- b[k] / -a

    # delta method: gradient of WTP with respect to (beta_k, alpha)
    grad <- setNames(rep(0, length(b)), names(b))
    grad[k]          <- -1 / a
    grad[price_name] <- b[k] / a^2
    se <- sqrt(as.numeric(t(grad) %*% V %*% grad))

    results <- rbind(results, data.frame(
      feature          = k,
      wtp_per_unit_usd = round(wtp * 1000),               # per 1 unit of the feature
      se_usd           = round(se * 1000),
      wtp_per_1sd_usd  = round(wtp * sd(d[[k]]) * 1000)   # per 1 std. dev. (easier to compare)
    ))
  }
  rownames(results) <- NULL
  results
}

wtp_table(ols2, "price")       # biased (OLS)
wtp_table(iv1,  "fit_price")   # main result (IV)
# Note: for air (0/1) the "per unit" WTP is the value of having AC.


# --------------------------------------------------------------
# STEP 9. PRICE ELASTICITIES
# --------------------------------------------------------------
# Logit formulas (alpha = price coefficient, negative):
#   own-price elasticity   e_jj = alpha * p_j * (1 - s_j)
#   cross-price elasticity e_jk = -alpha * p_k * s_k
# e_jj = -2 means a 1% price rise cuts that car's sales by about 2%.

alpha_ols <- coef(ols2)["price"]
alpha_iv  <- coef(iv1)["fit_price"]

d <- d %>%
  mutate(own_el_ols  =  alpha_ols * price * (1 - share),
         own_el_iv   =  alpha_iv  * price * (1 - share),
         cross_el_iv = -alpha_iv  * price * share)

# Sanity check: firms should never price where |elasticity| < 1
# (raising price would then increase profit). Fewer inelastic cars = better.
d %>%
  summarise(mean_own_el_ols   = mean(own_el_ols),
            pct_inelastic_ols = mean(abs(own_el_ols) < 1),
            mean_own_el_iv    = mean(own_el_iv),
            pct_inelastic_iv  = mean(abs(own_el_iv) < 1),
            median_cross_el   = median(cross_el_iv))

# Average own-price elasticity by price band (IV)
d %>%
  mutate(price_band = cut(price, breaks = c(0, 8, 12, 20, 70),
                          labels = c("<8k", "8-12k", "12-20k", ">20k"))) %>%
  group_by(price_band) %>%
  summarise(cars = n(), mean_own_el_iv = mean(own_el_iv))

# Limitation of the logit model:
#  - own elasticity rises mechanically with price (expensive cars look
#    MORE price sensitive, although their buyers are richer)
#  - cross elasticities ignore similarity: a Mercedes price rise sends
#    buyers to a cheap Chevette as easily as to a BMW
# The BLP random-coefficients model (next step) fixes both.

# --------------------------------------------------------------
# STEP 10. GRAPHS: WTP, ELASTICITIES, MARKUPS & MARGINAL COSTS
# --------------------------------------------------------------

# ---- 10a. Willingness to pay: OLS (biased) vs IV ----
# AC is shown for "having AC"; other features for +1 standard deviation
labels <- c(hpwt  = "Performance (+1 SD hp/weight)",
            air   = "Air conditioning (yes vs no)",
            mpg   = "Fuel economy (+1 SD, ~5.8 mpg)",
            space = "Size (+1 SD length x width)")

wtp_plot_data <- bind_rows(
  wtp_table(ols2, "price")     %>% mutate(model = "OLS (ignores endogeneity)"),
  wtp_table(iv1,  "fit_price") %>% mutate(model = "IV (BLP instruments)")
) %>%
  mutate(scale = ifelse(feature == "air", 1, sapply(feature, function(k) sd(d[[k]]))),
         wtp   = wtp_per_unit_usd * scale,          # WTP in $
         low   = (wtp_per_unit_usd - 1.96 * se_usd) * scale,   # 95% CI
         high  = (wtp_per_unit_usd + 1.96 * se_usd) * scale,
         feature = labels[feature])

p1 <- ggplot(wtp_plot_data, aes(x = feature, y = wtp, fill = model)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(0.8), width = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Consumers' Willingness to Pay for Car Features",
       subtitle = "US car market 1971-1990, 1983 dollars, 95% confidence intervals",
       x = NULL, y = "Willingness to pay", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
p1
ggsave("WTP by feature.png", p1, width = 8, height = 5)


# ---- 10b. Own-price elasticities: OLS vs IV ----
el_plot_data <- bind_rows(
  data.frame(elasticity = d$own_el_ols, model = "OLS (ignores endogeneity)"),
  data.frame(elasticity = d$own_el_iv,  model = "IV (BLP instruments)")
)

p2 <- ggplot(el_plot_data, aes(x = elasticity, fill = model)) +
  geom_histogram(bins = 60, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  annotate("text", x = -1, y = Inf, vjust = 1.5, hjust = -0.05,
           label = "Right of line: inelastic") +
  coord_cartesian(xlim = c(-8, 0)) +
  labs(title = "Own-Price Elasticities of Demand",
       subtitle = paste0("Inelastic cars: OLS ", round(100 * mean(abs(d$own_el_ols) < 1)),
                         "% vs IV ", round(100 * mean(abs(d$own_el_iv) < 1)), "%"),
       x = "Own-price elasticity (% change in sales for a 1% price rise)",
       y = "Number of cars", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
p2
ggsave("Elasticities OLS vs IV.png", p2, width = 8, height = 5)


# ---- 10c. Reverse-engineering markups and marginal costs ----
# If firms set prices to maximise profit (Bertrand-Nash), the logit model
# implies: markup = price - marginal cost = 1 / (-alpha * (1 - firm's total share))
# So: marginal cost = price - markup   (no cost data needed!)
d <- d %>%
  group_by(cdid, firmid) %>%
  mutate(firm_share = sum(share)) %>%
  ungroup() %>%
  mutate(markup = 1 / (-alpha_iv * (1 - firm_share)),   # in $1,000s
         mc     = price - markup,                       # implied marginal cost
         lerner = markup / price)                       # markup as % of price

summary(d$markup)      # markup in $1,000s
summary(d$lerner)      # markup as share of price
mean(d$mc < 0)         # share of cars with an implausible negative cost

p3 <- ggplot(d, aes(x = price, y = mc, colour = mc < 0)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values = c("grey40", "red"),
                      labels = c("Positive cost", "Negative cost (implausible)")) +
  labs(title = "Implied Marginal Cost vs Price (IV logit)",
       subtitle = "Dashed line = price; vertical gap to the line = markup",
       x = "Price ($1,000s)", y = "Implied marginal cost ($1,000s)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
p3
ggsave("Implied marginal costs.png", p3, width = 8, height = 5)

