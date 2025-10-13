## Made on R version 4.5.1, but should run on any recentish version 

## Might be necessary for the program to run on your device
install.packages("tidyverse")
install.packages("lifecontingencies")
install.packages("ggplot2")
install.packages("readxl")
install.packages("here")
install.packages("xts")
install.packages("lubridate")
install.packages("forecast")


## library calls for session
suppressPackageStartupMessages
library(tidyverse)            # eases suffering
library(lifecontingencies)    # will be used for actual reserves modelling
library(ggplot2)              # dataviz
library(readxl)               # parses .xlsx files
library(here)                 # provides robust file pathing for project based programs
library(xts)                  # time series utility R
library(lubridate)            # handles dates in R
library(forecast)             # will be used for ARIMA

## if this fails, kill whatever R process was running previously and rerun Main.R, here will figure it out
## mortality rates dataframe, data was taken off the ONS, and put into an R dataframe ===================
mortalityDF <- read_excel(here("data", "nltuk198020213.xlsx"), sheet = 5, skip = 5)
mortalityDF <- mortalityDF[,-7]
mortalityDF <- mortalityDF[,-7]
colnames(mortalityDF) <- c("age", "mx_male", "qx_male", "lx_male", "dx_male", "ex_male", "mx_female", "qx_female", "lx_female", "dx_female", "ex_female") # lets dplyr merge them later

mortalityDF <- mortalityDF %>%
  pivot_longer( # pivot longer will transform the raw frame from two tables to a unified one
    cols = -age, # age is obviously the only exception (it's ungendered)
    names_to = c("rate_type", "gender"),
    names_sep = "_",
    values_to = "value"
  ) %>%  # pipe operator allows operations to be encoded for one row but applied to all
  pivot_wider(
    names_from = rate_type,
    values_from = value
  )

mortalityDF

## BoE UK implied real spot curve, short end, taken into a dataframe for projections
yieldDF <- read_excel(here("data", "GLC Real daily data current month.xlsx"), sheet = 3, skip = 3)
yieldDF <- yieldDF[-1,]
yieldDF$"years:" <- ymd(yieldDF$"years:")
xtsYield <- xts(yieldDF[,-1], order.by = yieldDF$"years:")

yieldDF


## xts, support for Vasicek Model directly through quantmod, only used for ARIMA here
## xtsYield <- xts(yieldDF[,-1], order.by = yieldDF$"years:")
## ARfitYield <- Arima(xtsYield, order = c(1, 0, 0))



## Vasicek Model ==========================================================================
## model functions as a basic differential equation relating past to present interest rates
## otherwise model parameters can be estimated using standard OLS
## dr_t = a(b - r_t)dt + sdW_t
## where r_t is the interest rate at time t, a is a mean reversion speed measure
## b is a stationary mean, s is the volatility, and dWt is standard Brownian motion

rat <- xtsYield[,"2.5"]
ratData <- tibble( # makes tables print more nicely for readers
  rat_t = as.numeric(rat[-nrow(rat)]),
  rat_tp1 = as.numeric(rat[-1])
) 


ratData <- ratData %>%
    mutate(delta_rat = rat_tp1 - rat_t) # ratData now holds the original 2.5 year rate,
## the lag, and a differential rate

## we now have a df entry with lagged values, change in rates, and the original rate
## we can now estimate our Vasicek model

fit <- lm(delta_rat ~ rat_t, data = ratData)
summary(fit)

## as a standard OLS, we've regressed
## delta_rate = constant + coef1 * rate_t + X
## where X ~ Norm(mu, sigma)

## we can effectively rearrange this, since the Vasicek model is equivalent to
## dr_t = ab - (ar)dt + sdW_t
## therefore
## a = -slope of regression
## b = intercept/a
## s = sigma of X

a_est <- coef(fit)[2] * -1
b_est <- coef(fit)[1] / a_est
s_est <- sd(resid(fit))

print(a_est)
print(b_est)
print(s_est)


## our model of interest rates then becomes
## r_t = 0.454 * (1.04 - r_t)dt + 0.0144 * dW_t
## as per the rstudio docs on the matter https://rstudio-pubs-static.s3.amazonaws.com/1052820_b82959a712604baeb614d0ff52134a32.html, we can ask R to run Monte Carlo simulations on a user defined Vasicek model

## the model takes in parameters as specified in the Vasicek differential equation, and
## adds the equation to the previous value to generate an experimental time series
## for convenience, the first entry is the long term mean
vasicek_sim <- function(a, b, s, size) {
    
    dt <- 1 # differential term

    r <- numeric(size) # array that will be filled with the time series
    r[1] <- b # mean makes good starter rate

    for (i in 2:size) { # runs function for rest of requested steps
        dW_t <- rnorm(1, mean = 0, sd = sqrt(dt)) # makes random error term satisfying model assumptions

        r[i] <- r[i-1] + a * (b - r[i-1]) * dt + s * dW_t  # generates next period from previous
    }

    return(r)
}


set.seed(1234)

size <- 7

simulation <- vasicek_sim(a = a_est, b = b_est, s = s_est, size = size)
time <- 1:size

# Plotting the experimental yield curve and the observed yield curve next to each other
ts_actual <- ts(rat, start = 1, frequency = 1)
ts_pred   <- ts(simulation, start = 1, frequency = 1)

plot(ts_actual, col = "black", lwd = 2, ylab = "Rate", main = "Actual vs Vasicek-Predicted Rates")
lines(ts_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted (Vasicek)"), col = c("black", "blue"), lty = 1, lwd = 2)



## our datasets are now mortalityDF, which holds the mortality table data from the ONS,
## and simulation, which has 1000 inflation projections based on 2.5 year spot rates

## next step is making policyholders to run the model on ======================

population <- 1000
portfolio <- tibble( # this portfolio will hold our artificial policyholders 
  id = 1:population,
  age = sample(18:80, population, replace = TRUE), # we can also use monte carlo to pick uniformly
  gender = sample(c("male", "female"), population, replace = TRUE, prob = c(0.5, 0.5)),
  smoker = sample(c("yes", "no"), population, replace = TRUE, prob = c(0.119, 0.881)), # bad habit
  term = sample(5:30, population, replace = TRUE), # a 90 year old could end up with a 30 year term, fix?
  driver = sample(c("driver", "can't drive"), population, replace = TRUE, prob = c(0.9, 0.1)),
  sum_assured = round(rlnorm(population, meanlog = log(100000), sdlog = 0.3), -3),  # basic lnorm wealth fill
  start_year = sample(2014:2024, population, replace = TRUE)
)
portfolio

## premium calculation =====================================================================

## to calculate the value of the net premium, we can employ the actuarial equivalence principle
## P = S * A_x / a_[x]:n
## where P is the premium, S is a benefit paid out at death,
## A_x has x as the age of the policyholder, and a_x[n] references the discounted value of
## the policyholder's n payments till death

maleMort <- mortalityDF[mortalityDF$gender == 'male', ] # because we have both genders in one 
femaleMort <- mortalityDF[mortalityDF$gender == 'female', ] # the package gets confused

maleMortRat <- new("actuarialtable",
               x = maleMort$age,
               lx = femaleMort$lx,
               interest = simulation[1])

femaleMortRat <- new("actuarialtable",
                     x = femaleMort$age,
                     lx = femaleMort$lx,
                     interest = simulation[2])

premiumCalculator <- function(benefit, age, term, table) {

    ## lifecontingencies is happy to calculate A and a for us
    A = Axn(actuarialtable = table, x = age, n = term)

    a = axn(actuarialtable = table, x = age, n = term)
    
    Premium = benefit * A / a
    return(Premium)
    
}

whichTable <- function(gender) {
  if (gender == "male") return(maleMortRat)
  if (gender == "female") return(femaleMortRat)
  stop("Unknown gender")
}


print(portfolio)

portfolio <- portfolio %>%
  rowwise() %>%
    mutate(
        useTable = list(whichTable(gender)), # utility function makes sure we use right table
        premium = premiumCalculator(
            benefit = sum_assured,
            age = age,
            term = term,
            table = useTable)
    ) %>%
    ungroup() #splits premia or else tibble has to try loading a datatable with a grouped value

print(portfolio) # now has premium values calculated for every policyholder :)



## Calculating general reserve requirements ===================================================

## the basic principle of actuarial reserves is that the value of the future insurance
## benefits - the discounted value of insurance premiums:
## V_x = S * A_(x+t) - P_x * a_(x+t)
## V_x is the value of the reserve, and the only new variable in this equation

reserveCalculator <- function(benefit, age, term, t, premium, table) {
    
  remaining_term <- term - t # figure out how much time is left on the policy
  new_age <- age + t

  if (remaining_term <= 0) {
    return(0)
  }

  A_remaining <- Axn(table, x = new_age, n = remaining_term)  # lifecontingency calculates A and a
  a_remaining <- axn(table, x = new_age, n = remaining_term)

  reserve <- benefit * A_remaining - premium * a_remaining # return value of reserve
  return(reserve)
}

policyTerm <- 10
portfolio <- portfolio %>%
  rowwise() %>%
    mutate(
        useTable = list(whichTable(gender)), # an old friend
        reqReserves = reserveCalculator(
            benefit = sum_assured,
            age = age,
            term = term,
            t = policyTerm,
            premium = premium,
            table = useTable)
        ) %>%
    ungroup()

## finally, we can see what the required reserves and premium are for our entire portfolio of people
print(portfolio)
sum(portfolio$reqReserves)
sum(portfolio$premium)

## ending out with £994,549 in general reserve requirements and £348,038.60 in premiums overall.
