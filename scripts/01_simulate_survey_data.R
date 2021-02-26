#### Preamble ####
# Purpose: Simulate realistic survey responses to investigate COVID-19's impact on restaurants.
# Author: Youjing Li
# Email: youjing.li@utoronto.ca
# Date: 26 February 2021
# Prerequisites: -
# Issues: I want to simulate responses based on survey questions.
# To do: 
# - Statistical simulations using Poisson, Normal, and Uniform Distributions.
# - Realistic probabilities for each outcome.

#### Workspace set-up ####
# Libraries
library(tidyverse)
library(tibble)

#### Simulate questions ####
# Q1: Total sales decline(%)?
# Q2: Staffs laid off(%)?
# Q3: How worried restaurant owners are with respect to loss of liquidity?
# Q4: Employment size?
# Q5: Source of business debt?
# Q6: Current operation status?
# Q7: Number of customer reviews?
# Q8: Yelp star rating?
# Q9: Price points?
# Q10: Location/city?

# Do this one for treated and once for control and then bring them together
set.seed(853)
number_of_observations_treated <- 2000
simulated_dataset_treated <- 
  tibble(
    type = rep("Treated", number_of_observations_treated),
    Q1 = rpois(n = number_of_observations_treated, lambda = 48.2), 
    Q2 = rnorm(n = number_of_observations_treated, mean = 50, sd = 5) %>% round(digits = 0) %>% abs(), # Use abs() to make sure no negatives!
    Q3 = sample(x = c(
      "5-Extremely",
      "4-Very",
      "3-Moderately",
      "2-Slightly",
      "1-Not at all"),
      size = number_of_observations_treated,
      replace = TRUE, 
      prob = c(0.43, 0.26, 0.18, 0.10, 0.03)
    ),
    Q4 = sample(x = c(
      "Micro (1-4)",
      "Small (5-99)",
      "Medium (100-499)"),
      size = number_of_observations_treated,
      replace = TRUE, 
      prob = c(0.25, 0.73, 0.02)
    ),
    
    Q5 = sample(x = c(
      "Rent",
      "Paying vendors",
      "Business taxes",
      "Payroll",
      "Insurance"),
      size = number_of_observations_treated,
      replace = TRUE, 
      prob = c(0.76, 0.61, 0.58, 0.56, 0.44)
    ),
    Q6 = sample(x = c(
      "Open",
      "Closed",
      "New"),
      size = number_of_observations_treated,
      replace = TRUE, 
      prob = c(0.923, 0.036, 0.041)
    ),
    Q7 = runif(n = number_of_observations_treated, min = 0, max =500) %>% round(digits = 0), 
    Q8 = rnorm(n = number_of_observations_treated, mean = 3.5, sd = 0.5) %>% round(digits = 1) %>% abs(),
    Q9 = rnorm(n = number_of_observations_treated, mean = 2, sd = 0.5) %>% round(digits = 0) %>% abs(),
    Q10 = sample(x = c(
      "Toronto",
      "Ottawa",
      "Hamilton",
      "London",
      "Thunder Bay"
    ),
    size = number_of_observations_treated,
    replace = TRUE, 
    prob = c(0.58, 0.20, 0.11, 0.08, 0.03)
    )
  )

number_of_observations_control <- 2000
simulated_dataset_control <- 
  tibble(
    type = rep("Control", number_of_observations_control),
    Q1 = rpois(n = number_of_observations_control, lambda = 37.2), 
    Q2 = rnorm(n = number_of_observations_control, mean = 45, sd = 5) %>% round(digits = 0) %>% abs(), 
    Q3 = sample(x = c(
      "5-Extremely",
      "4-Very",
      "3-Moderately",
      "2-Slightly",
      "1-Not at all"),
      size = number_of_observations_control,
      replace = TRUE, 
      prob = c(0.37, 0.31, 0.18, 0.10, 0.03)
    ),
    Q4 = sample(x = c(
      "Micro (1-4)",
      "Small (5-99)",
      "Medium (100-499)"),
      size = number_of_observations_treated,
      replace = TRUE, 
      prob = c(0.25, 0.73, 0.02)
    ),
    Q5 = sample(x = c(
      "Rent",
      "Paying vendors",
      "Business taxes",
      "Payroll",
      "Insurance"),
      size = number_of_observations_control,
      replace = TRUE, 
      prob = c(0.76, 0.61, 0.58, 0.56, 0.44)
    ),
    Q6 = sample(x = c(
      "Open",
      "Closed",
      "New"),
      size = number_of_observations_control,
      replace = TRUE, 
      prob = c(0.03, 0.95, 0.02)
    ),
    Q7 = runif(n = number_of_observations_control, min = 0, max =500) %>% round(digits = 0), 
    Q8 = rnorm(n = number_of_observations_control, mean = 3.5, sd = 0.5) %>% round(digits = 1) %>% abs(),
    Q9 = rnorm(n = number_of_observations_control, mean = 2, sd = 0.5) %>% round(digits = 0) %>% abs(),
    Q10 = sample(x = c(
      "Toronto",
      "Ottawa",
      "Hamilton",
      "London",
      "Thunder Bay"
    ),
    size = number_of_observations_control,
    replace = TRUE, 
    prob = c(0.58, 0.20, 0.11, 0.08, 0.03)
    )
  )

simulated_dataset <- 
  rbind(simulated_dataset_control, simulated_dataset_treated)

#### Save the data ####
write_csv(simulated_dataset, "inputs/simulated_dataset.csv")