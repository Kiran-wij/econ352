library(tidyverse)
library(bunching)

# Read data
loans <- read_csv("~/Econ 352/HW2/HW3/all_llma_nc.csv")
limits <- read_csv("~/Econ 352/HW2/HW3/conformingloanlimits.csv")

# Question 1.1
sapply(loans, class)

# Question 1.2
loans <- loans %>% 
  filter(year >= 1992 & year <= 2007,
         product_type == 10,
         original_term == 360 | original_term == 180,
         loan_type == 1,
         loan_purpose %in% c(1, 2, 3, 5))

# Question 1.3
loans <- merge(loans, limits,by="year")

# Question 1.4
loans <- loans %>%
  mutate(deviation = log(original_balance) - log(conformingloanlimit)) %>% 
  filter(!is.na(deviation)) %>% 
  mutate(bin = floor(deviation * 100)) %>%
  filter(abs(bin) <= 100) %>%
  filter(!is.na(initial_interest_rate)) %>%
  na.omit()

# Question 1.5
bins <- loans %>%
  mutate(lower_bound = (deviation*200000) %/% 1000) %>%
  filter(abs(lower_bound) < 100)

# Question 1.6
total <- as.integer(count(loans))

bin_share <- bins %>%
  group_by(lower_bound) %>%
  summarize(share = n()/total, mean = mean(initial_interest_rate, na.rm = TRUE))

bin_share

# Question 2.1
ggplot(data = bin_share, aes(x=lower_bound, y = share)) +
  geom_bar(stat="identity") +
  labs(title = "Loan Size Density", y = "Fraction of Loans", 
       x = "Deviation from CLL")

# Question 2.2
graph1a <- bin_share %>% 
  mutate(overlimit = case_when(lower_bound <= 0 ~ FALSE,
                               TRUE ~ TRUE))

head(graph1a)

ggplot(data = graph1a, aes(x=lower_bound, y = mean, color = overlimit)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, linetype = 2) +
  geom_vline(linetype = 2, xintercept = 0) +
  labs(title = "Mean interest rate by loan size", y = "Interest rate", x = "Loan amount - conforming limit") +
  theme(legend.position = "none")

# Question 2.3
preBin = bin_share %>% 
  filter(lower_bound < 0)
preBinReg = lm(mean ~ lower_bound, preBin)

postBin = bin_share %>%
  filter(lower_bound > 0)
postBinReg = lm(mean ~ lower_bound, postBin) 

preRates = predict(preBinReg, data.frame(lower_bound=c(0)))/100
postRates = predict(postBinReg, data.frame(lower_bound=c(0)))/100

question3 <- loans%>% 
  mutate(shifted_dev = deviation + 1)

options(warn=-1)
bunchit(question3$shifted_dev, zstar=1, t0=preRates, t1=postRates, binwidth = 0.01, bins_l=200, bins_r=200, poly=13)

#Error message when notch=T included


