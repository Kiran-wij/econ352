---
  title: "HW4 Markdown"
output: beamer_presentation
date: "2022-12-05"
---
  

library(tidyverse)
library(bunching)




l_2000 <- read_csv("~/Econ 352/HW2/HW4/SFLP_performance_2000.csv")
l_2007 <- read_csv("~/Econ 352/HW2/HW4/SFLP_performance_2007.csv")




z <- nrow(l_2000)

l_2000g <- l_2000 %>%
  mutate(rate_gap = round((current_interest_rate - mortgagerate_30yr), 1)) %>%
  mutate(pre_pay = case_when(
    !is.na(zero_balance_date) ~ 1,
    is.na(zero_balance_date) ~ 0)) %>%
  group_by(rate_gap) %>%
  summarize(pct = n()/z, prob = sum(pre_pay)/n())



ggplot(data = l_2000g, aes(x = rate_gap, y = pct)) +
  geom_smooth() + 
  geom_smooth(aes(y = prob), color = "red") +
  scale_y_continuous(sec.axis = dup_axis()) +
  labs(title = "Distribution (Blue) vs Hazard Rates (Red) by Rate Gap", x = "Rate Gap", y = "Percentage of Loans")



# Here we filter out gaps > 2.0 because when rates fall these gaps go to > 3.0
# and we don't have probabilities for when gap > 3.0 so for a better comparison
# we only look at mortgages for which we know the before/ after probabilities

#Rates fall baseline
l_2000g %>%
  filter(rate_gap <= 2.0) %>% 
  mutate(product = pct*prob) %>%
  summarize(total_prob = sum(product))

# Here we do the same but filter out gaps < -0.5 because with the rate rise the
# gaps would rise to >-1.5% which we simmilarly dont have probabilities for

#Rates rise baseline
l_2000g %>%
  filter(rate_gap >= -0.5) %>% 
  mutate(product = pct*prob) %>%
  summarize(total_prob = sum(product))


# Rates fall
probs <- l_2000g %>%
  select(rate_gap, prob)

new_pcts <- l_2000g %>%
  mutate(rate_gap = rate_gap + 1) %>%
  select(rate_gap, pct)


dfm <- merge(probs, new_pcts, by="rate_gap")

dfm %>%
  mutate(product = pct*prob) %>%
  summarize(total_prob = sum(product))


# Rates rise
probs2 <- l_2000g %>%
  select(rate_gap, prob)

new_pcts2 <- l_2000g %>%
  mutate(rate_gap = rate_gap - 1) %>%
  select(rate_gap, pct)


dfm2 <- merge(probs2, new_pcts2, by="rate_gap")

dfm2 %>%
  mutate(product = pct*prob) %>%
  summarize(total_prob = sum(product))




z2 <- nrow(l_2007)

l_2007g <- l_2007 %>%
  mutate(rate_gap = round((current_interest_rate - mortgagerate_30yr), 1)) %>%
  mutate(pre_pay = case_when(
    !is.na(zero_balance_date) ~ 1,
    is.na(zero_balance_date) ~ 0)) %>%
  group_by(rate_gap) %>%
  summarize(pct = n()/z2, prob = sum(pre_pay)/n())



ggplot(data = l_2007g, aes(x = rate_gap, y = pct)) +
  geom_smooth() + 
  geom_smooth(aes(y = prob), color = "red") +
  scale_y_continuous(sec.axis = dup_axis()) +
  labs(title = "Distribution (Blue) vs Hazard Rates (Red) by Rate Gap", 
       x = "Rate Gap", y = "Percentage of Loans")




