library(tidyverse)

top_rates <- read_csv("~/HW2/toprates_77_18.csv")
tax_rate_merged <- read_csv("~/HW2/tax_rate_merged.csv")
forbes400 <- read_csv("~/HW2/forbes400.csv")

#Question 1.1

means_and_median_federal <- top_rates %>% 
  filter(state_full == "FEDERAL") %>% 
  group_by(year) %>% 
  summarize(mean_federal=mean(topfederalrate))

means_and_median_state <- top_rates %>% 
  filter(state_full != "FEDERAL") %>% 
  group_by(year) %>% 
  summarize(mean_state=mean(topstaterate),
            median_state=median(topstaterate))

ggplot(data = means_and_median_federal) + geom_point(aes(x = year, y = mean_federal)) +
  geom_smooth(aes(x = year, y = mean_federal), method = "lm", se = F) +
  labs(title = "Top Federal Tax Rate (1977-2018)",
       x = "Year",
       y = "Top Federal Tax Rate")

ggplot(data = means_and_median_state) + 
  geom_point(aes(x = year, y = mean_state)) +
  geom_smooth(aes(x = year, y = mean_state), method = "lm", se = F) +
  labs(title = "Mean Top State Tax Rate (1977-2018)",
       x = "Year",
       y = "Mean Top State Tax Rate")

ggplot(data = means_and_median_state) + 
  geom_point(aes(x = year, y = median_state)) +
  geom_smooth(aes(x = year, y = median_state), method = "lm", se = F) +
  labs(title = "Median Top State Tax Rate (1977-2018)",
       x = "Year",
       y = "Median Top State Tax Rate")

#Question 1.2

group_t <- top_rates %>%
  filter(year == 2016) %>%
  filter(state_full != "FEDERAL") %>%
  arrange(topstaterate) %>%
  slice(unique(c(n() - 0:9)) ) %>%
  select(state_full, topstaterate)

group_c <- top_rates %>%
  filter(year == 2016) %>%
  filter(state_full != "FEDERAL") %>%
  arrange(topstaterate) %>%
  slice(unique(c(1:10)) ) %>%
  select(state_full, topstaterate)

group_t
group_c

#Question 1.3

tax_rate_merged %>%
  filter(state_full %in% group_t$state_full & year == 2016) %>%
  summarize(mean(millionaire_share))

tax_rate_merged %>%
  filter(state_full %in% group_c$state_full & year == 2016) %>%
  summarize(mean(millionaire_share))

#Question 1.4

changes_temp <- tax_rate_merged %>%
  filter(year == 2001 | year == 2016) %>%
  select(state_full, topstaterate, year) %>%
  group_by(state_full) %>%
  pivot_wider(id_cols = state_full, names_from = year, 
              values_from = c(topstaterate)) %>%
  rename("rate_2001" = 2) %>%
  rename("rate_2016" = 3) %>%
  mutate(diff = rate_2016 - rate_2001) %>%
  mutate(diff_pct = (rate_2016 - rate_2001)/rate_2001 * 100) %>%
  replace(is.na(.), 0)

group_tt <- changes_temp %>%
  arrange(desc(diff))  %>%
  head(10)

group_cc <- changes_temp %>%
  arrange(diff)  %>%
  head(10)

group_tt 
group_cc

mean(group_tt$diff)
mean(group_tt$diff_pct)

mean(group_cc$diff)
mean(group_cc$diff_pct)

#Question 1.5

tax_rate_merged %>%
  filter(state_full %in% group_tt$state_full) %>%
  filter(year == 2001 | year == 2016) %>%
  group_by(year) %>%
  select(state_full, year, millionaire_share) %>%
  pivot_wider(id_cols = state_full, names_from = year, 
              values_from = c(millionaire_share)) %>%
  rename("mshare_2001" = 2) %>%
  rename("mshare_2016" = 3) %>%
  summarize(mshare_01 = sum(mshare_2001), mshare_16 = sum(mshare_2016)) %>%
  mutate(diff = mshare_16 - mshare_01, diff_pct = (mshare_16 - mshare_01) / mshare_01)

tax_rate_merged %>%
  filter(state_full %in% group_cc$state_full) %>%
  filter(year == 2001 | year == 2016) %>%
  group_by(year) %>%
  select(state_full, year, millionaire_share) %>%
  pivot_wider(id_cols = state_full, names_from = year, 
              values_from = c(millionaire_share)) %>%
  rename("mshare_2001" = 2) %>%
  rename("mshare_2016" = 3) %>%
  summarize(mshare_01 = sum(mshare_2001), mshare_16 = sum(mshare_2016)) %>%
  mutate(diff = mshare_16 - mshare_01, diff_pct = (mshare_16 - mshare_01) / mshare_01)

#Question 1.6

nc_wi <- tax_rate_merged %>%
  filter(state == "NC" | state == "WI") %>%
  filter(year <= 2016 & year >= 2010)

ggplot(data = nc_wi) + geom_line(aes(x = year, y = topstaterate, group = state_full,
                                     color = state_full)) +
  labs(title = "Top State Tax Rate (2010-2016)",
       x = "Year",
       y = "Top State Tax Rate")

ggplot(data = nc_wi) + geom_line(aes(x = year, y = millionaire_share, group = state_full,
                                     color = state_full)) +
  labs(title = "Millionaire Share by State (2010-2016)",
       x = "Year",
       y = "Fraction of Nationwide Tax Filers with $1m+ AGI in State")

question_1_6 <- nc_wi %>% 
  filter(year != 2013) %>% 
  mutate(control_years = case_when(
    year < 2013 ~ 1,
    year > 2013 ~ 0))

question_1_6 %>% 
  group_by(state, control_years) %>% 
  summarize(avg_million_share = mean(millionaire_share))

#Question 2.1

question_2_1 <- forbes400 %>%
  mutate(num_richer = 401 - row_number(),
         log_num_richer = log(num_richer),
         log_networth = log(networth))

ggplot(question_2_1, aes(x=log_networth, y=log_num_richer)) +
  geom_point() +
  geom_smooth(aes(x = log_networth, y = log_num_richer), method = "lm", se = F)

#Question 2.2

lm(log_networth~log_num_richer, data=question_2_1)

#Question 2.3

0.7408/(1 - 0.7408)
