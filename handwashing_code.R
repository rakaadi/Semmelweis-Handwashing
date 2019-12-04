# Library
library(tidyverse)
library(lubridate)

# read the yearly data by clinic
Yearly_deaths <- read_csv("yearly_deaths_by_clinic.csv")
print(Yearly_deaths)

  # Add proportion of deaths
Yearly_deaths %>%
  mutate(proportion_deaths = deaths / births) -> Yearly_deaths
head(Yearly_deaths)

  # ploting proportion of deaths at both clinic
Yearly_deaths %>%
  ggplot(aes(x = year, y = proportion_deaths, color = clinic)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Proportion Deaths") +
  theme_minimal() +
  theme(legend.title = element_blank())

# read the monthly data from clinic 1
Monthly_deaths <- read_csv("monthly_deaths.csv", col_types = cols(
  date = col_date(format = "")))

  # Add proportion of deaths
Monthly_deaths %>%
  mutate(proportion_deaths = deaths / births) -> Monthly_deaths
head(Monthly_deaths)

  # ploting monthly Deaths&Births 
ggplot(Monthly_deaths) +
  geom_segment(aes(x=date, xend=date, y=deaths, yend=births), color="grey")+
  geom_point(aes(x=date, y =births), color="dodgerblue4") +
  geom_point(aes(x=date, y=deaths), color="darkred") +
  theme_minimal() +
  labs(x = "Year", y = "Deaths & Births Number")

  # Highligting handwashing plot
Monthly_deaths %>%
  mutate(handwashing_started = date >= as_date('1847-06-01')) %>%
  ggplot(aes(x = date, y = proportion_deaths, color = handwashing_started)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Proportion Deaths") +
  theme_minimal()

# Statistical analysis
monthly_sum <- Monthly_deaths %>%
  group_by(handwashing_started) %>%
  summarise(mean_proportion_deaths = mean(proportion_deaths))
print(monthly_sum)

  # Using t-test to calculate 95% confidence interval
t_result <- t.test(proportion_deaths ~ handwashing_started, data = Monthly_deaths)
print(t_result)