# Applied_Data_Wrangling_Visualisation_Project

library(haven)
library(tidyverse)
library(ggplot2)

total_consumption <- read_csv('net-receipts-by-commodity.csv')

total_excise <- read_csv('excise-volumes-commodity.csv')

summary(total_consumption)

total_cons_tob <- subset (total_consumption, commodity_and_head_of_duty == "Tobacco Cigarettes")
total_cons_tob$net_receipts_ <- as.numeric(total_cons_tob$net_receipts_)
total_cons_tob <- total_cons_tob |>
  mutate (net_receipts_mills = net_receipts_/10^6)

theme_set(theme_classic())
important_dates <- (c(2008, 2012, 2020)) # need to sub for actual important tax dates

ggplot(total_cons_tob, aes(x=year, y=net_receipts_mills, label = year)) +
  geom_point() +
  geom_smooth(color = "grey")+
  geom_vline(xintercept = important_dates, 
             color = "blue", linetype = "dashed", size = 1) +
  geom_text() +
  labs(title = "Cigarette Consumption in Ireland",x = "Year", y = "Total Consumption (in m)") 
