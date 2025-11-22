# Applied_Data_Wrangling_Visualisation_Project

library(haven)
library(tidyverse)
library(ggplot2)
library(ggrepel)

e_cig_data <- read_csv('ECigarette.csv')
e_cig_data_all <- e_cig_data |>
  filter(Sex == "Both sexes")

smoking_data <- read_csv('Smoking.csv') 
smoking_data_all <- smoking_data |>
  filter(Sex == "Both sexes")

smoking_v_ecig <- inner_join(smoking_data_all, e_cig_data_all, by = c("Year", "Age Group", "Sex"))

ggplot(smoking_v_ecig, aes(x = Year))+
  geom_smooth(se=FALSE, aes(y= VALUE.x, colour = "Tobacco Cigarette"), size = 0.5) +
  geom_smooth(se=FALSE, aes(y= VALUE.y, colour = "e-Cigarette"), size = 0.5) +
  facet_wrap(~ `Age Group`)+
  theme_minimal()+
  scale_x_continuous(breaks = scales::pretty_breaks())+
  scale_colour_manual(values = c("#004488", "#BB5566"))+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.justification = "center",
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold")
  ) +
  labs(
    x = "Year",
    y = "% of People Using each Tobacco Product",
    colour = "Tobacco Product",
    title = "Tobacco Use in Ireland since 2015",
    subtitle = "By Age Group and Product Type"
  )


ggsave("Tobacco Cigs vs e-Cigs Plot.png")
