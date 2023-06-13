# Packages
library(tidyverse)
library(magrittr)

# Data
data("ToothGrowth")
head(ToothGrowth)

# Plot 1

ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(mean_length = mean(len)) %>%
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supp)) +
  geom_bar(stat = "identity")

# Plot 2
ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(mean_length = mean(len)) %>%
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supp)) +
  geom_bar(stat = "identity",
           position = "dodge")

# Plot 3
ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(mean_length = mean(len)) %>%
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supp)) +
  geom_bar(stat = "identity",
           position = "dodge", 
           colour = "#FFFFFF",
           size = 2)

# Plot 4
ToothGrowth %>%
  mutate(supplement = 
           case_when(supp == "OJ" ~ "Orange Juice",
                     supp == "VC" ~ "Vitamin C",
                     TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           position = "dodge", 
           colour = "#FFFFFF",
           size = 2)

# Plot 5
ToothGrowth %>%
  mutate(supplement = 
           case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           position = "dodge", 
           colour = "#FFFFFF",
           size = 2) +
  theme_minimal()

# Plot 6
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           position = "dodge",
           colour = "#FFFFFF", 
           size = 2) +
  theme_minimal()

# Plot 7
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

# Plot 8
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)",
       title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
       subtitle = "With the highest dose, the mean recorded length was almost identical.") +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

# Plot 9
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)",
       title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
       subtitle = "With the highest dose, the mean recorded length was almost identical.") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

# Plot 10
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose),
         supplement = 
           factor(supplement, 
                  levels = c("Vitamin C", 
                             "Orange Juice"))) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)",
       title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
       subtitle = "With the highest dose, the mean recorded length was almost identical.") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

# Plot 11
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose),
         supplement = 
           factor(supplement, 
                  levels = c("Vitamin C", 
                             "Orange Juice"))) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)",
       title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
       subtitle = "With the highest dose, the mean recorded length was almost identical.") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  coord_flip() +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

# Plot 12
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose),
         supplement = 
           factor(supplement, 
                  levels = c("Vitamin C", 
                             "Orange Juice"))) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2,
           aes(alpha=dose)) + 
  labs(x = "Dose",
       y = "Mean length (mm)",
       title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
       subtitle = "With the highest dose, the mean recorded length was almost identical.") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  coord_flip() +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

# Plot 13
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose),
         supplement = 
           factor(supplement, 
                  levels = c("Vitamin C", 
                             "Orange Juice"))) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2,
           aes(alpha=dose)) + 
  labs(x = "Dose",
       y = "Mean length (mm)",
       title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
       subtitle = "With the highest dose, the mean recorded length was almost identical.") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  scale_alpha(range = c(0.4, 1)) +
  coord_flip() +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()


