rm(list=ls())

#install packages 
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("sjPlot")

#libraries
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(sjPlot)
#=================================================================================
MMP DATA (MATERIAL MILITARY POWER)
#=================================================================================

MMP_Data <- read_excel("Souva MMP Data.xlsx")
View(MMP_Data)

#Repeat process from CINC
MMP_Data <- MMP_Data %>%
  filter(ccode == 666| #Israel
           ccode == 663| #Jordan
           ccode == 645| #Iraq 
           ccode == 651| #Egypt
           ccode == 652| #Syria
           ccode == 660) #Lebanon
#rename for nicer plots
MMP_Data <- MMP_Data %>% 
  mutate(country = recode(stateabb,
                          "ISR" = "Israel",
                          "EGY" = "Egypt",
                          "JOR" = "Jordan",
                          "SYR" = "Syria",
                          "IRQ" = "Iraq",
                          "LEB" = "Lebanon"))

#only want years close to before and after war
MMP_Data <- MMP_Data %>%
  filter(year >=1938, 
         year <= 1958)

#Plot MMP Scores
#plot the power for each country.
library(scales)  # for comma() or label_number()

ggplot(MMP_Data, aes(x = year, y = MMP, color = country)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 15) +
  labs(
    title = "MMP Scores Over Time",
    x = "Year",
    y = "MMP Score",
    color = "Country"
  ) +
  scale_y_continuous(labels = label_number()) +  # disables scientific notation
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )+
  geom_vline(xintercept = 1948, 
             linetype = "dotted", 
             color = "black", 
             size = 1)

#extrapolate using linear model
#extrapolating for israel
israel_data <- subset(MMP_Data, country == "Israel")
prediction_model <- lm(MMP ~ year, data = israel_data)
summary(prediction_model)

#nice table for regression
tab_model(prediction_model,
          digits = 6,
          show.se = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          title = "Linear Model: MMP ~ Year (Israel)",
          dv.labels = "Military Material Power (MMP)",
          string.pred = "Predictor",
          string.est = "Estimate")

#plot to make sure linear is reasonable
ggplot(data = israel_data, aes(x = year, y = MMP)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") + # Add linear regression line with confidence interval
  labs(title = "Israel Regression Plot", x = "Year", y = "MMP Score") +
  theme_minimal()

#Yes, evidence for a linear relationship for Israel

#get 1938-1947 predictions
past_years <- data.frame(year = 1938:1947)
predictions <- predict(prediction_model, newdata = past_years)

#make data for extrapolation
israel_extrapolated <- data.frame(
  country = "Israel",
  year = 1938:1947,
  MMP = predictions,
  ccode = 666
)

#combine 
israel_extended <- bind_rows(israel_data, israel_extrapolated)

#clean up
rm(israel_data, israel_extrapolated, predictions, past_years, prediction_model)

#merge to full dataset
MMP_Data <- MMP_Data %>%
  filter(ccode != 666) %>%
  bind_rows(israel_extended) %>%
  arrange(ccode, year) %>%
  mutate(stateabb = if_else(ccode == 666, "ISR", stateabb))%>%
  mutate(MMP = if_else(MMP < 0, 0, MMP))

#clean up
rm(israel_extended)

#export clean dataset
write_csv(MMP_Data, file = "MMP_Data.csv")

#now plot for extrapolation
ggplot(MMP_Data, aes(x = year, y = MMP, color = country)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 15) +
  labs(
    title = "MMP Scores Over Time",
    x = "Year",
    y = "MMP Score",
    color = "Country" +
  theme(plot.title = element_text(hjust = 0.5))+
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  geom_vline(xintercept = 1948, 
             linetype = "dotted", 
             color = "black", 
             size = 1)
#=================================================================================
# CINC DATA (COMPOSITE INDEX OF NATIONAL CAPABILITY)
#=================================================================================
#import cow Data Set
cow_data <- read_csv("COW NMC v 6 (1).csv") #initial read
View(cow_data) #view dataset

#only want Israel, Jordan, Iraq, Egypyt, Lebanon, and Syria
cow_data <- cow_data %>%
  filter(ccode == 666| #Israel
           ccode == 663| #Jordan
           ccode == 645| #Iraq 
           ccode == 651| #Egypt
           ccode == 652| #Syria
           ccode == 660) #Lebanon
#rename for nicer plots
cow_data <- cow_data %>% 
  mutate(country = recode(stateabb,
                          "ISR" = "Israel",
                          "EGY" = "Egypt",
                          "JOR" = "Jordan",
                          "SYR" = "Syria",
                          "IRQ" = "Iraq",
                          "LEB" = "Lebanon"))

#only want years close to before and after war
cow_data <- cow_data %>%
  filter(year >=1938, 
         year <= 1958)

#plot the power for each country.
ggplot(cow_data, aes(x = year, y = cinc, color = country)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 15) +
  labs(
    title = "CINC Scores Over Time",
    x = "Year",
    y = "CINC Score",
    color = "Country"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
#extrapolate using linear model
#extrapolating for israel
israel_data <- subset(cow_data, country == "Israel")
prediction_model <- lm(cinc ~ year, data = israel_data)
summary(prediction_model)

#plot to make sure linear is reasonable
ggplot(data = israel_data, aes(x = year, y = cinc)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") + # Add linear regression line with confidence interval
  labs(title = "Linear Regression Plot", x = "Year", y = "CINC Score") +
  theme_minimal()

#Yes, evidence for a linear relationship for Israel

#get 1938-1947 predictions
past_years <- data.frame(year = 1938:1947)
predictions <- predict(prediction_model, newdata = past_years)

#make data for extrapolation
israel_extrapolated <- data.frame(
  country = "Israel",
  year = 1938:1947,
  cinc = predictions,
  ccode = 666
)
#combine 
israel_extended <- bind_rows(israel_data, israel_extrapolated)

#clean up
rm(israel_data, israel_extrapolated, predictions, past_years, prediction_model)

#merge to full dataset
cow_data <- cow_data %>%
  filter(ccode != 666) %>%
  bind_rows(israel_extended) %>%
  arrange(ccode, year) %>%
  mutate(stateabb = if_else(ccode == 666, "ISR", stateabb))

#clean up
rm(israel_extended)

#export clean data
write_csv(cow_data, file = "cow_data_clean.csv")

#now plot for extrapolation
ggplot(cow_data, aes(x = year, y = cinc, color = country)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 15) +
  labs(
    title = "CINC Scores Over Time",
    x = "Year",
    y = "CINC Score",
    color = "Country"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )+
  geom_vline(xintercept = 1948, 
             linetype = "dotted", 
             color = "black", 
             size = 1)
