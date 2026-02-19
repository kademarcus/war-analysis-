rm(list=ls())
#installs
#install.packages("dplyr")
#install.packages("gt")
#install.packages("haven")

#libraries
library(dplyr)
library(gt)
library(haven)

#Data Reading
War_Data <- read_dta("stata_data.dta")
War_Data <- War_Data %>%
  mutate(cinc = if_else(cinc < 0, 0, cinc))
print(mean(War_Data$cinc,na.rm = TRUE))

#predictive model
initiation_logit <- glm(initiation ~ age + milnoncombat + combat + rebel + warwin +
                          warloss + rebelwin + rebelloss + aut + cinc + tau_lead +
                          officetenure1000 + fiveyearchallengelag,
                        family = "binomial", 
                        data = War_Data) 

summary(initiation_logit)

#predicted probabilities
War_Data <- na.omit(War_Data)
predicted_probabilities <- predict(initiation_logit, type = "response", na.action = na.exclude())
War_Data$predicted_probabilities <- predicted_probabilities

#Israel and Egypt Leaders only
War_Data <- War_Data %>%
  filter(ccode == 666|
           ccode == 651|
           ccode == 663|
           ccode == 645|
           ccode == 660|
           ccode == 652) %>%
  filter(year == 1948)
View(War_Data)

#export clean data
write_csv(War_Data, file = "War_Data.csv")

#table for probabilities
leader_predictions <- War_Data %>%
  select(leadername, predicted_probabilities) %>%
  distinct() %>%   # ensure one row per leader
  arrange(desc(predicted_probabilities))  

leader_predictions %>%
  gt() %>%
  fmt_number(columns = predicted_probabilities, decimals = 3) %>%
  tab_header(title = "Predicted Probabilities for Israeli/Arab Leaders in 1948")
