library(tidyverse)

nhanes <- read_csv("NHANES.csv")

summary(nhanes)

nhanes %>% ggplot(aes(x = Height, y = Weight, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm")
