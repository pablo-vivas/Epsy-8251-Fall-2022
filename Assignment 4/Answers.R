#Read libraries
library(tidyverse)
library(readr)
library(broom)

#Import Dataset
state_education <- readr::read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8251/main/data/state-education.csv")

#Convert character as factor and create salary_thousand
data <- state_education %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(salary_thousand = salary_2020_21/1000)

m_1 <- lm(sat_total ~ 1 + salary_thousand, data = data)

tidy(m_1, conf.int = T)

# Q6 ----------------------------------------------------------------------

pdf("q6.pdf")
ggplot( data, aes(x = salary_thousand, y = sat_total) ) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm", 
    color = "#7a0019", 
    fill = "#ffcc33"
  ) +
  ylab("Average Total SAT Score in the State") +
  xlab("Average 2020–2021 Public Teacher Salary in the State")+
  theme(axis.title = element_text(size = 15),
        axis.text=element_text(size=10)) +
  theme_bw()
dev.off()


data <- data %>% 
  mutate(center_salary_thousand = salary_thousand - mean(salary_thousand))

m_2 <- lm(sat_total ~ 1 + center_salary_thousand, data = data)

tidy(m_2, conf.int = T)
anova(m_2)
