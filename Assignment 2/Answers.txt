#Read libraries

library(tidyverse)
library(readr)

#Import Dataset

state_education <- readr::read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8251/main/data/state-education.csv")

#Convert character as factor and create salary_thousand

data <- state_education %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(salary_thousand = salary_2020_21/1000)


# Q1 ----------------------------------------------------------------------

pdf("q1.pdf")
data %>% 
  ggplot(aes(x=sat_total)) +
  geom_histogram(fill = "#ffcc33", color = "#7a0019", alpha = 0.6, binwidth=20) +
  scale_x_continuous(limits = c(890,1310), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,5.5),expand = c(0, 0)) +
  xlab("Total SAT Score") +
  ylab("Frequency") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text=element_text(size=12))
dev.off()

# Q2 ----------------------------------------------------------------------

data %>% 
  summarise(
    M_sat = mean(sat_total),
    Sd_sat = sd(sat_total),
    M_salaries = mean(salary_thousand),
    Sd_salaries = sd(salary_thousand)
  )

# Q3 ----------------------------------------------------------------------

pdf("q3.pdf")
data %>% 
  ggplot(aes(y=sat_total, x=salary_thousand))+
  geom_point(color = "#7a0019", size = 4) +
  ylab("Total SAT Score")+
  xlab("Teacher Salary (in thousands)")+
  theme_bw()+
  theme(axis.title = element_text(size = 15),
        axis.text=element_text(size=12))
dev.off()

# Q4 ----------------------------------------------------------------------
# Q5 ----------------------------------------------------------------------

m <- lm(sat_total ~ 1 + salary_thousand, data = data)

# Q6 ----------------------------------------------------------------------
# Q7 ----------------------------------------------------------------------
# Q8 ----------------------------------------------------------------------

lm(sat_total ~ 1, data = data)

anova(lm(sat_total ~ 1 + salary_thousand, data = data))
anova(lm(sat_total ~ 1, data = data))
