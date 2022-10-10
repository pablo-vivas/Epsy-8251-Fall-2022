# Load Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(corrr)

#### Part 1: Corr ####

#Read in data
data = read_csv(file ="https://raw.githubusercontent.com/zief0002/epsy-8251/main/data/state-education.csv")

data$salary_thousand = data$salary_2020_21/1000

#### Q1 ####

data %>%
  select(sat_total, salary_thousand) %>%
  correlate()
#corr: -0.319 #weak neagtive corr

'''
data %>%
  select(sat_math, salary_thousand) %>%
  correlate()

data %>%
  select(sat_ebrw, salary_thousand) %>%
  correlate()
'''


#### Q2 ####
pdf("q2.pdf")
ggplot( data, aes(x = salary_thousand, y = sat_total) ) +
  geom_point(color = "#7a0019", size = 2) +
  theme_bw() +
  xlab("Average Total SAT Score in the State") +
  ylab("Average 2020–2021 Public Teacher Salary in the State")+
  theme_bw()+
  theme(axis.title = element_text(size = 15),
        axis.text=element_text(size=10))
dev.off()


#### Part 2 : Regression with a Mean Centered Predictor ####

#### Q3 ####
# mean centered teacher salary
data = data %>%
  mutate(
    c_salary = (salary_thousand - mean(salary_thousand)),
  )

data %>%
  summarize(
    SD  = sd(c_salary)
  )
#sd : 10.8

data %>%
  select(c_salary, sat_total) %>%
  correlate()
#-0.319

#### Q4 ####
data |>
  filter(state == "Minnesota")|>
  print(width= Inf)


#### Q7 ####
lm.c = lm(sat_total ~ 1 + c_salary, data)
lm.c

#intercept: 1100.353, c_salary: -2.631 

#### Part 3: Standardized Regression ####

# Standardize the outcome and predictor
data = data %>%
  mutate(
    z_salary = (salary_2020_21 - mean(salary_2020_21)) / sd(salary_2020_21),
    z_sat = (sat_total - mean(sat_total)) / sd(sat_total)
  )

#### Q10 ####
# Fit standardized regression
lm.z = lm(z_sat ~ 1 + z_salary, data)
lm.z

#Minnesota_predicted sat 
0 - 0.3191 * (-0.2466770)
#0.07871463


#error
1.755260 - 0.07871463
#1.676545
