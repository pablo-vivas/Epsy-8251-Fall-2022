# Load Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(corrr)
library(broom)

#### Preparation ####
#Read in data
data = read_csv(file ="https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/fertility.csv")


# Standardize the outcome and predictor
data = data %>%
  mutate(
    z_fertility_rate = (fertility_rate - mean(fertility_rate)) / sd(fertility_rate),
    z_contraceptive = (contraceptive - mean(contraceptive)) / sd(contraceptive),
    z_infant_mortality = (infant_mortality - mean(infant_mortality))/sd(infant_mortality),
    z_educ_female = (educ_female - mean(educ_female))/sd(educ_female)
  )


#### Part 1 ####
#### Q1 ####
data %>%
  select(z_fertility_rate, z_contraceptive, z_infant_mortality, z_educ_female) %>%
  correlate()%>%
  print(width= Inf)


#### Q2 ####
lm.1 = lm(z_fertility_rate ~ 1 + z_contraceptive, data)
lm.1
tidy(lm.1, conf.int = TRUE, conf.level = 0.95)%>%
  print(width= Inf)
glance(lm.1)
lm.2 = lm(z_fertility_rate ~ 1 + z_infant_mortality + z_contraceptive, data)
lm.2
tidy(lm.2, conf.int = TRUE, conf.level = 0.95)%>%
  print(width= Inf)
glance(lm.2)
lm.3 = lm(z_fertility_rate ~ 1 + z_educ_female + z_infant_mortality + z_contraceptive, data)
lm.3
tidy(lm.3, conf.int = TRUE, conf.level = 0.95)%>%
  print(width= Inf)
glance(lm.3)



#### Q3 ####
library(dotwhisker)
mod_1 = tidy(lm.1) %>%
  mutate(model = "Model 1")
mod_2 = tidy(lm.2) %>%
  mutate(model = "Model 2")
mod_3 = tidy(lm.3) %>%
  mutate(model = "Model 3")

all_models = rbind(mod_1, mod_2, mod_3)

# Create plot
pdf("q3.pdf")
dwplot(all_models, show_intercept = FALSE) +
  theme_bw() +
  scale_color_manual(name = "Model", values = c("#c62f4b", "#c62f4b", "#c62f4b")) +
  scale_x_continuous(name = "Estimate") +
  scale_y_discrete(name = "Coefficients", 
                   labels = c("Education for females", "Infant mortality rate", "Contraceptive rate")) +
  facet_wrap(~model) +
  guides(color = "none")
dev.off()



#### Q6 ####
anova(lm.3)

#### Q9 ####
glance(lm.3)

#### Q12 ####
pdf("q12.pdf")
ggplot(data = data, aes(x = z_contraceptive, y = z_fertility_rate)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Contraceptive rate") +
  ylab("Fertility rate") +
  geom_abline(intercept = -0.418, slope = -0.206, color = "#003f5c", linetype = "dotdash") +
  geom_abline(intercept = 1.608, slope = -0.206, color = "#f26419", linetype = "solid") +
  annotate(geom = "text", x=-0.5, y=-0.1, angle=-13, label = "Infant mortality rate: -0.99",size=2) +
  annotate(geom = "text", x=-0.5, y=1.9, angle=-13,label = "Infant mortality rate: 3.81",size=2) 
dev.off()
