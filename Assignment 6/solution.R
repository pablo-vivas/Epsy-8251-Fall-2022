# Solution
install.packages("patchwork")
# Load Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(educate)
library(patchwork)

#### Part 1: Assumptions for Simple Regression ####
#Read in data
data = read_csv(file ="https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/evaluations.csv")

#Fit simple regression model
lm.a = lm(avg_eval ~ 1 + beauty, data = data)
lm.a
#### Q1 ####
pdf("q1.pdf")
ggplot(data=data, aes(x=avg_eval)) +
  stat_density(geom="line") +
  theme_bw() +
  xlab("Average Course Rating") +
  ylab("Probability density")
dev.off()

#### Q2 ####
pdf("q2.pdf")
ggplot(data = data, aes(x = beauty, y = avg_eval)) +
  geom_abline(intercept = 3.9059, slope = 0.1094)+
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw() +
  xlab("Average Beauty of Professor") +
  ylab("Average Course Rating") 
dev.off()


#### Q3 ####
# Augment the model to get residuals
aug_a = augment(lm.a)

# Density plot of the std residuals
pdf("q3.pdf")
ggplot(data = aug_a, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  stat_density_confidence(model = "normal") +
  xlab("Standardized residual") +
  ylab("Probability density")
dev.off()

#### Q4 ####
# Create ID variable in the augmented data
aug_a = aug_a %>% 
  mutate(id = row.names(data))

# Create different data sets for the extreme and non-extreme observations
extreme = aug_a %>% 
  filter(.std.resid <= -3 | .std.resid >= 3)

nonextreme = aug_a %>% 
  filter(.std.resid > -3 & .std.resid < 3)

# Plot using text for the extreme observations and points for the non-extreme
pdf("q4.pdf")
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")
dev.off()


#### Q5 ####
pdf("q5.pdf")
ggplot(data = aug_a, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")
dev.off()


#### Part2 ####
#Read in data
fertility = read_csv(file ="https://raw.githubusercontent.com/zief0002/epsy-8251/main/data/fertility.csv")

#Fit regression model
lm.b = lm(fertility_rate ~ 1 + contraceptive + educ_female + infant_mortality, data = fertility)

#### Q8 ####
# Density plot
pdf("q8.pdf")
ggplot(data = fertility, aes(x = fertility_rate)) +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Fertility rate") +
  ylab("Probability density")
dev.off()

#### Q9 ####
lm.1 = lm(fertility_rate ~ 1 + contraceptive, data = fertility)
lm.1
lm.2 = lm(fertility_rate ~ 1 + educ_female, data = fertility)
lm.2
lm.3 = lm(fertility_rate ~ 1 + infant_mortality, data = fertility)
lm.3

# Create plot 1
p1 = ggplot(data = fertility, aes(x = contraceptive, y = fertility_rate)) +
  geom_point() +
  geom_abline(intercept = 5.51633, slope = -0.05019)+
  geom_smooth(method = "loess") +
  theme_bw() +
  xlab("Contraceptive Rate") +
  ylab("Fertility Rate") 

# Create plot 2
p2 = ggplot(data = fertility, aes(x = educ_female, y = fertility_rate)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_abline(intercept = 5.0590, slope = -0.3143)+
  theme_bw() +
  xlab("Education for Females") +
  ylab("Fertility Rate") 

# Create plot 3
p3 = ggplot(data = fertility, aes(x = infant_mortality, y = fertility_rate)) +
  geom_point() +
  geom_abline(intercept = 1.47604, slope = 0.04712)+
  geom_smooth(method = "loess") +
  theme_bw() +
  xlab("Infant Mortality Rate") +
  ylab("Fertility Rate") 

# Put plots side-by-side
pdf("q9.pdf")
p1 / p2 / p3
dev.off()


#### Q10 ####
# Augment the model to get residuals
aug_b = augment(lm.b)

# Density plot of the std residuals
pdf("q10.pdf")
ggplot(data = aug_b, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  stat_density_confidence(model = "normal") +
  xlab("Standardized residual") +
  ylab("Probability density")
dev.off()


#### Q11 ####
# Create country variable in the augmented data
aug_c<-cbind(fertility$country, aug_b)
colnames(aug_c)[which(names(aug_c) == "fertility$country")] <- "country"

# Create different data sets for the extreme and non-extreme observations
extreme = aug_c %>% 
  filter(.std.resid <= -3 | .std.resid >= 3)

nonextreme = aug_c %>% 
  filter(.std.resid > -3 & .std.resid < 3)

# Plot using text for the extreme observations and points for the non-extreme
pdf("q11.pdf")
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = country), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")
dev.off()


#### Q12 ####
pdf("q12.pdf")
ggplot(data = aug_b, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")
dev.off()
