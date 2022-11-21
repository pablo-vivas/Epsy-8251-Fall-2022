# Solution 

#Read libraries
library(tidyverse)
library(readr)
library(broom)
library(educate)

#Set the decimal places to 3

#Import Dataset
evaluation <- readr::read_csv(file = "https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/evaluations.csv")

# Q1 ----------------------------------------------------------------------

m_1 <- lm(avg_eval ~ native_english, data = evaluation)

tidy(m_1)
glance(m_1)


# Q7 ----------------------------------------------------------------------

m_2 <- lm(avg_eval ~ native_english + beauty + num_courses, data = evaluation)

tidy(m_2)
glance(m_2)


# Q13 ---------------------------------------------------------------------

aug_f = augment(m_2)

pdf("q13.pdf")
ggplot(data = aug_f, aes(x = .std.resid)) +
  stat_density_confidence(model ="normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")
dev.off()



# Q14 ---------------------------------------------------------------------

aug_f = aug_f %>% 
  mutate(id = row_number())

extreme = aug_f %>% 
  filter(.std.resid <= -3 | .std.resid >= 3)

nonextreme = aug_f %>% 
  filter(.std.resid > -3 & .std.resid < 3)

pdf("q14.pdf")
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(data = aug_f, method = "loess") +
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  geom_hline(yintercept = 3, lty = "dashed") + 
  geom_hline(yintercept = -3, lty = "dashed") + 
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")
dev.off()
