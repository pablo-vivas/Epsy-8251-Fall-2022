#Solution

# Load Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(educate)
library(patchwork)
library(corrr)

#### Preparation : Create Dummy Variables ####
#Read in data
data = read_csv(file ="https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/scoobydoo.csv")

# Create all dummy variables
data = data %>%
  mutate(
    Shaggy_Scooby = if_else(caught_by == "Shaggy/Scooby", 1, 0),
    Fred_Daphne_Velma = if_else(caught_by == "Fred/Daphne/Velma", 1, 0),
    Combo = if_else(caught_by == "Combo", 1, 0),
    Other = if_else(caught_by == "Other", 1, 0),
    TV = if_else(format == "TV", 1, 0),
    Movie = if_else(format == "Movie", 1, 0)
  )

#### Description ####
#### Q1 ####
data %>%
  select(engagement, Shaggy_Scooby, Fred_Daphne_Velma, Combo, Other, 
         imdb_rating, catchphrase, TV) %>%
  correlate()

#### Unadjusted Group Differences Model: ANOVA ####
#### Q3 ####
#Fit regression model
lm.a = lm(engagement ~ 1 + Fred_Daphne_Velma + Combo + Other, data = data)
lm.a

#### Q4 ####
# Coefficient-level info
tidy(lm.a)

#### Adjusted Group Differences Model: ANCOVA ####
#### Q7 ####
#Fit regression model
lm.b = lm(engagement ~ 1 + imdb_rating + catchphrase + TV + Fred_Daphne_Velma + Combo + Other, data = data)
lm.b

#### Assumptions ####
#### Q10 ####
# Augment the model to get residuals
aug_b = augment(lm.b)

# density plot of the marginal distribution of the standardized residuals from the ANCOVA model
pdf("q1.pdf")
ggplot(data = aug_b, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  stat_density_confidence(model = "normal") +
  xlab("Standardized residual") +
  ylab("Probability density")
dev.off()


#### Q11 ####
# Create ID variable in the augmented data
aug_b = aug_b %>% 
  mutate(id = row.names(data))

# Create different data sets for the extreme and non-extreme observations
extreme = aug_b %>% 
  filter(.std.resid <= -3 | .std.resid >= 3)

nonextreme = aug_b %>% 
  filter(.std.resid > -3 & .std.resid < 3)

# Plot using text for the extreme observations and points for the non-extreme
pdf("q2.pdf")
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(data = aug_b, method = "loess") +
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  geom_hline(yintercept = 3, lty = "dashed") + 
  geom_hline(yintercept = -3, lty = "dashed") + 
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")
dev.off()


#### Q13 ####
lm.c = lm(engagement ~ 1 + imdb_rating + catchphrase + TV + Fred_Daphne_Velma + Shaggy_Scooby + Other, data = data)
lm.c

tidy(lm.c)
glance(lm.b)

pvals <- c(0.18, 0.257, 0.0276, 0.723, 0.013)
bh_p <- p.adjust(pvals, method = "BH")

# Heatmap -----------------------------------------------------------------

x = y = c("SS", "FDV", "Combo", "Other")
h_data <- expand.grid(X=x, Y=y)
h_data <- h_data %>% 
  mutate(pval = c(0, 0.270, 0.308, 0.055,
                  0.270, 0, 0.723, 0.008,
                 0.308, 0.723, 0, 0.039,
                 0.055, 0.008, 0.039, 0),
         color = case_when(pval == 0 ~ "0",
                           pval > 0 & pval < 0.01 ~ "1",
                           pval < 0.05 & pval >= 0.01 ~ "2",
                           pval >= 0.05 ~ "3"))


ggplot(h_data, aes(X, Y, fill= color)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = pval), color = "white", size = 4) +
  scale_fill_manual("P-value",
                    values = c("#FFFFFF", "#c61a09", "#ff8164", "#ffc9bb"),
                    labels = c("0", "< 0.01", "< 0.05", "> 0.05")) +
  coord_fixed() +
  theme_void() +
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text())
