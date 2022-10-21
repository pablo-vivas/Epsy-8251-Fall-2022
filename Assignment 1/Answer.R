# Load Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

#### Part 1: ####

#Read in data
broad <- read_csv(file ="https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/broadband.csv")

mn <- broad |> 
  filter(state == "MN")

#### Q1 ####
pdf("q1.pdf")
ggplot(data = mn, aes(microsoft_useage)) +
  geom_density(fill = "#ffcc33", color = "#7a0019", alpha = 0.6)+
  theme_bw()+
  scale_x_continuous(limits = c(0,1.05), expand=c(0,0))+
  scale_y_continuous(limits = c(0, 2.7), expand = c(0,0))+
  xlab("Proportion")+
  ylab("Density")+
  theme(axis.title = element_text(size = 20),
        axis.text=element_text(size=10))
dev.off()

#### Q2 ####
pdf("q2.pdf")
ggplot(data = mn, aes(microsoft_useage))+
  geom_density(aes(fill = metro), alpha = 0.6)+
  scale_fill_manual(
    name = "County Classification",
    values = c("#ffcc33","#7a0019"),
    labels = c("Metro", "Nonmetro")
  )+
  theme_bw()+
  scale_x_continuous(limits = c(0,1.05), expand=c(0,0))+
  scale_y_continuous(limits = c(0, 3.2), expand = c(0,0))+
  xlab("Proportion")+
  ylab("Density")+
  theme(axis.title = element_text(size = 20),
        axis.text=element_text(size=10))
dev.off()

#### Q3 ####
mn |>
  group_by(metro) |>
  summarize(
    M = mean(microsoft_useage, na.rm = TRUE),
    SD = sd(microsoft_useage, na.rm = TRUE),
    N = n())
)

#### Q4 ####

#### Part 2: ####
nodc <- broad |> 
  filter(state != "DC")

#### Q5 ####
micro_nodc <- nodc |>
  group_by(state) |>
  summarize(
    M_1 = mean(microsoft_useage, na.rm = TRUE),
    SD_1 = sd(microsoft_useage, na.rm = TRUE))
    M_2 = mean(fcc_availability, na.rm = TRUE),
    SD_2 = sd(fcc_availability, na.rm = TRUE))
)


#### Q6 ####
nodc |>
  group_by(state) |>
  summarize(
    M = mean(microsoft_useage, na.rm = TRUE),
    SD = sd(microsoft_useage, na.rm = TRUE))
)


#### Q7 ####
broad |> 
  filter(state %in% c("MN","IA","SD","ND","WI")) |>
  group_by(state) |>
  summarize(
    M_1 = mean(microsoft_useage, na.rm = TRUE),
    SD_1 = sd(microsoft_useage, na.rm = TRUE),
    M_2 = mean(fcc_availability, na.rm = TRUE),
    SD_2 = sd(fcc_availability, na.rm = TRUE)
) 


#### Part 3 ####
#### Q8 ####
part3 <- broad |>
  group_by(state) |>
  summarize(
    proportion = mean(microsoft_useage, na.rm = TRUE),
    poverty = mean(pct_poverty,na.rm = TRUE))
)


pdf("q8.pdf")
ggplot(data = part3, aes(y=proportion, x=poverty))+
  geom_point(color = "#7a0019", size = 2) +
  xlab("Mean Poverty Percentage")+
  ylab("Mean Broadband Access Proportion")+
  theme_bw()+
  theme(axis.title = element_text(size = 20),
        axis.text=element_text(size=10))
dev.off()
