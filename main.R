rm(list = ls())
library(tidyverse)

df = read.csv("D:/Creative Cloud Files/Projects/tests/damage_through/calibration_hornet.csv", 
              fileEncoding="UTF-8-BOM") %>% as.tibble %>% 
  mutate(damage_min = HP/(Shots),
         damage_max = HP/(Shots - 1))

damage = (min(df$damage_max) + max(df$damage_min))/2

df %>% ggplot(aes(x = damage_min, xend = damage_max, y = Name, yend = Name)) + 
  geom_segment(size=5, color="white") +
  geom_vline(aes(xintercept = min(damage_max)), col = 'deeppink') +
  geom_vline(aes(xintercept = max(damage_min)), col = 'deeppink') +
  geom_vline(xintercept = damage, col = 'deeppink', lty = 2) +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white'),
        panel.grid = element_line(color = 'gray50', linetype = 2))
  geom_vline(xintercept = 2.70864, col = 'darkviolet')
ggsave('C:/Dropbox/temp/hornet_calibration.pdf', width = 1920/400, height = 1080/600)

df = read.csv("D:/Creative Cloud Files/Projects/tests/damage_through/canon.csv", 
              fileEncoding="UTF-8-BOM") %>% as.tibble

df %>% ggplot(aes(x = distance - 1, y = damage)) + 
  geom_point(col = 'white', size = 2) +
  geom_line(col = 'white') +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white'),
        panel.grid = element_line(color = 'gray50', linetype = 2)) +
  scale_x_continuous(breaks = (-1):7, labels = c('NONE', 0:7))
ggsave('C:/Dropbox/temp/canon.pdf', width = 1920/400, height = 1080/400)


df = read.csv("D:/Creative Cloud Files/Projects/tests/damage_through/ehp.csv", 
              fileEncoding="UTF-8-BOM") %>% as.tibble
df$Class = as.factor(c(
  rep('Griles', 5),
  rep('Wheels', 2),
  rep('Weapons', 1),
  rep('Bumpers', 4),
  rep('Frames', 5),
  rep('Lightweight Frames', 5)
))

df <- df %>% 
  mutate(EHP_max = damage * Shots,
         EHP_min = damage * (Shots-1)) %>%
  mutate(dam_through_max = (EHP_max - HP)/EHP_max,
         dam_through_min = (EHP_min - HP)/EHP_min)
 
p <- function(df) df %>% ggplot(aes(x = 1-dam_through_max, xend = 1-dam_through_min, y = Name, yend = Name)) + 
  geom_segment(size=5, color="white") +
    geom_vline(aes(xintercept = 1-min(dam_through_max)), col = 'deeppink') +
    geom_vline(aes(xintercept = 1-max(dam_through_min)), col = 'deeppink') +
    geom_vline(aes(xintercept = 1-(min(dam_through_max) + max(dam_through_min))/2), col = 'deeppink', lty = 2) +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white'),
        panel.grid = element_line(color = 'gray50', linetype = 2))

ps <- df %>% split(df$Class) %>% map(p)

ps[[1]] + geom_vline(xintercept = 1-0.25, col = 'darkviolet')
ggsave('C:/Dropbox/temp/ehp1.pdf', width = 1920/400, height = 1080/600)
ps[[2]] + geom_vline(xintercept = 1-0.90, col = 'darkviolet')
ggsave('C:/Dropbox/temp/ehp2.pdf', width = 1920/400, height = 1080/600)
ps[[3]] + geom_vline(xintercept = 1-0.90, col = 'darkviolet')
ggsave('C:/Dropbox/temp/ehp3.pdf', width = 1920/400, height = 1080/600)
ps[[4]] + geom_vline(xintercept = 1-0.90, col = 'darkviolet')
ggsave('C:/Dropbox/temp/ehp4.pdf', width = 1920/400, height = 1080/600)
ps[[5]] + geom_vline(xintercept = 1-0.50, col = 'darkviolet')
ggsave('C:/Dropbox/temp/ehp5.pdf', width = 1920/400, height = 1080/600)
ps[[6]] + geom_vline(xintercept = 1-0.50, col = 'darkviolet')
ggsave('C:/Dropbox/temp/ehp6.pdf', width = 1920/400, height = 1080/600)



