setwd("/home/agricolamz/work/articles/2019_Abaza_VOT_Mamonova")
library(tidyverse)
df <- read_csv("result_all.csv")


df %>% 
  filter(duration < 250) %>% 
  mutate(cons = factor(cons, c("б", "п", "пI", "д", "т", "тI", "гь", "кь", "кIь", "г", "к", "кI", "хъ", "къ")),
         poa = case_when(cons == "б" ~ "labial", 
                         cons == "п" ~ "labial", 
                         cons == "пI" ~ "labial", 
                         cons == "д" ~ "dental",
                         cons == "т" ~ "dental", 
                         cons == "тI" ~ "dental", 
                         cons == "гь" ~ "palatal", 
                         cons == "кь" ~ "palatal", 
                         cons == "кIь" ~ "palatal", 
                         cons == "г" ~ "velar", 
                         cons == "к" ~ "velar", 
                         cons == "кI" ~ "velar", 
                         cons == "хъ" ~ "uvular", 
                         cons == "къ" ~ "uvular"),
         phonation = case_when(cons == "б" ~ "voiced", 
                         cons == "п" ~ "plain", 
                         cons == "пI" ~ "ejective", 
                         cons == "д" ~ "voiced",
                         cons == "т" ~ "plain", 
                         cons == "тI" ~ "ejective", 
                         cons == "гь" ~ "voiced", 
                         cons == "кь" ~ "plain", 
                         cons == "кIь" ~ "ejective", 
                         cons == "г" ~ "voiced", 
                         cons == "к" ~ "plain", 
                         cons == "кI" ~ "ejective", 
                         cons == "хъ" ~ "plain", 
                         cons == "къ" ~ "ejective"),
         poa = factor(poa, c("labial", "dental", "palatal", "velar", "uvular"))) %>% 
  select(-start_time, -end_time) %>% 
  group_by(speaker, utterance, word) %>% 
  mutate(sum = sum(duration)) %>% 
  filter(!is.na(cons)) %>% 
  ungroup() %>% 
  slice(-c(3846, 3850, 3849, 3851, 3852, 3847)) %>% 
  spread(annotation, duration, fill = NA) %>% 
  mutate(cd_n = cd/sum,
         vot_n = vot/sum) ->
data

data %>% 
  ggplot(aes(cd, vot, label = cons, color = phonation))+
  geom_text()+
  facet_grid(poa~speaker, scales = "free")+
  stat_ellipse()+
  labs(title = "Absolute values of CD and VOT")

data %>% 
  ggplot(aes(cd_n, vot_n, label = cons, color = phonation))+
  geom_text()+
  facet_grid(poa~speaker, scales = "free")+
  stat_ellipse()+
  labs(title = "Values of CD and VOT normolized with adjacent vowels")
