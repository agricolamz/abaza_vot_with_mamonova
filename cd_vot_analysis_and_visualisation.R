setwd("/home/agricolamz/work/articles/2019_Abaza_VOT_Mamonova/github_repo")
library(tidyverse)
df <- read_csv("result_all_v2.csv")


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
         poa = factor(poa, c("labial", "dental", "palatal", "velar", "uvular")),
         phonation = factor(phonation, c("plain", "ejective", "voiced"))) %>% 
  select(-start_time, -end_time) %>% 
  group_by(speaker, utterance, word) %>% 
  mutate(sum = sum(duration)) %>% 
  filter(!is.na(cons)) %>% 
  ungroup() %>% 
  slice(-c(3180, 3181, 3183, 3184, 3185, 3186)) %>% 
  spread(annotation, duration, fill = NA) %>% 
  mutate(cd_n = cd/sum,
         vot_n = vot/sum) ->
data

data %>% 
  mutate(vot = ifelse(phonation == "voiced", -vot, vot),
         utterance = str_extract(utterance, "\\d"),
         utterance = ifelse(is.na(utterance), "cf", utterance)) %>% 
  filter(utterance != "4",
         utterance != "5") %>% 
  ggplot(aes(phonation, vot))+
  ggbeeswarm::geom_quasirandom(size = 0.5)+
  facet_grid(utterance~poa)+
  theme_bw()
ggsave("vot_phonation_poa.png", device = "png")

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


data %>% 
  filter(poa != "velar") %>% 
  mutate(vot = ifelse(str_detect(cons, "[бдг]"), -vot, vot)) ->
  data_for_glm

data_for_glm %>% 
  ggplot(aes(poa, vot, fill = phonation))+
  geom_violin()+
  facet_wrap(~speaker)
library(lme4)
library(lmerTest)
fit <- lmer(vot~poa+phonation+(1|speaker)+(1|utterance)+(1|word), data = data_for_glm)
summary(fit)

library(tidyverse)
as.matrix(cf) %>% 
  as.data.frame() %>% 
  mutate(names = rownames(.)) %>% 
  slice(-c(1:3)) %>% 
  mutate(fit = fit@beta) %>% 
  select(names, fit, `2.5 %`, `97.5 %`)

library(effects)
summary(fit)
plot(allEffects(fit))

data %>% 
  filter(poa != "velar") %>% 
  mutate(cons = case_when(
    cons == "б" ~ "b",
    cons == "п" ~ "p",
    cons == "пI" ~ "p'",
    cons == "д" ~ "d",
    cons == "т" ~ "t",
    cons == "тI" ~ "t'",
    cons == "гь" ~ "gʲ",
    cons == "кь" ~ "kʲ",
    cons == "кIь" ~ "k'ʲ",
    cons == "хъ" ~ "q",
    cons == "къ" ~ "q'")) %>% 
  count(cons, utterance, speaker) %>% 
  spread(utterance, n) %>% 
  arrange(speaker) %>% 
  select(-u4, -u5) %>% 
  pivot_longer(names_to = "utterance", values_to = "number", cf:u3) %>% 
  ggplot(aes(cons, number, label = number, fill = utterance))+
  geom_col(position = "dodge")+
  facet_wrap(~speaker)+
  theme_bw()+
  labs(x = "consonant", y = "number of consonants in the survey")
ggsave("cons.png", device = "png")  
  
