library(tidyverse)

#### Study 1 ----

# read data
dat_study_1 <- read_csv("data/results-for-strength-and-2021-09-22-1003.csv")


# Helpful or harmful i.e., impact
help_harm <- dat_study_1 |>
  select(2,43:56) |>
  pivot_longer(2:15,
               names_to = "item",
               values_to = "impact") |>
  mutate(modality_valence = if_else(item == "Q14_1" |
                                      item == "Q14_2" |
                                      item == "Q14_3" |
                                      item == "Q14_5" |
                                      item == "Q14_6" |
                                      item == "Q14_8" |
                                      item == "Q14_10", "Positive", "Negative")) |>
  rename(id = "Q2")


help_harm$modality <- recode(help_harm$item,
                             Q14_1 = "Pleasant states",
                             Q14_2 = "Anger",
                             Q14_3 =  "Motor-behavioural",
                             Q14_4 = "Cognitive",
                             Q14_5 = "Operational",
                             Q14_6 = "Bodily-somatic",
                             Q14_7 = "Motor-behavioural",
                             Q14_8 = "Cognitive",
                             Q14_9 = "Operational",
                             Q14_10 = "Volitional",
                             Q14_11 = "Anxiety",
                             Q14_12 = "Bodily-somatic",
                             Q14_13 =  "Anger",
                             Q14_14 = "Volitional")

help_harm$item <- recode(help_harm$item,
                         Q14_1 = "Enthusiastic, confident, carefree, joyful",
                         Q14_2 = "Fighting spirit, fierce, aggressive",
                         Q14_3 =  "Relaxed-, coordinated-, powerful-, effortless-movement",
                         Q14_4 = "Distracted, overloaded, doubtful, confused",
                         Q14_5 = "Effective-, skilful-, reliable-, consistent-task execution",
                         Q14_6 = "Vigorous, energetic, physically-charged",
                         Q14_7 = "Sluggish, clumsy, uncoordinated, powerless-movement",
                         Q14_8 = "Alert, focused, sharp, attentive",
                         Q14_9 = "Ineffective-, unskilful-, unreliable-task execution",
                         Q14_10 = "Purposeful, determined, persistent, decisive",
                         Q14_11 = "Worried, apprehensive, concerned, troubled",
                         Q14_12 = "Physically-tense, jittery, tired, exhausted",
                         Q14_13 =  "Furious, resentful, irritated, annoyed",
                         Q14_14 = "Unwilling, undetermined, indecisive")


help_harm |>
  ggplot(aes(x=as.ordered(impact-4), group = item)) +
  facet_grid(cols = vars(modality_valence), rows = vars(modality)) +
  geom_bar() +
  geom_text(aes(label = paste((..count..),"(",scales::percent(..prop..), ")"), group = 1), size = 1.25, nudge_y = 5, stat= "count") +
  labs(x = "impact", y = "Count") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))


# Helpful or harmful
performance <- dat_study_1 |>
  select(c(2,58:71,73:86)) |>
  pivot_longer(2:29,
               names_to = "item",
               values_to = "response") |>
  mutate(performance = as.factor(rep(c("BEP", "WEP"), each = 14, length.out = 2548)),
         performance_code = as.factor(rep(c("1", "2"), each = 14, length.out = 2548))) |>
  mutate(modality_valence = if_else(item == "Q15_1" |
                                      item == "Q15_2" |
                                      item == "Q15_3" |
                                      item == "Q15_5" |
                                      item == "Q15_6" |
                                      item == "Q15_8" |
                                      item == "Q15_10" |
                                      item == "Q16_1" |
                                      item == "Q16_2" |
                                      item == "Q16_3" |
                                      item == "Q16_5" |
                                      item == "Q16_6" |
                                      item == "Q16_8" |
                                      item == "Q16_10", "Positive", "Negative"))

performance$modality <- recode(performance$item,
                               Q15_1 = "Pleasant states",
                               Q15_2 = "Anger",
                               Q15_3 =  "Motor-behavioural",
                               Q15_4 = "Cognitive",
                               Q15_5 = "Operational",
                               Q15_6 = "Bodily-somatic",
                               Q15_7 = "Motor-behavioural",
                               Q15_8 = "Cognitive",
                               Q15_9 = "Operational",
                               Q15_10 = "Volitional",
                               Q15_11 = "Anxiety",
                               Q15_12 = "Bodily-somatic",
                               Q15_13 =  "Anger",
                               Q15_14 = "Volitional",
                               Q16_1 = "Pleasant states",
                               Q16_2 = "Anger",
                               Q16_3 =  "Motor-behavioural",
                               Q16_4 = "Cognitive",
                               Q16_5 = "Operational",
                               Q16_6 = "Bodily-somatic",
                               Q16_7 = "Motor-behavioural",
                               Q16_8 = "Cognitive",
                               Q16_9 = "Operational",
                               Q16_10 = "Volitional",
                               Q16_11 = "Anxiety",
                               Q16_12 = "Bodily-somatic",
                               Q16_13 =  "Anger",
                               Q16_14 = "Volitional")

performance$item <- recode(performance$item,
                           Q15_1 = "Enthusiastic, confident, carefree, joyful",
                           Q15_2 = "Fighting spirit, fierce, aggressive",
                           Q15_3 =  "Relaxed-, coordinated-, powerful-, effortless-movement",
                           Q15_4 = "Distracted, overloaded, doubtful, confused",
                           Q15_5 = "Effective-, skilful-, reliable-, consistent-task execution",
                           Q15_6 = "Vigorous, energetic, physically-charged",
                           Q15_7 = "Sluggish, clumsy, uncoordinated, powerless-movement",
                           Q15_8 = "Alert, focused, sharp, attentive",
                           Q15_9 = "Ineffective-, unskilful-, unreliable-task execution",
                           Q15_10 = "Purposeful, determined, persistent, decisive",
                           Q15_11 = "Worried, apprehensive, concerned, troubled",
                           Q15_12 = "Physically-tense, jittery, tired, exhausted",
                           Q15_13 =  "Furious, resentful, irritated, annoyed",
                           Q15_14 = "Unwilling, undetermined, indecisive",
                           Q16_1 = "Enthusiastic, confident, carefree, joyful",
                           Q16_2 = "Fighting spirit, fierce, aggressive",
                           Q16_3 =  "Relaxed-, coordinated-, powerful-, effortless-movement",
                           Q16_4 = "Distracted, overloaded, doubtful, confused",
                           Q16_5 = "Effective-, skilful-, reliable-, consistent-task execution",
                           Q16_6 = "Vigorous, energetic, physically-charged",
                           Q16_7 = "Sluggish, clumsy, uncoordinated, powerless-movement",
                           Q16_8 = "Alert, focused, sharp, attentive",
                           Q16_9 = "Ineffective-, unskilful-, unreliable-task execution",
                           Q16_10 = "Purposeful, determined, persistent, decisive",
                           Q16_11 = "Worried, apprehensive, concerned, troubled",
                           Q16_12 = "Physically-tense, jittery, tired, exhausted",
                           Q16_13 =  "Furious, resentful, irritated, annoyed",
                           Q16_14 = "Unwilling, undetermined, indecisive")

all_dat_study_1 <- performance |>
  rename(id = "Q2") |>
  mutate(item = as.factor(item),
         modality_valence = as.factor(modality_valence),
         modality = as.factor(modality),
         performance = as.factor(performance)) |>
  left_join(help_harm, "impact", by = c("id", "modality", "modality_valence")) |>
  select(-c(item.x, item.y)) |>
  unite("label", c(modality, modality_valence), sep = " ", remove = FALSE)

# wilcoxon signed ranks test to extract Z stat

library(rstatix)

zstats <- bind_rows(

performancetest_anger_minus <- wilcox_effsize(response ~ performance_code,
                                             conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                             data = subset(performance, modality == "Anger" & modality_valence == "Negative")),

performancetest_anger_plus <- wilcox_effsize(response ~ performance_code,
                                             conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                         data = subset(performance, modality == "Anger" & modality_valence == "Positive")),

performancetest_anxiety_minus <- wilcox_effsize(response ~ performance_code,
                                                conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                            data = subset(performance, modality == "Anxiety" & modality_valence == "Negative")),

performancetest_bodily_minus <- wilcox_effsize(response ~ performance_code,         
                                               conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                           data = subset(performance, modality == "Bodily-somatic" & modality_valence == "Negative")),

performancetest_bodily_plus <- wilcox_effsize(response ~ performance_code,            
                                              conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                          data = subset(performance, modality == "Bodily-somatic" & modality_valence == "Positive")),

performancetest_cognitive_minus <- wilcox_effsize(response ~ performance_code,       
                                                  conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                              data = subset(performance, modality == "Cognitive" & modality_valence == "Negative")),

performancetest_cognitive_plus <- wilcox_effsize(response ~ performance_code,        
                                                 conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                             data = subset(performance, modality == "Cognitive" & modality_valence == "Positive")),

performancetest_motor_minus <- wilcox_effsize(response ~ performance_code,           
                                              conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                          data = subset(performance, modality == "Motor-behavioural" & modality_valence == "Negative")),

performancetest_motor_plus <- wilcox_effsize(response ~ performance_code,           
                                             conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                         data = subset(performance, modality == "Motor-behavioural" & modality_valence == "Positive")),

performancetest_operational_minus <- wilcox_effsize(response ~ performance_code,     
                                                    conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                                data = subset(performance, modality == "Operational" & modality_valence == "Negative")),

performancetest_operational_plus <- wilcox_effsize(response ~ performance_code,      
                                                   conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                               data = subset(performance, modality == "Operational" & modality_valence == "Positive")),

performancetest_pleasant_plus <- wilcox_effsize(response ~ performance_code,        
                                                conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                            data = subset(performance, modality == "Pleasant states" & modality_valence == "Positive")),

performancetest_volitional_minus <- wilcox_effsize(response ~ performance_code,     
                                                   conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                               data = subset(performance, modality == "Volitional" & modality_valence == "Negative")),

performancetest_volitional_plus <- wilcox_effsize(response ~ performance_code,       
                                                  conf.level = 0.95, ci.type = "basic", ci = TRUE,
                                              data = subset(performance, modality == "Volitional" & modality_valence == "Positive"))

)

zstats <- data.frame(zstats,
                      modality = c("Anger",
                                   "Anger",
                                   "Anxiety",
                                   "Bodily-somatic",
                                   "Bodily-somatic",
                                   "Cognitive",
                                   "Cognitive",
                                   "Motor-behavioural",
                                   "Motor-behavioural",
                                   "Operational",
                                   "Operational",
                                   "Pleasant states",
                                   "Volitional",
                                   "Volitional"
                      ),
                      modality_valence = c("Negative",
                                           "Positive",
                                           "Negative",
                                           "Negative",
                                           "Positive",
                                           "Negative",
                                           "Positive",
                                           "Negative",
                                           "Positive",
                                           "Negative",
                                           "Positive",
                                           "Positive",
                                           "Negative",
                                           "Positive"))

study_1_plot <- performance %>%
  ggplot(aes(y=response, x = performance)) +
  stat_slab(data = subset(performance, performance == "WEP"),
            density = "histogram",
            breaks = breaks_fixed(width = 0.5),
            align = align_center(),
            position = position_nudge(x = -0.1),
            side = "left") +
  stat_slab(data = subset(performance, performance == "BEP"),
            density = "histogram",
            breaks = breaks_fixed(width = 0.5),
            align = align_center(),
            position = position_nudge(x = 0.1),
            side = "right") +
  geom_line(aes(y=response, x = performance, group = Q2), size = 0.25, position = position_jitter(h=0.05, w=0.05), alpha = 0.25) +
  geom_text(data = zstats, aes(x = 1.5, y = 5.5, label = glue::glue("r = {round(effsize, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, modality_valence)), size = 2.5) +
  scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
  facet_grid(cols = vars(modality_valence), rows = vars(modality)) +
  labs(x = "Performance", y = "Response (Intensity)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))

ggsave("study_1_plot.tiff", plot = study_1_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)


  performancetest_anger_minus <- cor.test(formula = ~ response + impact,
                                                conf.level = 0.95, 
                                                data = subset(all_dat_study_1, modality == "Anger" & modality_valence == "Negative")),
  
  performancetest_anger_plus <- cor.test(formula = ~ response + impact,
                                         conf.level = 0.95,
                                               data = subset(all_dat_study_1, modality == "Anger" & modality_valence == "Positive")),
  
  performancetest_anxiety_minus <- cor.test(formula = ~ response + impact,
                                            conf.level = 0.95,
                                                  data = subset(all_dat_study_1, modality == "Anxiety" & modality_valence == "Negative")),
  
  performancetest_bodily_minus <- cor.test(formula = ~ response + impact,
                                           conf.level = 0.95,
                                                 data = subset(all_dat_study_1, modality == "Bodily-somatic" & modality_valence == "Negative")),
  
  performancetest_bodily_plus <- cor.test(formula = ~ response + impact,
                                          conf.level = 0.95,
                                                data = subset(all_dat_study_1, modality == "Bodily-somatic" & modality_valence == "Positive")),
  
  performancetest_cognitive_minus <- cor.test(formula = ~ response + impact,
                                              conf.level = 0.95,
                                                    data = subset(all_dat_study_1, modality == "Cognitive" & modality_valence == "Negative")),
  
  performancetest_cognitive_plus <- cor.test(formula = ~ response + impact,
                                             conf.level = 0.95,
                                                   data = subset(all_dat_study_1, modality == "Cognitive" & modality_valence == "Positive")),
  
  performancetest_motor_minus <- cor.test(formula = ~ response + impact,
                                          conf.level = 0.95,
                                                data = subset(all_dat_study_1, modality == "Motor-behavioural" & modality_valence == "Negative")),
  
  performancetest_motor_plus <- cor.test(formula = ~ response + impact,
                                         conf.level = 0.95,
                                               data = subset(all_dat_study_1, modality == "Motor-behavioural" & modality_valence == "Positive")),
  
  performancetest_operational_minus <- cor.test(formula = ~ response + impact,
                                                conf.level = 0.95,
                                                      data = subset(all_dat_study_1, modality == "Operational" & modality_valence == "Negative")),
  
  performancetest_operational_plus <- cor.test(formula = ~ response + impact,
                                               conf.level = 0.95,
                                                     data = subset(all_dat_study_1, modality == "Operational" & modality_valence == "Positive")),
  
  performancetest_pleasant_plus <- cor.test(formula = ~ response + impact,
                                            conf.level = 0.95,
                                                  data = subset(all_dat_study_1, modality == "Pleasant states" & modality_valence == "Positive")),
  
  performancetest_volitional_minus <- cor.test(formula = ~ response + impact,
                                               conf.level = 0.95,
                                                     data = subset(all_dat_study_1, modality == "Volitional" & modality_valence == "Negative")),
  
  performancetest_volitional_plus <- cor.test(formula = ~ response + impact,
                                              conf.level = 0.95,
                                                    data = subset(all_dat_study_1, modality == "Volitional" & modality_valence == "Positive"))
  

  r_stats <- tibble(
    R = c(
      performancetest_anger_minus$estimate,
      performancetest_anger_plus$estimate,
      performancetest_anxiety_minus$estimate,
      performancetest_bodily_minus$estimate,
      performancetest_bodily_plus$estimate,
      performancetest_cognitive_minus$estimate,
      performancetest_cognitive_plus$estimate,
      performancetest_motor_minus$estimate,
      performancetest_motor_plus$estimate,
      performancetest_operational_minus$estimate,
      performancetest_operational_plus$estimate,
      performancetest_pleasant_plus$estimate,
      performancetest_volitional_minus$estimate,
      performancetest_volitional_plus$estimate
    ),
    conf.low = c(
      performancetest_anger_minus$conf.int[1],
      performancetest_anger_plus$conf.int[1],
      performancetest_anxiety_minus$conf.int[1],
      performancetest_bodily_minus$conf.int[1],
      performancetest_bodily_plus$conf.int[1],
      performancetest_cognitive_minus$conf.int[1],
      performancetest_cognitive_plus$conf.int[1],
      performancetest_motor_minus$conf.int[1],
      performancetest_motor_plus$conf.int[1],
      performancetest_operational_minus$conf.int[1],
      performancetest_operational_plus$conf.int[1],
      performancetest_pleasant_plus$conf.int[1],
      performancetest_volitional_minus$conf.int[1],
      performancetest_volitional_plus$conf.int[1]
    ),
    conf.high = c(
      performancetest_anger_minus$conf.int[2],
      performancetest_anger_plus$conf.int[2],
      performancetest_anxiety_minus$conf.int[2],
      performancetest_bodily_minus$conf.int[2],
      performancetest_bodily_plus$conf.int[2],
      performancetest_cognitive_minus$conf.int[2],
      performancetest_cognitive_plus$conf.int[2],
      performancetest_motor_minus$conf.int[2],
      performancetest_motor_plus$conf.int[2],
      performancetest_operational_minus$conf.int[2],
      performancetest_operational_plus$conf.int[2],
      performancetest_pleasant_plus$conf.int[2],
      performancetest_volitional_minus$conf.int[2],
      performancetest_volitional_plus$conf.int[2]
    ),
    modality = c("Anger",
                 "Anger",
                 "Anxiety",
                 "Bodily-somatic",
                 "Bodily-somatic",
                 "Cognitive",
                 "Cognitive",
                 "Motor-behavioural",
                 "Motor-behavioural",
                 "Operational",
                 "Operational",
                 "Pleasant states",
                 "Volitional",
                 "Volitional"
    ),
    modality_valence = c("Negative",
                "Positive",
                "Negative",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Positive",
                "Negative",
                "Positive")
  )



study_1_PI_plot <-  all_dat_study_1 %>%
  ggplot(aes(y=response, x = impact-4)) +
  geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.5) + 
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_text(data = r_stats, aes(x = 0, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, modality_valence)), size = 2.5) +
  scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
  scale_x_continuous(limits = c(-3, 3), breaks = (-3:3),
                     labels = c("Very\nHarmful", "", "", "No Effect", "", "", "Very\nHelpful")) +
  facet_grid(cols = vars(modality_valence), rows = vars(modality)) +
  labs(x = "Perceived Impact on Performance", y = "Response (Intensity)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))

ggsave("study_1_PI_plot.tiff", plot = study_1_PI_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)




#### Study 2  ----

library(readxl)

sheets_2 <- "data/S2 Raw Data.xlsx" |>
  excel_sheets() |>
  set_names() 

dat_study_2 <- sheets_2[2:6] |> 
  map(~ read_excel(path = "data/S2 Raw Data.xlsx", sheet = .x)) |>
  list_rbind(names_to = "sheet") |>
  pivot_longer(3:30,
               names_to = "item",
               values_to = "response") |>
  mutate(
    modality = case_when(
      item == "P+I" ~ "Pleasant states",
      item == "A+I" ~ "Anger",
      item == "MB+I" ~  "Motor-behavioural",
      item == "Cog-I" ~ "Cognitive",
      item == "O+I" ~ "Operational",
      item == "BS+I" ~ "Bodily-somatic",
      item == "MB-I" ~ "Motor-behavioural",
      item == "Cog+I" ~ "Cognitive",
      item == "O-I" ~ "Operational",
      item == "V+I" ~ "Volitional",
      item == "Anx-I" ~ "Anxiety",
      item == "BS-I" ~ "Bodily-somatic",
      item == "Ang-I" ~ "Anger",
      item == "V-I" ~ "Volitional",
      item == "P+PI" ~ "Pleasant states",
      item == "A+PI" ~ "Anger",
      item == "MB+PI" ~  "Motor-behavioural",
      item == "Cog-PI" ~ "Cognitive",
      item == "O+PI" ~ "Operational",
      item == "BS+PI" ~ "Bodily-somatic",
      item == "MB-PI" ~ "Motor-behavioural",
      item == "Cog+PI" ~ "Cognitive",
      item == "O-PI" ~ "Operational",
      item == "V+PI" ~ "Volitional",
      item == "Anx-PI" ~ "Anxiety",
      item == "BS-PI" ~ "Bodily-somatic",
      item == "Ang-PI" ~ "Anger",
      item == "V-PI" ~ "Volitional"
    ),
    valence = case_when(
      str_detect(item, pattern = "-") ~ "Negative",
      .default = "Positive"
    ),
    intensity_perceivedimpact = case_when(
      str_detect(item, pattern = "PI") ~ "Perceived Impact",
      .default = "Intensity"
    )
  ) |>
  janitor::clean_names() |>
  
  # z - score the performances and shift to long form to model altogether
  
  mutate(
    imtp_z = (imtp_pvf_n - mean(imtp_pvf_n, na.rm = TRUE)) / sd(imtp_pvf_n, na.rm = TRUE),
    cmj_z = (cmj_im_cm - mean(cmj_im_cm, na.rm = TRUE)) / sd(cmj_im_cm, na.rm = TRUE),
    mmp_z = (x3min_mmp_w - mean(x3min_mmp_w, na.rm = TRUE)) / sd(x3min_mmp_w, na.rm = TRUE)
  ) |>
  
  pivot_longer(c(4,6,8,14:16),
               names_to = "performance",
               values_to = "value")


# RM correlations
library(rmcorr)


  performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                             filter(modality == "Anger" & valence == "Negative"))
  
  performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                       value, response,
                                       dataset = dat_study_2 |>
                                         filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                         filter(modality == "Anger" & valence == "Positive"))
  
  performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                          value, response,
                                          dataset = dat_study_2 |>
                                            filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                            filter(modality == "Anxiety" & valence == "Negative"))
  
  performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_study_2 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                           filter(modality == "Bodily-somatic" & valence == "Negative"))
  
  performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                        value, response,
                                        dataset = dat_study_2 |>
                                          filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                          filter(modality == "Bodily-somatic" & valence == "Positive"))
  
  performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                            value, response,
                                            dataset = dat_study_2 |>
                                              filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                              filter(modality == "Cognitive" & valence == "Negative"))
  
  performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                             filter(modality == "Cognitive" & valence == "Positive"))
  
  performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                        value, response,
                                        dataset = dat_study_2 |>
                                          filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                          filter(modality == "Motor-behavioural" & valence == "Negative"))
  
  performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                       value, response,
                                       dataset = dat_study_2 |>
                                         filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                         filter(modality == "Motor-behavioural" & valence == "Positive"))
  
  performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                              value, response,
                                              dataset = dat_study_2 |>
                                                filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                                filter(modality == "Operational" & valence == "Negative"))
  
  performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_study_2 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                               filter(modality == "Operational" & valence == "Positive"))
  
  performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                          value, response,
                                          dataset = dat_study_2 |>
                                            filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                            filter(modality == "Pleasant states" & valence == "Positive"))
  
  performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_study_2 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                               filter(modality == "Volitional" & valence == "Negative"))
  
  performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                            value, response,
                                            dataset = dat_study_2 |>
                                              filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |> 
                                              filter(modality == "Volitional" & valence == "Positive"))
  

  rmcorr_stats <- tibble(
    R = c(
      performance_rmcorr_anger_minus$r,
      performance_rmcorr_anger_plus$r,
      performance_rmcorr_anxiety_minus$r,
      performance_rmcorr_bodily_minus$r,
      performance_rmcorr_bodily_plus$r,
      performance_rmcorr_cognitive_minus$r,
      performance_rmcorr_cognitive_plus$r,
      performance_rmcorr_motor_minus$r,
      performance_rmcorr_motor_plus$r,
      performance_rmcorr_operational_minus$r,
      performance_rmcorr_operational_plus$r,
      performance_rmcorr_pleasant_plus$r,
      performance_rmcorr_volitional_minus$r,
      performance_rmcorr_volitional_plus$r
    ),
    conf.low = c(
      performance_rmcorr_anger_minus$CI[1],
      performance_rmcorr_anger_plus$CI[1],
      performance_rmcorr_anxiety_minus$CI[1],
      performance_rmcorr_bodily_minus$CI[1],
      performance_rmcorr_bodily_plus$CI[1],
      performance_rmcorr_cognitive_minus$CI[1],
      performance_rmcorr_cognitive_plus$CI[1],
      performance_rmcorr_motor_minus$CI[1],
      performance_rmcorr_motor_plus$CI[1],
      performance_rmcorr_operational_minus$CI[1],
      performance_rmcorr_operational_plus$CI[1],
      performance_rmcorr_pleasant_plus$CI[1],
      performance_rmcorr_volitional_minus$CI[1],
      performance_rmcorr_volitional_plus$CI[1]
    ),
    conf.high = c(
      performance_rmcorr_anger_minus$CI[2],
      performance_rmcorr_anger_plus$CI[2],
      performance_rmcorr_anxiety_minus$CI[2],
      performance_rmcorr_bodily_minus$CI[2],
      performance_rmcorr_bodily_plus$CI[2],
      performance_rmcorr_cognitive_minus$CI[2],
      performance_rmcorr_cognitive_plus$CI[2],
      performance_rmcorr_motor_minus$CI[2],
      performance_rmcorr_motor_plus$CI[2],
      performance_rmcorr_operational_minus$CI[2],
      performance_rmcorr_operational_plus$CI[2],
      performance_rmcorr_pleasant_plus$CI[2],
      performance_rmcorr_volitional_minus$CI[2],
      performance_rmcorr_volitional_plus$CI[2]
    ),
    modality = c("Anger",
                 "Anger",
                 "Anxiety",
                 "Bodily-somatic",
                 "Bodily-somatic",
                 "Cognitive",
                 "Cognitive",
                 "Motor-behavioural",
                 "Motor-behavioural",
                 "Operational",
                 "Operational",
                 "Pleasant states",
                 "Volitional",
                 "Volitional"
    ),
    valence = c("Negative",
                         "Positive",
                         "Negative",
                         "Negative",
                         "Positive",
                         "Negative",
                         "Positive",
                         "Negative",
                         "Positive",
                         "Negative",
                         "Positive",
                         "Positive",
                         "Negative",
                         "Positive")
  )

  
  rmcrorr_fits <- bind_rows(
    
    bind_cols(performance_rmcorr_anger_minus$model$model,
                     performance_rmcorr_anger_minus$model$fitted.values) |>
      mutate(
        modality = "Anger",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_anger_plus$model$model,
              performance_rmcorr_anger_plus$model$fitted.values) |>
      mutate(
        modality = "Anger",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_anxiety_minus$model$model,
              performance_rmcorr_anxiety_minus$model$fitted.values) |>
      mutate(
        modality = "Anxiety",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_bodily_minus$model$model,
              performance_rmcorr_bodily_minus$model$fitted.values) |>
      mutate(
        modality = "Bodily-somatic",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_bodily_plus$model$model,
              performance_rmcorr_bodily_plus$model$fitted.values) |>
      mutate(
        modality = "Bodily-somatic",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_cognitive_minus$model$model,
              performance_rmcorr_cognitive_minus$model$fitted.values) |>
      mutate(
        modality = "Cognitive",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_cognitive_plus$model$model,
              performance_rmcorr_cognitive_plus$model$fitted.values) |>
      mutate(
        modality = "Cognitive",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_motor_minus$model$model,
              performance_rmcorr_motor_minus$model$fitted.values) |>
      mutate(
        modality = "Motor-behavioural",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_motor_plus$model$model,
              performance_rmcorr_motor_plus$model$fitted.values) |>
      mutate(
        modality = "Motor-behavioural",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_operational_minus$model$model,
              performance_rmcorr_operational_minus$model$fitted.values) |>
      mutate(
        modality = "Operational",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_operational_plus$model$model,
              performance_rmcorr_operational_plus$model$fitted.values) |>
      mutate(
        modality = "Operational",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_pleasant_plus$model$model,
              performance_rmcorr_pleasant_plus$model$fitted.values) |>
      mutate(
        modality = "Pleasant states",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_volitional_minus$model$model,
              performance_rmcorr_volitional_minus$model$fitted.values) |>
      mutate(
        modality = "Volitional",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_volitional_plus$model$model,
              performance_rmcorr_volitional_plus$model$fitted.values) |>
      mutate(
        modality = "Volitional",
        valence = "Positive"
      )
    
  ) |>
    rename(fit = "...4",
           value = "Measure1",
           response = "Measure2")


study_2_objective_plot <-  dat_study_2 %>%
    filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "z")) |>
    ggplot(aes(y=response+1, x = value)) +
    geom_point(position = position_jitter(w=0, h=0.1), size=0.5, alpha = 0.5) + 
    geom_line(data = rmcrorr_fits,
              aes(y=fit+1, group = Participant),
              alpha = 0.75) +
    geom_text(data = rmcorr_stats, aes(x = -2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
    scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
    facet_grid(cols = vars(valence), rows = vars(modality)) +
    labs(x = "Performance (z-score)", y = "Response (Intensity)") +
    theme_bw() +
    theme(strip.text.y = element_text(size = 6),
          axis.text = element_text(size = 6))

ggsave("study_2_objective_plot.tiff", plot = study_2_objective_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)

  
  
  performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Anger" & valence == "Negative"))
  
  performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                          value, response,
                                          dataset = dat_study_2 |>
                                            filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                            filter(modality == "Anger" & valence == "Positive"))
  
  performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_study_2 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                               filter(modality == "Anxiety" & valence == "Negative"))
  
  performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                            value, response,
                                            dataset = dat_study_2 |>
                                              filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                              filter(modality == "Bodily-somatic" & valence == "Negative"))
  
  performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Bodily-somatic" & valence == "Positive"))
  
  performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                               value, response,
                                               dataset = dat_study_2 |>
                                                 filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                 filter(modality == "Cognitive" & valence == "Negative"))
  
  performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                              value, response,
                                              dataset = dat_study_2 |>
                                                filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                filter(modality == "Cognitive" & valence == "Positive"))
  
  performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Motor-behavioural" & valence == "Negative"))
  
  performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                          value, response,
                                          dataset = dat_study_2 |>
                                            filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                            filter(modality == "Motor-behavioural" & valence == "Positive"))
  
  performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                                 value, response,
                                                 dataset = dat_study_2 |>
                                                   filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                   filter(modality == "Operational" & valence == "Negative"))
  
  performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                                value, response,
                                                dataset = dat_study_2 |>
                                                  filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                  filter(modality == "Operational" & valence == "Positive"))
  
  performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_study_2 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                               filter(modality == "Pleasant states" & valence == "Positive"))
  
  performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                                value, response,
                                                dataset = dat_study_2 |>
                                                  filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                  filter(modality == "Volitional" & valence == "Negative"))
  
  performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                               value, response,
                                               dataset = dat_study_2 |>
                                                 filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                 filter(modality == "Volitional" & valence == "Positive"))
  
  
  rmcorr_stats <- tibble(
    R = c(
      performance_rmcorr_anger_minus$r,
      performance_rmcorr_anger_plus$r,
      performance_rmcorr_anxiety_minus$r,
      performance_rmcorr_bodily_minus$r,
      performance_rmcorr_bodily_plus$r,
      performance_rmcorr_cognitive_minus$r,
      performance_rmcorr_cognitive_plus$r,
      performance_rmcorr_motor_minus$r,
      performance_rmcorr_motor_plus$r,
      performance_rmcorr_operational_minus$r,
      performance_rmcorr_operational_plus$r,
      performance_rmcorr_pleasant_plus$r,
      performance_rmcorr_volitional_minus$r,
      performance_rmcorr_volitional_plus$r
    ),
    conf.low = c(
      performance_rmcorr_anger_minus$CI[1],
      performance_rmcorr_anger_plus$CI[1],
      performance_rmcorr_anxiety_minus$CI[1],
      performance_rmcorr_bodily_minus$CI[1],
      performance_rmcorr_bodily_plus$CI[1],
      performance_rmcorr_cognitive_minus$CI[1],
      performance_rmcorr_cognitive_plus$CI[1],
      performance_rmcorr_motor_minus$CI[1],
      performance_rmcorr_motor_plus$CI[1],
      performance_rmcorr_operational_minus$CI[1],
      performance_rmcorr_operational_plus$CI[1],
      performance_rmcorr_pleasant_plus$CI[1],
      performance_rmcorr_volitional_minus$CI[1],
      performance_rmcorr_volitional_plus$CI[1]
    ),
    conf.high = c(
      performance_rmcorr_anger_minus$CI[2],
      performance_rmcorr_anger_plus$CI[2],
      performance_rmcorr_anxiety_minus$CI[2],
      performance_rmcorr_bodily_minus$CI[2],
      performance_rmcorr_bodily_plus$CI[2],
      performance_rmcorr_cognitive_minus$CI[2],
      performance_rmcorr_cognitive_plus$CI[2],
      performance_rmcorr_motor_minus$CI[2],
      performance_rmcorr_motor_plus$CI[2],
      performance_rmcorr_operational_minus$CI[2],
      performance_rmcorr_operational_plus$CI[2],
      performance_rmcorr_pleasant_plus$CI[2],
      performance_rmcorr_volitional_minus$CI[2],
      performance_rmcorr_volitional_plus$CI[2]
    ),
    modality = c("Anger",
                 "Anger",
                 "Anxiety",
                 "Bodily-somatic",
                 "Bodily-somatic",
                 "Cognitive",
                 "Cognitive",
                 "Motor-behavioural",
                 "Motor-behavioural",
                 "Operational",
                 "Operational",
                 "Pleasant states",
                 "Volitional",
                 "Volitional"
    ),
    valence = c("Negative",
                "Positive",
                "Negative",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Positive",
                "Negative",
                "Positive")
  )
  
  
  rmcrorr_fits <- bind_rows(
    
    bind_cols(performance_rmcorr_anger_minus$model$model,
              performance_rmcorr_anger_minus$model$fitted.values) |>
      mutate(
        modality = "Anger",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_anger_plus$model$model,
              performance_rmcorr_anger_plus$model$fitted.values) |>
      mutate(
        modality = "Anger",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_anxiety_minus$model$model,
              performance_rmcorr_anxiety_minus$model$fitted.values) |>
      mutate(
        modality = "Anxiety",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_bodily_minus$model$model,
              performance_rmcorr_bodily_minus$model$fitted.values) |>
      mutate(
        modality = "Bodily-somatic",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_bodily_plus$model$model,
              performance_rmcorr_bodily_plus$model$fitted.values) |>
      mutate(
        modality = "Bodily-somatic",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_cognitive_minus$model$model,
              performance_rmcorr_cognitive_minus$model$fitted.values) |>
      mutate(
        modality = "Cognitive",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_cognitive_plus$model$model,
              performance_rmcorr_cognitive_plus$model$fitted.values) |>
      mutate(
        modality = "Cognitive",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_motor_minus$model$model,
              performance_rmcorr_motor_minus$model$fitted.values) |>
      mutate(
        modality = "Motor-behavioural",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_motor_plus$model$model,
              performance_rmcorr_motor_plus$model$fitted.values) |>
      mutate(
        modality = "Motor-behavioural",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_operational_minus$model$model,
              performance_rmcorr_operational_minus$model$fitted.values) |>
      mutate(
        modality = "Operational",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_operational_plus$model$model,
              performance_rmcorr_operational_plus$model$fitted.values) |>
      mutate(
        modality = "Operational",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_pleasant_plus$model$model,
              performance_rmcorr_pleasant_plus$model$fitted.values) |>
      mutate(
        modality = "Pleasant states",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_volitional_minus$model$model,
              performance_rmcorr_volitional_minus$model$fitted.values) |>
      mutate(
        modality = "Volitional",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_volitional_plus$model$model,
              performance_rmcorr_volitional_plus$model$fitted.values) |>
      mutate(
        modality = "Volitional",
        valence = "Positive"
      )
    
  ) |>
    rename(fit = "...4",
           value = "Measure1",
           response = "Measure2")
  
  
  study_2_subjective_plot <-  dat_study_2 %>%
    filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |>
    ggplot(aes(y=response+1, x = value)) +
    geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.5) + 
    geom_line(data = rmcrorr_fits,
              aes(y=fit+1, group = Participant),
              alpha = 0.75) +
    geom_text(data = rmcorr_stats, aes(x = 2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
    scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
    scale_x_continuous(limits = c(0,11), breaks = c(1,2,3,4,5)) +
    facet_grid(cols = vars(valence), rows = vars(modality)) +
    labs(x = "Performance (self-rated)", y = "Response (Intensity)") +
    theme_bw() +
    theme(strip.text.y = element_text(size = 6),
          axis.text = element_text(size = 6))

  ggsave("study_2_subjective_plot.tiff", plot = study_2_subjective_plot,
         w = 7.5, h = 10, device = "tiff", dpi = 300)
  
  
  
  dat_study_2_wide <- dat_study_2 |>
    group_by(sheet, code, modality, valence, intensity_perceivedimpact) |>
    slice_head(n=1) |>
    select(sheet, code, modality, valence, intensity_perceivedimpact, response) |>
    pivot_wider(names_from = intensity_perceivedimpact,
                values_from = response,
                id_cols = c(code, modality, valence, sheet))
  
  
  performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_study_2_wide |> 
                                             filter(modality == "Anger" & valence == "Negative"))
  
  performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                          `Perceived Impact`, Intensity,
                                          dataset = dat_study_2_wide |> 
                                            filter(modality == "Anger" & valence == "Positive"))
  
  performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                             `Perceived Impact`, Intensity,
                                             dataset = dat_study_2_wide |> 
                                               filter(modality == "Anxiety" & valence == "Negative"))
  
  performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                            `Perceived Impact`, Intensity,
                                            dataset = dat_study_2_wide |>
                                              filter(modality == "Bodily-somatic" & valence == "Negative"))
  
  performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_study_2_wide |> 
                                             filter(modality == "Bodily-somatic" & valence == "Positive"))
  
  performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                               `Perceived Impact`, Intensity,
                                               dataset = dat_study_2_wide |> 
                                                 filter(modality == "Cognitive" & valence == "Negative"))
  
  performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                              `Perceived Impact`, Intensity,
                                              dataset = dat_study_2_wide |> 
                                                filter(modality == "Cognitive" & valence == "Positive"))
  
  performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_study_2_wide |> 
                                             filter(modality == "Motor-behavioural" & valence == "Negative"))
  
  performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                          `Perceived Impact`, Intensity,
                                          dataset = dat_study_2_wide |> 
                                            filter(modality == "Motor-behavioural" & valence == "Positive"))
  
  performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                                 `Perceived Impact`, Intensity,
                                                 dataset = dat_study_2_wide |>
                                                   filter(modality == "Operational" & valence == "Negative"))
  
  performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                                `Perceived Impact`, Intensity,
                                                dataset = dat_study_2_wide |> 
                                                  filter(modality == "Operational" & valence == "Positive"))
  
  performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                             `Perceived Impact`, Intensity,
                                             dataset = dat_study_2_wide |> 
                                               filter(modality == "Pleasant states" & valence == "Positive"))
  
  performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                                `Perceived Impact`, Intensity,
                                                dataset = dat_study_2_wide |>
                                                  filter(modality == "Volitional" & valence == "Negative"))
  
  performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                               `Perceived Impact`, Intensity,
                                               dataset = dat_study_2_wide |>
                                                 filter(modality == "Volitional" & valence == "Positive"))
  
  
  rmcorr_stats <- tibble(
    R = c(
      performance_rmcorr_anger_minus$r,
      performance_rmcorr_anger_plus$r,
      performance_rmcorr_anxiety_minus$r,
      performance_rmcorr_bodily_minus$r,
      performance_rmcorr_bodily_plus$r,
      performance_rmcorr_cognitive_minus$r,
      performance_rmcorr_cognitive_plus$r,
      performance_rmcorr_motor_minus$r,
      performance_rmcorr_motor_plus$r,
      performance_rmcorr_operational_minus$r,
      performance_rmcorr_operational_plus$r,
      performance_rmcorr_pleasant_plus$r,
      performance_rmcorr_volitional_minus$r,
      performance_rmcorr_volitional_plus$r
    ),
    conf.low = c(
      performance_rmcorr_anger_minus$CI[1],
      performance_rmcorr_anger_plus$CI[1],
      performance_rmcorr_anxiety_minus$CI[1],
      performance_rmcorr_bodily_minus$CI[1],
      performance_rmcorr_bodily_plus$CI[1],
      performance_rmcorr_cognitive_minus$CI[1],
      performance_rmcorr_cognitive_plus$CI[1],
      performance_rmcorr_motor_minus$CI[1],
      performance_rmcorr_motor_plus$CI[1],
      performance_rmcorr_operational_minus$CI[1],
      performance_rmcorr_operational_plus$CI[1],
      performance_rmcorr_pleasant_plus$CI[1],
      performance_rmcorr_volitional_minus$CI[1],
      performance_rmcorr_volitional_plus$CI[1]
    ),
    conf.high = c(
      performance_rmcorr_anger_minus$CI[2],
      performance_rmcorr_anger_plus$CI[2],
      performance_rmcorr_anxiety_minus$CI[2],
      performance_rmcorr_bodily_minus$CI[2],
      performance_rmcorr_bodily_plus$CI[2],
      performance_rmcorr_cognitive_minus$CI[2],
      performance_rmcorr_cognitive_plus$CI[2],
      performance_rmcorr_motor_minus$CI[2],
      performance_rmcorr_motor_plus$CI[2],
      performance_rmcorr_operational_minus$CI[2],
      performance_rmcorr_operational_plus$CI[2],
      performance_rmcorr_pleasant_plus$CI[2],
      performance_rmcorr_volitional_minus$CI[2],
      performance_rmcorr_volitional_plus$CI[2]
    ),
    modality = c("Anger",
                 "Anger",
                 "Anxiety",
                 "Bodily-somatic",
                 "Bodily-somatic",
                 "Cognitive",
                 "Cognitive",
                 "Motor-behavioural",
                 "Motor-behavioural",
                 "Operational",
                 "Operational",
                 "Pleasant states",
                 "Volitional",
                 "Volitional"
    ),
    valence = c("Negative",
                "Positive",
                "Negative",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Negative",
                "Positive",
                "Positive",
                "Negative",
                "Positive")
  )
  
  
  rmcrorr_fits <- bind_rows(
    
    bind_cols(performance_rmcorr_anger_minus$model$model,
              performance_rmcorr_anger_minus$model$fitted.values) |>
      mutate(
        modality = "Anger",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_anger_plus$model$model,
              performance_rmcorr_anger_plus$model$fitted.values) |>
      mutate(
        modality = "Anger",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_anxiety_minus$model$model,
              performance_rmcorr_anxiety_minus$model$fitted.values) |>
      mutate(
        modality = "Anxiety",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_bodily_minus$model$model,
              performance_rmcorr_bodily_minus$model$fitted.values) |>
      mutate(
        modality = "Bodily-somatic",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_bodily_plus$model$model,
              performance_rmcorr_bodily_plus$model$fitted.values) |>
      mutate(
        modality = "Bodily-somatic",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_cognitive_minus$model$model,
              performance_rmcorr_cognitive_minus$model$fitted.values) |>
      mutate(
        modality = "Cognitive",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_cognitive_plus$model$model,
              performance_rmcorr_cognitive_plus$model$fitted.values) |>
      mutate(
        modality = "Cognitive",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_motor_minus$model$model,
              performance_rmcorr_motor_minus$model$fitted.values) |>
      mutate(
        modality = "Motor-behavioural",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_motor_plus$model$model,
              performance_rmcorr_motor_plus$model$fitted.values) |>
      mutate(
        modality = "Motor-behavioural",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_operational_minus$model$model,
              performance_rmcorr_operational_minus$model$fitted.values) |>
      mutate(
        modality = "Operational",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_operational_plus$model$model,
              performance_rmcorr_operational_plus$model$fitted.values) |>
      mutate(
        modality = "Operational",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_pleasant_plus$model$model,
              performance_rmcorr_pleasant_plus$model$fitted.values) |>
      mutate(
        modality = "Pleasant states",
        valence = "Positive"
      ),
    bind_cols(performance_rmcorr_volitional_minus$model$model,
              performance_rmcorr_volitional_minus$model$fitted.values) |>
      mutate(
        modality = "Volitional",
        valence = "Negative"
      ),
    bind_cols(performance_rmcorr_volitional_plus$model$model,
              performance_rmcorr_volitional_plus$model$fitted.values) |>
      mutate(
        modality = "Volitional",
        valence = "Positive"
      )
    
  ) |>
    rename(fit = "...4",
           `Perceived Impact` = "Measure1",
           Intensity = "Measure2")
  
  
  study_2_PI_plot <-  dat_study_2_wide %>%
    ggplot(aes(y=Intensity+1, x = `Perceived Impact`)) +
    geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.5) + 
    geom_line(data = rmcrorr_fits,
              aes(y=fit+1, group = Participant),
              alpha = 0.75) +
    geom_text(data = rmcorr_stats, aes(x = -2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
    scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
    scale_x_continuous(limits = c(-3, 3), breaks = (-3:3),
                       labels = c("Very\nHarmful", "", "", "No Effect", "", "", "Very\nHelpful")) +facet_grid(cols = vars(valence), rows = vars(modality)) +
    labs(x = "Perceived Impact on Performance", y = "Response (Intensity)") +
    theme_bw() +
    theme(strip.text.y = element_text(size = 6),
          axis.text = element_text(size = 6))
  
  ggsave("study_2_PI_plot.tiff", plot = study_2_PI_plot,
         w = 7.5, h = 10, device = "tiff", dpi = 300)

#### Study 3 ----

library(readxl)

sheets_3 <- "data/S3 Raw Data.xlsx" |>
  excel_sheets() |>
  set_names() 

dat_study_3 <- sheets_3[2:7] |> 
  map(~ read_excel(path = "data/S3 Raw Data.xlsx", sheet = .x)) |>
  list_rbind(names_to = "sheet") |>
  pivot_longer(3:30,
               names_to = "item",
               values_to = "response") |>
  mutate(
    modality = case_when(
      item == "P+I" ~ "Pleasant states",
      item == "A+I" ~ "Anger",
      item == "MB+I" ~  "Motor-behavioural",
      item == "Cog-I" ~ "Cognitive",
      item == "O+I" ~ "Operational",
      item == "BS+I" ~ "Bodily-somatic",
      item == "MB-I" ~ "Motor-behavioural",
      item == "Cog+I" ~ "Cognitive",
      item == "O-I" ~ "Operational",
      item == "V+I" ~ "Volitional",
      item == "Anx-I" ~ "Anxiety",
      item == "BS-I" ~ "Bodily-somatic",
      item == "Ang-I" ~ "Anger",
      item == "V-I" ~ "Volitional",
      item == "P+PI" ~ "Pleasant states",
      item == "A+PI" ~ "Anger",
      item == "MB+PI" ~  "Motor-behavioural",
      item == "Cog-PI" ~ "Cognitive",
      item == "O+PI" ~ "Operational",
      item == "BS+PI" ~ "Bodily-somatic",
      item == "MB-PI" ~ "Motor-behavioural",
      item == "Cog+PI" ~ "Cognitive",
      item == "O-PI" ~ "Operational",
      item == "V+PI" ~ "Volitional",
      item == "Anx-PI" ~ "Anxiety",
      item == "BS-PI" ~ "Bodily-somatic",
      item == "Ang-PI" ~ "Anger",
      item == "V-PI" ~ "Volitional"
    ),
    valence = case_when(
      str_detect(item, pattern = "-") ~ "Negative",
      .default = "Positive"
    ),
    intensity_perceivedimpact = case_when(
      str_detect(item, pattern = "PI") ~ "Perceived Impact",
      .default = "Intensity"
    )
  ) |>
  janitor::clean_names() |>
  
  # shift to long form to model altogether

  pivot_longer(c(3:5),
               names_to = "performance",
               values_to = "value")





performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_study_3 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                           filter(modality == "Anger" & valence == "Negative"))

performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                        value, response,
                                        dataset = dat_study_3 |>
                                          filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                          filter(modality == "Anger" & valence == "Positive"))

performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_3 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Anxiety" & valence == "Negative"))

performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                          value, response,
                                          dataset = dat_study_3 |>
                                            filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                            filter(modality == "Bodily-somatic" & valence == "Negative"))

performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_study_3 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                           filter(modality == "Bodily-somatic" & valence == "Positive"))

performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_study_3 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                               filter(modality == "Cognitive" & valence == "Negative"))

performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                            value, response,
                                            dataset = dat_study_3 |>
                                              filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                              filter(modality == "Cognitive" & valence == "Positive"))

performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_study_3 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                           filter(modality == "Motor-behavioural" & valence == "Negative"))

performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                        value, response,
                                        dataset = dat_study_3 |>
                                          filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                          filter(modality == "Motor-behavioural" & valence == "Positive"))

performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                               value, response,
                                               dataset = dat_study_3 |>
                                                 filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                 filter(modality == "Operational" & valence == "Negative"))

performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                              value, response,
                                              dataset = dat_study_3 |>
                                                filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                filter(modality == "Operational" & valence == "Positive"))

performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_study_3 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Pleasant states" & valence == "Positive"))

performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                              value, response,
                                              dataset = dat_study_3 |>
                                                filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                filter(modality == "Volitional" & valence == "Negative"))

performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_study_3 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                               filter(modality == "Volitional" & valence == "Positive"))


rmcorr_stats <- tibble(
  R = c(
    performance_rmcorr_anger_minus$r,
    performance_rmcorr_anger_plus$r,
    performance_rmcorr_anxiety_minus$r,
    performance_rmcorr_bodily_minus$r,
    performance_rmcorr_bodily_plus$r,
    performance_rmcorr_cognitive_minus$r,
    performance_rmcorr_cognitive_plus$r,
    performance_rmcorr_motor_minus$r,
    performance_rmcorr_motor_plus$r,
    performance_rmcorr_operational_minus$r,
    performance_rmcorr_operational_plus$r,
    performance_rmcorr_pleasant_plus$r,
    performance_rmcorr_volitional_minus$r,
    performance_rmcorr_volitional_plus$r
  ),
  conf.low = c(
    performance_rmcorr_anger_minus$CI[1],
    performance_rmcorr_anger_plus$CI[1],
    performance_rmcorr_anxiety_minus$CI[1],
    performance_rmcorr_bodily_minus$CI[1],
    performance_rmcorr_bodily_plus$CI[1],
    performance_rmcorr_cognitive_minus$CI[1],
    performance_rmcorr_cognitive_plus$CI[1],
    performance_rmcorr_motor_minus$CI[1],
    performance_rmcorr_motor_plus$CI[1],
    performance_rmcorr_operational_minus$CI[1],
    performance_rmcorr_operational_plus$CI[1],
    performance_rmcorr_pleasant_plus$CI[1],
    performance_rmcorr_volitional_minus$CI[1],
    performance_rmcorr_volitional_plus$CI[1]
  ),
  conf.high = c(
    performance_rmcorr_anger_minus$CI[2],
    performance_rmcorr_anger_plus$CI[2],
    performance_rmcorr_anxiety_minus$CI[2],
    performance_rmcorr_bodily_minus$CI[2],
    performance_rmcorr_bodily_plus$CI[2],
    performance_rmcorr_cognitive_minus$CI[2],
    performance_rmcorr_cognitive_plus$CI[2],
    performance_rmcorr_motor_minus$CI[2],
    performance_rmcorr_motor_plus$CI[2],
    performance_rmcorr_operational_minus$CI[2],
    performance_rmcorr_operational_plus$CI[2],
    performance_rmcorr_pleasant_plus$CI[2],
    performance_rmcorr_volitional_minus$CI[2],
    performance_rmcorr_volitional_plus$CI[2]
  ),
  modality = c("Anger",
               "Anger",
               "Anxiety",
               "Bodily-somatic",
               "Bodily-somatic",
               "Cognitive",
               "Cognitive",
               "Motor-behavioural",
               "Motor-behavioural",
               "Operational",
               "Operational",
               "Pleasant states",
               "Volitional",
               "Volitional"
  ),
  valence = c("Negative",
              "Positive",
              "Negative",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Positive",
              "Negative",
              "Positive")
)


rmcrorr_fits <- bind_rows(
  
  bind_cols(performance_rmcorr_anger_minus$model$model,
            performance_rmcorr_anger_minus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_anger_plus$model$model,
            performance_rmcorr_anger_plus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_anxiety_minus$model$model,
            performance_rmcorr_anxiety_minus$model$fitted.values) |>
    mutate(
      modality = "Anxiety",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_minus$model$model,
            performance_rmcorr_bodily_minus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_plus$model$model,
            performance_rmcorr_bodily_plus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_cognitive_minus$model$model,
            performance_rmcorr_cognitive_minus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_cognitive_plus$model$model,
            performance_rmcorr_cognitive_plus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_motor_minus$model$model,
            performance_rmcorr_motor_minus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_motor_plus$model$model,
            performance_rmcorr_motor_plus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_operational_minus$model$model,
            performance_rmcorr_operational_minus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_operational_plus$model$model,
            performance_rmcorr_operational_plus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_pleasant_plus$model$model,
            performance_rmcorr_pleasant_plus$model$fitted.values) |>
    mutate(
      modality = "Pleasant states",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_volitional_minus$model$model,
            performance_rmcorr_volitional_minus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_volitional_plus$model$model,
            performance_rmcorr_volitional_plus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Positive"
    )
  
) |>
  rename(fit = "...4",
         value = "Measure1",
         response = "Measure2")


study_3_subjective_plot <- dat_study_3 %>%
  filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |>
  ggplot(aes(y=response+1, x = value)) +
  geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.5) + 
  geom_line(data = rmcrorr_fits,
            aes(y=fit+1, group = Participant),
            alpha = 0.75) +
  geom_text(data = rmcorr_stats, aes(x = 2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
  scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
  scale_x_continuous(limits = c(1,11), breaks = c(1:11)) +
  facet_grid(cols = vars(valence), rows = vars(modality)) +
  labs(x = "Performance (self-rated)", y = "Response (Intensity)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))

ggsave("study_3_subjective_plot.tiff", plot = study_3_subjective_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)


dat_study_3_wide <- dat_study_3 |>
  group_by(sheet, code, modality, valence, intensity_perceivedimpact) |>
  slice_head(n=1) |>
  select(sheet, code, modality, valence, intensity_perceivedimpact, response) |>
  pivot_wider(names_from = intensity_perceivedimpact,
              values_from = response,
              id_cols = c(code, modality, valence, sheet))


performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                         `Perceived Impact`, Intensity,
                                         dataset = dat_study_3_wide |> 
                                           filter(modality == "Anger" & valence == "Negative"))

performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                        `Perceived Impact`, Intensity,
                                        dataset = dat_study_3_wide |> 
                                          filter(modality == "Anger" & valence == "Positive"))

performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_study_3_wide |> 
                                             filter(modality == "Anxiety" & valence == "Negative"))

performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                          `Perceived Impact`, Intensity,
                                          dataset = dat_study_3_wide |>
                                            filter(modality == "Bodily-somatic" & valence == "Negative"))

performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                         `Perceived Impact`, Intensity,
                                         dataset = dat_study_3_wide |> 
                                           filter(modality == "Bodily-somatic" & valence == "Positive"))

performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                             `Perceived Impact`, Intensity,
                                             dataset = dat_study_3_wide |> 
                                               filter(modality == "Cognitive" & valence == "Negative"))

performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                            `Perceived Impact`, Intensity,
                                            dataset = dat_study_3_wide |> 
                                              filter(modality == "Cognitive" & valence == "Positive"))

performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                         `Perceived Impact`, Intensity,
                                         dataset = dat_study_3_wide |> 
                                           filter(modality == "Motor-behavioural" & valence == "Negative"))

performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                        `Perceived Impact`, Intensity,
                                        dataset = dat_study_3_wide |> 
                                          filter(modality == "Motor-behavioural" & valence == "Positive"))

performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                               `Perceived Impact`, Intensity,
                                               dataset = dat_study_3_wide |>
                                                 filter(modality == "Operational" & valence == "Negative"))

performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                              `Perceived Impact`, Intensity,
                                              dataset = dat_study_3_wide |> 
                                                filter(modality == "Operational" & valence == "Positive"))

performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_study_3_wide |> 
                                             filter(modality == "Pleasant states" & valence == "Positive"))

performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                              `Perceived Impact`, Intensity,
                                              dataset = dat_study_3_wide |>
                                                filter(modality == "Volitional" & valence == "Negative"))

performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                             `Perceived Impact`, Intensity,
                                             dataset = dat_study_3_wide |>
                                               filter(modality == "Volitional" & valence == "Positive"))


rmcorr_stats <- tibble(
  R = c(
    performance_rmcorr_anger_minus$r,
    performance_rmcorr_anger_plus$r,
    performance_rmcorr_anxiety_minus$r,
    performance_rmcorr_bodily_minus$r,
    performance_rmcorr_bodily_plus$r,
    performance_rmcorr_cognitive_minus$r,
    performance_rmcorr_cognitive_plus$r,
    performance_rmcorr_motor_minus$r,
    performance_rmcorr_motor_plus$r,
    performance_rmcorr_operational_minus$r,
    performance_rmcorr_operational_plus$r,
    performance_rmcorr_pleasant_plus$r,
    performance_rmcorr_volitional_minus$r,
    performance_rmcorr_volitional_plus$r
  ),
  conf.low = c(
    performance_rmcorr_anger_minus$CI[1],
    performance_rmcorr_anger_plus$CI[1],
    performance_rmcorr_anxiety_minus$CI[1],
    performance_rmcorr_bodily_minus$CI[1],
    performance_rmcorr_bodily_plus$CI[1],
    performance_rmcorr_cognitive_minus$CI[1],
    performance_rmcorr_cognitive_plus$CI[1],
    performance_rmcorr_motor_minus$CI[1],
    performance_rmcorr_motor_plus$CI[1],
    performance_rmcorr_operational_minus$CI[1],
    performance_rmcorr_operational_plus$CI[1],
    performance_rmcorr_pleasant_plus$CI[1],
    performance_rmcorr_volitional_minus$CI[1],
    performance_rmcorr_volitional_plus$CI[1]
  ),
  conf.high = c(
    performance_rmcorr_anger_minus$CI[2],
    performance_rmcorr_anger_plus$CI[2],
    performance_rmcorr_anxiety_minus$CI[2],
    performance_rmcorr_bodily_minus$CI[2],
    performance_rmcorr_bodily_plus$CI[2],
    performance_rmcorr_cognitive_minus$CI[2],
    performance_rmcorr_cognitive_plus$CI[2],
    performance_rmcorr_motor_minus$CI[2],
    performance_rmcorr_motor_plus$CI[2],
    performance_rmcorr_operational_minus$CI[2],
    performance_rmcorr_operational_plus$CI[2],
    performance_rmcorr_pleasant_plus$CI[2],
    performance_rmcorr_volitional_minus$CI[2],
    performance_rmcorr_volitional_plus$CI[2]
  ),
  modality = c("Anger",
               "Anger",
               "Anxiety",
               "Bodily-somatic",
               "Bodily-somatic",
               "Cognitive",
               "Cognitive",
               "Motor-behavioural",
               "Motor-behavioural",
               "Operational",
               "Operational",
               "Pleasant states",
               "Volitional",
               "Volitional"
  ),
  valence = c("Negative",
              "Positive",
              "Negative",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Positive",
              "Negative",
              "Positive")
)


rmcrorr_fits <- bind_rows(
  
  bind_cols(performance_rmcorr_anger_minus$model$model,
            performance_rmcorr_anger_minus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_anger_plus$model$model,
            performance_rmcorr_anger_plus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_anxiety_minus$model$model,
            performance_rmcorr_anxiety_minus$model$fitted.values) |>
    mutate(
      modality = "Anxiety",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_minus$model$model,
            performance_rmcorr_bodily_minus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_plus$model$model,
            performance_rmcorr_bodily_plus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_cognitive_minus$model$model,
            performance_rmcorr_cognitive_minus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_cognitive_plus$model$model,
            performance_rmcorr_cognitive_plus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_motor_minus$model$model,
            performance_rmcorr_motor_minus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_motor_plus$model$model,
            performance_rmcorr_motor_plus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_operational_minus$model$model,
            performance_rmcorr_operational_minus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_operational_plus$model$model,
            performance_rmcorr_operational_plus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_pleasant_plus$model$model,
            performance_rmcorr_pleasant_plus$model$fitted.values) |>
    mutate(
      modality = "Pleasant states",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_volitional_minus$model$model,
            performance_rmcorr_volitional_minus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_volitional_plus$model$model,
            performance_rmcorr_volitional_plus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Positive"
    )
  
) |>
  rename(fit = "...4",
         `Perceived Impact` = "Measure1",
         Intensity = "Measure2")


study_3_PI_plot <-  dat_study_3_wide %>%
  ggplot(aes(y=Intensity+1, x = `Perceived Impact`)) +
  geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.5) + 
  geom_line(data = rmcrorr_fits,
            aes(y=fit+1, group = Participant),
            alpha = 0.75) +
  geom_text(data = rmcorr_stats, aes(x = -2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
  scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
  scale_x_continuous(limits = c(-3, 3), breaks = (-3:3),
                     labels = c("Very\nHarmful", "", "", "No Effect", "", "", "Very\nHelpful")) +facet_grid(cols = vars(valence), rows = vars(modality)) +
  labs(x = "Perceived Impact on Performance", y = "Response (Intensity)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))

ggsave("study_3_PI_plot.tiff", plot = study_3_PI_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)


# combine 2 & 3 ----

dat_comb_1_2 <- bind_rows(dat_study_2, dat_study_3)



performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_comb_1_2 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                           filter(modality == "Anger" & valence == "Negative"))

performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                        value, response,
                                        dataset = dat_comb_1_2 |>
                                          filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                          filter(modality == "Anger" & valence == "Positive"))

performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_comb_1_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Anxiety" & valence == "Negative"))

performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                          value, response,
                                          dataset = dat_comb_1_2 |>
                                            filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                            filter(modality == "Bodily-somatic" & valence == "Negative"))

performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_comb_1_2 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                           filter(modality == "Bodily-somatic" & valence == "Positive"))

performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_comb_1_2 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                               filter(modality == "Cognitive" & valence == "Negative"))

performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                            value, response,
                                            dataset = dat_comb_1_2 |>
                                              filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                              filter(modality == "Cognitive" & valence == "Positive"))

performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                         value, response,
                                         dataset = dat_comb_1_2 |>
                                           filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                           filter(modality == "Motor-behavioural" & valence == "Negative"))

performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                        value, response,
                                        dataset = dat_comb_1_2 |>
                                          filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                          filter(modality == "Motor-behavioural" & valence == "Positive"))

performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                               value, response,
                                               dataset = dat_comb_1_2 |>
                                                 filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                 filter(modality == "Operational" & valence == "Negative"))

performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                              value, response,
                                              dataset = dat_comb_1_2 |>
                                                filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                filter(modality == "Operational" & valence == "Positive"))

performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                           value, response,
                                           dataset = dat_comb_1_2 |>
                                             filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                             filter(modality == "Pleasant states" & valence == "Positive"))

performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                              value, response,
                                              dataset = dat_comb_1_2 |>
                                                filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                                filter(modality == "Volitional" & valence == "Negative"))

performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                             value, response,
                                             dataset = dat_comb_1_2 |>
                                               filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |> 
                                               filter(modality == "Volitional" & valence == "Positive"))


rmcorr_stats <- tibble(
  R = c(
    performance_rmcorr_anger_minus$r,
    performance_rmcorr_anger_plus$r,
    performance_rmcorr_anxiety_minus$r,
    performance_rmcorr_bodily_minus$r,
    performance_rmcorr_bodily_plus$r,
    performance_rmcorr_cognitive_minus$r,
    performance_rmcorr_cognitive_plus$r,
    performance_rmcorr_motor_minus$r,
    performance_rmcorr_motor_plus$r,
    performance_rmcorr_operational_minus$r,
    performance_rmcorr_operational_plus$r,
    performance_rmcorr_pleasant_plus$r,
    performance_rmcorr_volitional_minus$r,
    performance_rmcorr_volitional_plus$r
  ),
  conf.low = c(
    performance_rmcorr_anger_minus$CI[1],
    performance_rmcorr_anger_plus$CI[1],
    performance_rmcorr_anxiety_minus$CI[1],
    performance_rmcorr_bodily_minus$CI[1],
    performance_rmcorr_bodily_plus$CI[1],
    performance_rmcorr_cognitive_minus$CI[1],
    performance_rmcorr_cognitive_plus$CI[1],
    performance_rmcorr_motor_minus$CI[1],
    performance_rmcorr_motor_plus$CI[1],
    performance_rmcorr_operational_minus$CI[1],
    performance_rmcorr_operational_plus$CI[1],
    performance_rmcorr_pleasant_plus$CI[1],
    performance_rmcorr_volitional_minus$CI[1],
    performance_rmcorr_volitional_plus$CI[1]
  ),
  conf.high = c(
    performance_rmcorr_anger_minus$CI[2],
    performance_rmcorr_anger_plus$CI[2],
    performance_rmcorr_anxiety_minus$CI[2],
    performance_rmcorr_bodily_minus$CI[2],
    performance_rmcorr_bodily_plus$CI[2],
    performance_rmcorr_cognitive_minus$CI[2],
    performance_rmcorr_cognitive_plus$CI[2],
    performance_rmcorr_motor_minus$CI[2],
    performance_rmcorr_motor_plus$CI[2],
    performance_rmcorr_operational_minus$CI[2],
    performance_rmcorr_operational_plus$CI[2],
    performance_rmcorr_pleasant_plus$CI[2],
    performance_rmcorr_volitional_minus$CI[2],
    performance_rmcorr_volitional_plus$CI[2]
  ),
  modality = c("Anger",
               "Anger",
               "Anxiety",
               "Bodily-somatic",
               "Bodily-somatic",
               "Cognitive",
               "Cognitive",
               "Motor-behavioural",
               "Motor-behavioural",
               "Operational",
               "Operational",
               "Pleasant states",
               "Volitional",
               "Volitional"
  ),
  valence = c("Negative",
              "Positive",
              "Negative",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Positive",
              "Negative",
              "Positive")
)


rmcrorr_fits <- bind_rows(
  
  bind_cols(performance_rmcorr_anger_minus$model$model,
            performance_rmcorr_anger_minus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_anger_plus$model$model,
            performance_rmcorr_anger_plus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_anxiety_minus$model$model,
            performance_rmcorr_anxiety_minus$model$fitted.values) |>
    mutate(
      modality = "Anxiety",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_minus$model$model,
            performance_rmcorr_bodily_minus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_plus$model$model,
            performance_rmcorr_bodily_plus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_cognitive_minus$model$model,
            performance_rmcorr_cognitive_minus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_cognitive_plus$model$model,
            performance_rmcorr_cognitive_plus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_motor_minus$model$model,
            performance_rmcorr_motor_minus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_motor_plus$model$model,
            performance_rmcorr_motor_plus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_operational_minus$model$model,
            performance_rmcorr_operational_minus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_operational_plus$model$model,
            performance_rmcorr_operational_plus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_pleasant_plus$model$model,
            performance_rmcorr_pleasant_plus$model$fitted.values) |>
    mutate(
      modality = "Pleasant states",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_volitional_minus$model$model,
            performance_rmcorr_volitional_minus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_volitional_plus$model$model,
            performance_rmcorr_volitional_plus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Positive"
    )
  
) |>
  rename(fit = "...4",
         value = "Measure1",
         response = "Measure2")


study_2_3_combined_subjective_plot <- dat_comb_1_2 %>%
  filter(intensity_perceivedimpact == "Intensity" & str_detect(performance, "rating")) |>
  ggplot(aes(y=response+1, x = value)) +
  geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.25) + 
  geom_line(data = rmcrorr_fits,
            aes(y=fit+1, group = Participant),
            alpha = 0.5) +
  geom_text(data = rmcorr_stats, aes(x = 2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
  scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
  scale_x_continuous(limits = c(0,11), breaks = c(1,2,3,4,5)) +
  facet_grid(cols = vars(valence), rows = vars(modality)) +
  labs(x = "Performance (self-rated)", y = "Response (Intensity)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))


ggsave("study_2_3_combined_subjective_plot.tiff", plot = study_2_3_combined_subjective_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)


dat_comb_1_2_wide <- dat_comb_1_2 |>
  group_by(sheet, code, modality, valence, intensity_perceivedimpact) |>
  slice_head(n=1) |>
  select(sheet, code, modality, valence, intensity_perceivedimpact, response) |>
  pivot_wider(names_from = intensity_perceivedimpact,
              values_from = response,
              id_cols = c(code, modality, valence, sheet))


performance_rmcorr_anger_minus <- rmcorr(participant = code,
                                         `Perceived Impact`, Intensity,
                                         dataset = dat_comb_1_2_wide |> 
                                           filter(modality == "Anger" & valence == "Negative"))

performance_rmcorr_anger_plus <- rmcorr(participant = code,
                                        `Perceived Impact`, Intensity,
                                        dataset = dat_comb_1_2_wide |> 
                                          filter(modality == "Anger" & valence == "Positive"))

performance_rmcorr_anxiety_minus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_comb_1_2_wide |> 
                                             filter(modality == "Anxiety" & valence == "Negative"))

performance_rmcorr_bodily_minus <- rmcorr(participant = code,
                                          `Perceived Impact`, Intensity,
                                          dataset = dat_comb_1_2_wide |>
                                            filter(modality == "Bodily-somatic" & valence == "Negative"))

performance_rmcorr_bodily_plus <- rmcorr(participant = code,
                                         `Perceived Impact`, Intensity,
                                         dataset = dat_comb_1_2_wide |> 
                                           filter(modality == "Bodily-somatic" & valence == "Positive"))

performance_rmcorr_cognitive_minus <- rmcorr(participant = code,
                                             `Perceived Impact`, Intensity,
                                             dataset = dat_comb_1_2_wide |> 
                                               filter(modality == "Cognitive" & valence == "Negative"))

performance_rmcorr_cognitive_plus <- rmcorr(participant = code,
                                            `Perceived Impact`, Intensity,
                                            dataset = dat_comb_1_2_wide |> 
                                              filter(modality == "Cognitive" & valence == "Positive"))

performance_rmcorr_motor_minus <- rmcorr(participant = code,
                                         `Perceived Impact`, Intensity,
                                         dataset = dat_comb_1_2_wide |> 
                                           filter(modality == "Motor-behavioural" & valence == "Negative"))

performance_rmcorr_motor_plus <- rmcorr(participant = code,
                                        `Perceived Impact`, Intensity,
                                        dataset = dat_comb_1_2_wide |> 
                                          filter(modality == "Motor-behavioural" & valence == "Positive"))

performance_rmcorr_operational_minus <- rmcorr(participant = code,
                                               `Perceived Impact`, Intensity,
                                               dataset = dat_comb_1_2_wide |>
                                                 filter(modality == "Operational" & valence == "Negative"))

performance_rmcorr_operational_plus <- rmcorr(participant = code,
                                              `Perceived Impact`, Intensity,
                                              dataset = dat_comb_1_2_wide |> 
                                                filter(modality == "Operational" & valence == "Positive"))

performance_rmcorr_pleasant_plus <- rmcorr(participant = code,
                                           `Perceived Impact`, Intensity,
                                           dataset = dat_comb_1_2_wide |> 
                                             filter(modality == "Pleasant states" & valence == "Positive"))

performance_rmcorr_volitional_minus <- rmcorr(participant = code,
                                              `Perceived Impact`, Intensity,
                                              dataset = dat_comb_1_2_wide |>
                                                filter(modality == "Volitional" & valence == "Negative"))

performance_rmcorr_volitional_plus <- rmcorr(participant = code,
                                             `Perceived Impact`, Intensity,
                                             dataset = dat_comb_1_2_wide |>
                                               filter(modality == "Volitional" & valence == "Positive"))


rmcorr_stats <- tibble(
  R = c(
    performance_rmcorr_anger_minus$r,
    performance_rmcorr_anger_plus$r,
    performance_rmcorr_anxiety_minus$r,
    performance_rmcorr_bodily_minus$r,
    performance_rmcorr_bodily_plus$r,
    performance_rmcorr_cognitive_minus$r,
    performance_rmcorr_cognitive_plus$r,
    performance_rmcorr_motor_minus$r,
    performance_rmcorr_motor_plus$r,
    performance_rmcorr_operational_minus$r,
    performance_rmcorr_operational_plus$r,
    performance_rmcorr_pleasant_plus$r,
    performance_rmcorr_volitional_minus$r,
    performance_rmcorr_volitional_plus$r
  ),
  conf.low = c(
    performance_rmcorr_anger_minus$CI[1],
    performance_rmcorr_anger_plus$CI[1],
    performance_rmcorr_anxiety_minus$CI[1],
    performance_rmcorr_bodily_minus$CI[1],
    performance_rmcorr_bodily_plus$CI[1],
    performance_rmcorr_cognitive_minus$CI[1],
    performance_rmcorr_cognitive_plus$CI[1],
    performance_rmcorr_motor_minus$CI[1],
    performance_rmcorr_motor_plus$CI[1],
    performance_rmcorr_operational_minus$CI[1],
    performance_rmcorr_operational_plus$CI[1],
    performance_rmcorr_pleasant_plus$CI[1],
    performance_rmcorr_volitional_minus$CI[1],
    performance_rmcorr_volitional_plus$CI[1]
  ),
  conf.high = c(
    performance_rmcorr_anger_minus$CI[2],
    performance_rmcorr_anger_plus$CI[2],
    performance_rmcorr_anxiety_minus$CI[2],
    performance_rmcorr_bodily_minus$CI[2],
    performance_rmcorr_bodily_plus$CI[2],
    performance_rmcorr_cognitive_minus$CI[2],
    performance_rmcorr_cognitive_plus$CI[2],
    performance_rmcorr_motor_minus$CI[2],
    performance_rmcorr_motor_plus$CI[2],
    performance_rmcorr_operational_minus$CI[2],
    performance_rmcorr_operational_plus$CI[2],
    performance_rmcorr_pleasant_plus$CI[2],
    performance_rmcorr_volitional_minus$CI[2],
    performance_rmcorr_volitional_plus$CI[2]
  ),
  modality = c("Anger",
               "Anger",
               "Anxiety",
               "Bodily-somatic",
               "Bodily-somatic",
               "Cognitive",
               "Cognitive",
               "Motor-behavioural",
               "Motor-behavioural",
               "Operational",
               "Operational",
               "Pleasant states",
               "Volitional",
               "Volitional"
  ),
  valence = c("Negative",
              "Positive",
              "Negative",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Negative",
              "Positive",
              "Positive",
              "Negative",
              "Positive")
)


rmcrorr_fits <- bind_rows(
  
  bind_cols(performance_rmcorr_anger_minus$model$model,
            performance_rmcorr_anger_minus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_anger_plus$model$model,
            performance_rmcorr_anger_plus$model$fitted.values) |>
    mutate(
      modality = "Anger",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_anxiety_minus$model$model,
            performance_rmcorr_anxiety_minus$model$fitted.values) |>
    mutate(
      modality = "Anxiety",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_minus$model$model,
            performance_rmcorr_bodily_minus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_bodily_plus$model$model,
            performance_rmcorr_bodily_plus$model$fitted.values) |>
    mutate(
      modality = "Bodily-somatic",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_cognitive_minus$model$model,
            performance_rmcorr_cognitive_minus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_cognitive_plus$model$model,
            performance_rmcorr_cognitive_plus$model$fitted.values) |>
    mutate(
      modality = "Cognitive",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_motor_minus$model$model,
            performance_rmcorr_motor_minus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_motor_plus$model$model,
            performance_rmcorr_motor_plus$model$fitted.values) |>
    mutate(
      modality = "Motor-behavioural",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_operational_minus$model$model,
            performance_rmcorr_operational_minus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_operational_plus$model$model,
            performance_rmcorr_operational_plus$model$fitted.values) |>
    mutate(
      modality = "Operational",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_pleasant_plus$model$model,
            performance_rmcorr_pleasant_plus$model$fitted.values) |>
    mutate(
      modality = "Pleasant states",
      valence = "Positive"
    ),
  bind_cols(performance_rmcorr_volitional_minus$model$model,
            performance_rmcorr_volitional_minus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Negative"
    ),
  bind_cols(performance_rmcorr_volitional_plus$model$model,
            performance_rmcorr_volitional_plus$model$fitted.values) |>
    mutate(
      modality = "Volitional",
      valence = "Positive"
    )
  
) |>
  rename(fit = "...4",
         `Perceived Impact` = "Measure1",
         Intensity = "Measure2")


study_2_3_PI_plot <-  dat_comb_1_2_wide %>%
  ggplot(aes(y=Intensity+1, x = `Perceived Impact`)) +
  geom_point(position = position_jitter(w=0.1, h=0.1), size=0.5, alpha = 0.5) + 
  geom_line(data = rmcrorr_fits,
            aes(y=fit+1, group = Participant),
            alpha = 0.75) +
  geom_text(data = rmcorr_stats, aes(x = -2, y = 5.5, label = glue::glue("r = {round(R, 2)}\n(95%CI: {round(conf.low, 2)}, {round(conf.high, 2)})"), group = interaction(modality, valence)), size = 2.5) +
  scale_y_continuous(limits = c(0.5,6), breaks = c(1,2,3,4,5)) +
  scale_x_continuous(limits = c(-3, 3), breaks = (-3:3),
                     labels = c("Very\nHarmful", "", "", "No Effect", "", "", "Very\nHelpful")) +facet_grid(cols = vars(valence), rows = vars(modality)) +
  labs(x = "Perceived Impact on Performance", y = "Response (Intensity)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 6),
        axis.text = element_text(size = 6))

ggsave("study_2_3_PI_plot.tiff", plot = study_2_3_PI_plot,
       w = 7.5, h = 10, device = "tiff", dpi = 300)

