# Prep Workspace ----------------------------------------------------------
library(tidyverse)

rm(list = ls())

#load the csv file
framingham <- read.csv(file = "./raw_data/framingham.csv", header = TRUE)

# get some basic info about the data
names(framingham)
summary(framingham)
head(framingham)


# Task #1 -----------------------------------------------------------------
# How does the shock index differ for different cholesterol levels in women?

# create subset dataframe that only includes women 
# and desired variables
female_chol <- framingham %>% 
  filter(gender == "Female" & !is.na(totChol)) %>% 
  mutate(shockIndex = heartRate/sysBP,
         cholRisk = case_when(totChol %in% 0:199 ~ "Desirable",
                              totChol %in% 200:239 ~ "Borderline",
                              totChol %in% 240:9999 ~ "High"),
         cholRisk = factor(cholRisk),
         cholRisk = fct_relevel(cholRisk, "Desirable"),
         cholValue = case_when(totChol %in% 0:199 ~ "< 200 mg/dL",
                               totChol %in% 200:239 ~ "200 - 240 mg/dL",
                               totChol %in% 240:9999 ~ "> 240 mg/dL"),
         cholValue = factor(cholValue),
         cholValue = fct_relevel(cholValue, "< 200 mg/dL", "200 - 240 mg/dL", "> 240 mg/dL"))

# compare shock index to cholesterol level
px_1_chol_SI <- ggplot(female_chol) +
  aes(x = cholValue, y = shockIndex, color = cholRisk) +
  geom_point(alpha = 0.3, size = 2, position = position_jitter(width = 0.2)) +
  stat_summary(fun = "mean", geom = "point", size = 4, color = "black") +
  scale_y_continuous(limits = c(0,1.25)) +
  coord_flip() +
  labs(x = "Cholesterol Range", y = "Shock Index", 
       title = "Cholesterol Levels of Women from Framingham, Massachusetts",
       subtitle = "Data collected as part of The Framingham Heart Study beginning in 1948",
       color = "Cholesterol Level",
       caption = "Black dot represents group average Shock Index") +
  theme_bw() + theme(plot.caption = element_text(face = "italic", hjust = 0))
px_1_chol_SI

ggsave(px_1_chol_SI, filename = "px_1_chol_SI.png", width = 8, height = 4, dpi = 150)

# summary stats
sum_stats <- female_chol %>% 
  group_by(cholRisk) %>% 
  summarise(stdev = sd(totChol),
            mean = mean(totChol))

# Rough Conclusion: There does not appear to be a significant difference in Shock Index between the three cholesterol groups
# and they have similar levels of spread and variability within the data. 



# Task #2 -----------------------------------------------------------------
# How does the Shock Index differ for diabetics vs non-diabetics?

chol <- framingham %>% 
  filter(!is.na(totChol)) %>% 
  mutate(shockIndex = heartRate/sysBP,
         diabetes.fct = factor(diabetes),
         diabetes.fct = fct_recode(diabetes.fct, "Diabetic" = "1", "Non-Diabetic" = "0"))

 
px_2_diabetic_SI <- ggplot(chol) +
  aes(x = diabetes.fct, y = shockIndex, color = diabetes.fct) +
  # geom_boxplot(width = 0.5, outlier.shape = 8) +
  geom_point(alpha = 0.3, size = 2, position = position_jitter(width = 0.2)) +
  stat_summary(fun = "mean", geom = "point", size = 4, color = "black") +
  scale_y_continuous(limits = c(0,1.25)) +
  coord_flip() +
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1) +
  labs(x = "Diabetes Status", y = "Shock Index", 
       title = "Shock Index of People With and Without Diabetes from Framingham, Massachusetts",
       subtitle = "Data collected as part of The Framingham Heart Study beginning in 1948",
       caption = "Black dot represents group average Shock Index and the dashed line is an index of 0.9") +
  guides(color = FALSE) +
  theme_bw() + theme(plot.caption = element_text(face = "italic", hjust = 0))
px_2_diabetic_SI

ggsave(px_2_diabetic_SI, filename = "px_2_diabetic_SI.png", width = 8, height = 4, dpi = 150)

# which group has a higher rate of shock indexes > 0.9 ?
diabetes_shock <- chol %>% 
  filter(shockIndex >= 0.9) %>% 
  group_by(diabetes.fct) %>% 
  count()


# Rough Conclusion: There does not appear to be a difference between the two groups, as indicated by the mean shock index
# and the overall spread of the data. However, there are more individuals in the non-diabetes group compared to the diabetes group. 
# This could explain why the non-diabetic group has a greater number of individuals with a shock index > 0.9 (n = 35) compared to the 
# diabetic group (n = 4). 



