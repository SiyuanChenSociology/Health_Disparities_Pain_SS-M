
### related packages -----------------------------------------------------------
library(pacman)
pacman::p_load(tidyverse, janitor,
  haven, ggplot2, dplyr, expss, scales, ggeffects,
  geepack, MASS, marginaleffects
)


### Data import ----------------------------------------------------------------
dir()
data <- read_dta("pain.dta")
glimpse(data)


### Prepare variables before DA ------------------------------------------------
# factors pre
data <- data %>%
  mutate(
    # age group at baseline
    age_group = factor(cut(age,
                           breaks = c(-Inf, 49, 54, 59, 64, 69, Inf),
                           labels = c("45-49", "50-54", "55-59", "60-64", "65-69", "70+"), right = TRUE
    )),
    
    # gender
    gender = case_when(
      gender == 0 ~ "Female",
      gender == 1 ~ "Male"
    ),
    
    # urban-rural classification
    urban = case_when(
      urban == 0 ~ "Rural hukou holders living in rural areas",
      urban == 1 ~ "Rural hukou holders living in urban areas",
      urban == 2 ~ "Urban hukou holders"
    ),
    
    # education
    education = case_when(
      education == 0 ~ "0 Illiterate",
      education == 1 ~ "1 Less than elementary school",
      education == 2 ~ "2 Up to elementary school",
      education == 3 ~ "3 Middle school",
      education == 4 ~ "4 High School or beyond"
    )
  )


str(data$marital)
data$marital <- as_factor(data$marital) # using library: haven
table(data$marital)

data$age_group <- as.character(data$age_group)
data$age_group[data$age_group == "45-49"] <- "45-49 in 2011"
data$age_group[data$age_group == "50-54"] <- "50-54 in 2011"
data$age_group[data$age_group == "55-59"] <- "55-59 in 2011"
data$age_group[data$age_group == "60-64"] <- "60-64 in 2011"
data$age_group[data$age_group == "65-69"] <- "65-69 in 2011"
data$age_group[data$age_group == "70+"] <- "70+ in 2011"


data$pche_quartile[data$pche_quartile == "1"] <- "1st quartile"
data$pche_quartile[data$pche_quartile == "2"] <- "2nd quartile"
data$pche_quartile[data$pche_quartile == "3"] <- "3rd quartile"
data$pche_quartile[data$pche_quartile == "4"] <- "4th quartile"
data$pche_quartile[data$pche_quartile == "5"] <- "Missing"


# health condition pre
convert_labels <- function(var, labels, ref = NULL) {
  var <- as.character(var)
  for (i in seq_along(labels)) {
    var[var == names(labels)[i]] <- labels[[i]]
  }
  var <- as.factor(var)
  if (!is.null(ref)) var <- relevel(var, ref = ref)
  return(var)
}

data$arthritis_pre_2011 <- convert_labels(data$arthritis_pre_2011,
                                          c("0" = "No", "1" = "Yes", "2" = "Missing"),
                                          ref = "Yes"
)

data$hypertension_pre_2011 <- convert_labels(
  data$hypertension_pre_2011,
  c("0" = "No", "1" = "Yes", "2" = "Missing"),
  ref = "Yes"
)

data$diabetes_pre_2011 <- convert_labels(
  data$diabetes_pre_2011,
  c("0" = "No", "1" = "Yes", "2" = "Missing"),
  ref = "Yes"
)

data$dyslipidemia_pre_2011 <- convert_labels(
  data$dyslipidemia_pre_2011,
  c("0" = "No", "1" = "Yes", "2" = "Missing"),
  ref = "Yes"
)

# X: Years after the baseline
data$years <- data$wave - 2011








### Full sample
data$urban <- as.factor(data$urban)
data$urban <- relevel(data$urban, ref = "Rural hukou holders living in urban areas")
table(data$urban)


data$pche_quartile <- as.factor(data$pche_quartile)
data$pche_quartile <- relevel(data$pche_quartile, ref = "4th quartile")
table(data$pche_quartile)

# grand mean of years, mean = 4.11
full_data <- data
full_data$years <- full_data$years - mean(full_data$years)
summary(full_data$years)




# Using OR + 95% CI ------------------------------------------------------------
# 自定义函数来显示OR、原始系数、标准误差、显著性和95% CI
summary_gee_or <- function(model) {
  # 提取模型的系数表
  model_summary <- summary(model)$coefficients
  
  # 提取原始系数、标准误差、z值和p值
  coefs <- model_summary[, "Estimate"]
  se <- model_summary[, "Std.err"]
  
  # 检查是否有 "z value" 列名，如果没有则计算 z value
  if (!"z value" %in% colnames(model_summary)) {
    z_value <- coefs / se
  } else {
    z_value <- model_summary[, "z value"]
  }
  
  p_value <- 2 * pnorm(-abs(z_value)) # 计算 p 值
  
  # 计算OR和95%置信区间
  exp_coefs <- exp(coefs)
  lower_ci <- exp(coefs - 1.96 * se)
  upper_ci <- exp(coefs + 1.96 * se)
  
  # 添加显著性星号
  significance <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*",
                                       ifelse(p_value < 0.1, ".", ""))))
  
  # 组织结果为数据框
  results <- data.frame(
    Variable = rownames(model_summary), # 添加变量名
    Estimate = coefs,
    `Std. Error` = se,
    `z value` = z_value,
    `Pr(>|z|)` = p_value,
    OR = exp_coefs,
    `2.5 %` = lower_ci,
    `97.5 %` = upper_ci,
    Significance = significance
  )
  
  return(results)
}

summary_gee_or2 <- function(model) {
  # 提取模型的系数表
  model_summary <- summary(model)$coefficients
  
  # 提取原始系数、标准误差、z值和p值
  coefs <- model_summary[, "Estimate"]
  se <- model_summary[, "Std.err"]
  
  # 检查是否有 "z value" 列名，如果没有则计算 z value
  if (!"z value" %in% colnames(model_summary)) {
    z_value <- coefs / se
  } else {
    z_value <- model_summary[, "z value"]
  }
  
  p_value <- 2 * pnorm(-abs(z_value)) # 计算 p 值
  
  # 计算OR和95%置信区间
  exp_coefs <- exp(coefs)
  lower_ci <- exp(coefs - 1.96 * se)
  upper_ci <- exp(coefs + 1.96 * se)
  
  # 添加显著性星号
  significance <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*",
                                       ifelse(p_value < 0.1, ".", ""))))
  
  # 组织结果为数据框，并保留两位小数
  results <- data.frame(
    Variable = rownames(model_summary), # 添加变量名
    Estimate = round(coefs, 2),
    `Std. Error` = round(se, 2),
    `z value` = round(z_value, 2),
    `Pr(>|z|)` = round(p_value, 2),
    OR = round(exp_coefs, 2),
    `2.5 %` = round(lower_ci, 2),
    `97.5 %` = round(upper_ci, 2),
    Significance = significance
  )
  
  return(results)
}





# ------------------  now is the analysis part ---------------------------------
data <- data %>%
  mutate(measure = case_when(
    wave %in% c(2011, 2015) ~ 1,
    wave %in% c(2013, 2018, 2020) ~ 0,
    TRUE ~ NA_real_  
  ))

data %>% 
  tabyl(measure)

data <- data %>%
  mutate(pain01 = ifelse(pain > 0, 1, 0))

table(data$pain01) # PAIN = 0, N IS 39178 // after deleting chronic condition, = 0, n is 37146, = 1, n is 28009
table(data$pain) # PAIN = 0, N IS 39178 // consistent with pain01
# table(data$prevalence)

### Full sample ----------------------------------------------------------------
# Robust check 1. add the measurement chang
full_1 <- geeglm(
   pain01 ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011 + 
    measure,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_1)
QIC(full_1)

# OR format
summary_gee_or(full_1)

summary_gee_or2(full_1)






# Robust check 2. using different body pain
# plot

# Without urban_rural
# 1. Body pain prevalence at every wave
pain_prevalence_1 <- data %>%
  group_by(wave) %>%
  summarise(
    head = mean(head, na.rm = TRUE),
    shoulder = mean(shoulder, na.rm = TRUE),
    arm = mean(arm, na.rm = TRUE),
    wrist = mean(wrist, na.rm = TRUE),
    fingers = mean(fingers, na.rm = TRUE),
    chest = mean(chest, na.rm = TRUE),
    stomach = mean(stomach, na.rm = TRUE),
    back = mean(back, na.rm = TRUE),
    waist = mean(waist, na.rm = TRUE),
    buttock = mean(buttock, na.rm = TRUE),
    leg = mean(leg, na.rm = TRUE),
    knee = mean(knee, na.rm = TRUE),
    ankle = mean(ankle, na.rm = TRUE),
    toes = mean(toes, na.rm = TRUE),
    neck = mean(neck, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -wave, names_to = "body_part", values_to = "prevalence")


# Order
pain_prevalence_1 <- pain_prevalence_1 %>%
  group_by(body_part) %>%
  mutate(max_prevalence = max(prevalence)) %>%
  ungroup() %>%
  mutate(body_part = reorder(body_part, -max_prevalence)) %>%
  mutate(prevalence = prevalence * 100)

# plot
ggplot(pain_prevalence_1, aes(x = wave, y = prevalence, group = body_part, color = body_part)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(
    aes(label = round(prevalence, 2)),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~body_part, scales = "free_y", ncol = 3) +
  scale_x_continuous(
    breaks = c(2011, 2013, 2015, 2018, 2020),
    limits = c(2011, 2020)
  ) +
  labs(
    title = "Pain Prevalence Trends by Body Part",
    x = "Year",
    y = "Prevalence",
    color = "Body Part"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "none"
  )




# plot
ggplot(pain_prevalence_1, aes(x = wave, y = prevalence, group = body_part, color = body_part)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(
      label = round(prevalence, 2),
      vjust = ifelse(prevalence == max(prevalence), 1.5, -0.3)
    ),
    size = 4
  ) +
  facet_wrap(~body_part, scales = "fixed", ncol = 3) +
  scale_x_continuous(
    breaks = c(2011, 2013, 2015, 2018, 2020),
    limits = c(2011, 2020)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(
    title = "Pain Prevalence Trends by Specific Body Part",
    x = "Survey Year",
    y = "Prevalence (%)",
    color = "Body Part"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "none"
  )





# Pain prevalence at body part across urban-rural classification and waves 
pain_prevalence <- data %>%
  group_by(wave, urban) %>%
  summarise(
    head = mean(head, na.rm = TRUE),
    shoulder = mean(shoulder, na.rm = TRUE),
    arm = mean(arm, na.rm = TRUE),
    wrist = mean(wrist, na.rm = TRUE),
    fingers = mean(fingers, na.rm = TRUE),
    chest = mean(chest, na.rm = TRUE),
    stomach = mean(stomach, na.rm = TRUE),
    back = mean(back, na.rm = TRUE),
    waist = mean(waist, na.rm = TRUE),
    buttock = mean(buttock, na.rm = TRUE),
    leg = mean(leg, na.rm = TRUE),
    knee = mean(knee, na.rm = TRUE),
    ankle = mean(ankle, na.rm = TRUE),
    toes = mean(toes, na.rm = TRUE),
    neck = mean(neck, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -c(wave, urban),
    names_to = "body_part",
    values_to = "prevalence"
  ) %>%
  ungroup()

# Order
pain_prevalence <- pain_prevalence %>%
  group_by(body_part) %>%
  mutate(max_prevalence = max(prevalence)) %>%
  ungroup() %>%
  mutate(body_part = reorder(body_part, -max_prevalence)) %>%
  mutate(prevalence = prevalence * 100)

# plot
ggplot(pain_prevalence, aes(x = wave, y = prevalence, group = urban, color = urban)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~body_part, scales = "free_y", ncol = 3) +
  scale_x_continuous(
    breaks = c(2011, 2013, 2015, 2018, 2020),
    limits = c(2011, 2020)
  ) +
  labs(
    title = "Pain Prevalence Trends by Urban-Rural Classification and Body Part",
    x = "Survey Year",
    y = "Prevalence (%)",
    color = "Urban Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )






# GEE Model part

# 1. Waist
class(data$waist)
summary(data$waist)
data$waist <- as.numeric(data$waist)

full_b1_waist <- geeglm(
  waist ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_waist)
QIC(full_b1_waist)

# OR format
summary_gee_or(full_b1_waist)


# 2. knee
class(data$knee)
summary(data$knee)
data$knee <- as.numeric(data$knee)

full_b1_knee <- geeglm(
  knee ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_knee)
QIC(full_b1_knee)

# OR format
summary_gee_or(full_b1_knee)



# 3. Leg
class(data$leg)
summary(data$leg)
data$leg <- as.numeric(data$leg)

full_b1_leg <- geeglm(
  leg ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_leg)
QIC(full_b1_leg)

# OR format
summary_gee_or(full_b1_leg)



# 4. head
class(data$head)
any(is.na(data$head))
data$head <- as.numeric(data$head)

full_b1_head <- geeglm(
  head ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_head)
QIC(full_b1_head)

# OR format
summary_gee_or(full_b1_head)


# 5. shoulder
class(data$shoulder)
any(is.na(data$shoulder))
data$shoulder <- as.numeric(data$shoulder)

full_b1_shoulder <- geeglm(
  shoulder ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_shoulder)
QIC(full_b1_shoulder)

# OR format
summary_gee_or(full_b1_shoulder)


# 6. arm
class(data$arm)
any(is.na(data$arm))
data$arm <- as.numeric(data$arm)

full_b1_arm <- geeglm(
  arm ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_arm)
QIC(full_b1_arm)

# OR format
summary_gee_or(full_b1_arm)


# 7. back
class(data$back)
any(is.na(data$back))
data$back <- as.numeric(data$back)

full_b1_back <- geeglm(
  back ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_back)
QIC(full_b1_back)

# OR format
summary_gee_or(full_b1_back)


# 8. stomach
class(data$stomach)
any(is.na(data$stomach))
data$stomach <- as.numeric(data$stomach)

full_b1_stomach <- geeglm(
  stomach ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_stomach)
QIC(full_b1_stomach)

# OR format
summary_gee_or(full_b1_stomach)


# 9. neck
class(data$neck)
any(is.na(data$neck))
data$neck <- as.numeric(data$neck)

full_b1_neck <- geeglm(
  neck ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_neck)
QIC(full_b1_neck)

# OR format
summary_gee_or(full_b1_neck)


# 10. fingers
class(data$fingers)
any(is.na(data$fingers))
data$fingers <- as.numeric(data$fingers)

full_b1_fingers <- geeglm(
  fingers ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_fingers)
QIC(full_b1_fingers)

# OR format
summary_gee_or(full_b1_fingers)


# 11. ankle
class(data$ankle)
any(is.na(data$ankle))
data$ankle <- as.numeric(data$ankle)

full_b1_ankle <- geeglm(
  ankle ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_ankle)
QIC(full_b1_ankle)

# OR format
summary_gee_or(full_b1_ankle)


# 12. wrist
class(data$wrist)
any(is.na(data$wrist))
data$wrist <- as.numeric(data$wrist)

full_b1_wrist <- geeglm(
  wrist ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_wrist)
QIC(full_b1_wrist)

# OR format
summary_gee_or(full_b1_wrist)


# 13. chest
class(data$chest)
any(is.na(data$chest))
data$chest <- as.numeric(data$chest)

full_b1_chest <- geeglm(
  chest ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_chest)
QIC(full_b1_chest)

# OR format
summary_gee_or(full_b1_chest)


# 14. buttock
class(data$buttock)
any(is.na(data$buttock))
data$buttock <- as.numeric(data$buttock)

full_b1_buttock <- geeglm(
  buttock ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_buttock)
QIC(full_b1_buttock)

# OR format
summary_gee_or(full_b1_buttock)



# 15. toes
class(data$toes)
any(is.na(data$toes))
data$toes <- as.numeric(data$toes)

full_b1_toes <- geeglm(
  toes ~ years + urban + years * urban + gender  + marital + age_group + 
    education + pche_quartile + arthritis_pre_2011 +
    hypertension_pre_2011 + dyslipidemia_pre_2011 + diabetes_pre_2011,
  id = id,
  data = data,
  family = binomial(link = "logit"),
  corstr = "unstructured" 
)
summary(full_b1_toes)
QIC(full_b1_toes)

# OR format
summary_gee_or(full_b1_toes)




# --------------------------------- Count Model --------------------------------

# pain_count

# 假设 data 是你的数据框，包含 'wave', 'pain_count' 和 'urban' 列

# 计算每个 wave 和 urban 组的平均疼痛计数
mean_pain_count <- data %>%
  group_by(wave, urban) %>%  # 按 wave 和 urban 分组
  summarise(mean_pain_count = mean(pain_count, na.rm = TRUE)) %>%
  ungroup()  # 解除分组

# 绘制图形
ggplot(data = mean_pain_count, aes(x = wave, y = mean_pain_count, color = urban, group = urban)) +
  geom_line(size = 1) +  # 根据 urban 列自动为每条线选择颜色
  geom_point(size = 2) + 
  geom_text(aes(label = round(mean_pain_count, 2)), 
            vjust = -0.5, 
            size = 5, 
            color = "black") +
  scale_x_continuous(
    breaks = c(2011, 2013, 2015, 2018, 2020),
    limits = c(2011, 2020)
  ) +
  labs(
    title = "Pain Count over Survey Years by Urban Group",
    x = "Survey Year",
    y = "Mean Pain Count",
    color = "Urban Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom"  # 可以调整 legend 位置
  )





library(glmmTMB)
model_control <- glmmTMBControl(
  optCtrl = list(iter.max = 1000, eval.max = 1000),
  optimizer = "nlminb"
)

model_2 <- glmmTMB(
  pain_count ~ years + urban + years * urban + (1 | id),  
  ziformula = ~ gender  + marital + age_group +
                education + pche_quartile ,  
  family = nbinom2(),     
  data = data
  )

summary(model_2)


model_2_lr <- glmmTMB(
  pain_count ~ years + urban + years * urban ,  
  ziformula = ~ gender  + marital + age_group +
    education + pche_quartile ,  
  family = nbinom2(),     
  data = data
)

summary(model_2_lr)

lrt_result <- anova(model_2_lr, model_2)
print(lrt_result)


model_summary <- summary(model_2)
model_summary 

# 保留 3 位小数
coef_fixed <- round(model_summary$coefficients$cond, 3)  # 固定效应
coef_zero <- round(model_summary$coefficients$zi, 3)    # 零膨胀部分

# 输出格式化的结果
print("Fixed Effects (Count):")
print(coef_fixed)

print("Zero-Inflation Effects:")
print(coef_zero)








