# 4th round review: Double check for no miscoding in data wrangling & analysis
# Final check: 10th - 13rd Jan., 2025

setwd("/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 4/Data Cleaning/raw_data")
dir()

library(haven)
library(tidyverse)
library(janitor)
library(labelled)

############# --------- survey year 2011 -----------#############################
health_2011 <- read_dta("health_2011.dta")
dim(health_2011)

### Outcome check
### da041: Currently Feel Any Body Pains / yes vs no, missing = 69
var_label(health_2011$da041)

health_2011 %>%
  tabyl(da041)

# we delete all the missing (0.39%)
health_2011 <- health_2011 %>%
  filter(!is.na(da041))

health_2011 <- health_2011 %>%
  mutate(prevalence = case_when(
    da041 == 1 ~ 1,
    da041 == 2 ~ 0
  ))

health_2011 %>%
  tabyl(prevalence)


####### ------------ Specific body-pain in 2011 --------------------------------
############## 1. da042s1: Head pain
var_label(health_2011$da042s1)

# the missing here, actually refers to no head pain, due to the routine question
health_2011 <- health_2011 %>%
  rename(head = da042s1)

health_2011 <- health_2011 %>%
  mutate(head = case_when(
    is.na(head) ~ 0,
    head == 1 ~ 1,
    TRUE ~ head
  ))

health_2011 %>%
  tabyl(head)

############## 2. da042s2: Shoulder Pain
var_label(health_2011$da042s2)

health_2011 <- health_2011 %>%
  rename(shoulder = da042s2)

health_2011 <- health_2011 %>%
  mutate(shoulder = case_when(
    is.na(shoulder) ~ 0,
    shoulder == 2 ~ 1,
    TRUE ~ shoulder
  ))

health_2011 %>%
  tabyl(shoulder)


############## 3. da042s3: Arm Pain
var_label(health_2011$da042s3)

health_2011 <- health_2011 %>%
  rename(arm = da042s3)

health_2011 <- health_2011 %>%
  mutate(arm = case_when(
    is.na(arm) ~ 0,
    arm == 3 ~ 1,
    TRUE ~ arm
  ))

health_2011 %>%
  tabyl(arm)


############## 4. da042s4: Wrist pain
var_label(health_2011$da042s4)

health_2011 <- health_2011 %>%
  rename(wrist = da042s4)

health_2011 <- health_2011 %>%
  mutate(wrist = case_when(
    is.na(wrist) ~ 0,
    wrist == 4 ~ 1,
    TRUE ~ wrist
  ))

health_2011 %>%
  tabyl(wrist)


############## 5. da042s5: Fingers pain
var_label(health_2011$da042s5)

health_2011 <- health_2011 %>%
  rename(fingers = da042s5)

health_2011 <- health_2011 %>%
  mutate(fingers = case_when(
    is.na(fingers) ~ 0,
    fingers == 5 ~ 1,
    TRUE ~ fingers
  ))

health_2011 %>%
  tabyl(fingers)


############## 6. da042s6: Chest pain
var_label(health_2011$da042s6)

health_2011 <- health_2011 %>%
  rename(chest = da042s6)

health_2011 <- health_2011 %>%
  mutate(chest = case_when(
    is.na(chest) ~ 0,
    chest == 6 ~ 1,
    TRUE ~ chest
  ))

health_2011 %>%
  tabyl(chest)


############## 7. da042s7: Stomach pain
var_label(health_2011$da042s7)

health_2011 <- health_2011 %>%
  rename(stomach = da042s7)

health_2011 <- health_2011 %>%
  mutate(stomach = case_when(
    is.na(stomach) ~ 0,
    stomach == 7 ~ 1,
    TRUE ~ stomach
  ))

health_2011 %>%
  tabyl(stomach)


############## 8. da042s8: Back pain
var_label(health_2011$da042s8)

health_2011 <- health_2011 %>%
  rename(back = da042s8)

health_2011 <- health_2011 %>%
  mutate(back = case_when(
    is.na(back) ~ 0,
    back == 8 ~ 1,
    TRUE ~ back
  ))

health_2011 %>%
  tabyl(back)


############## 9. da042s9: Waist pain
var_label(health_2011$da042s9)

health_2011 <- health_2011 %>%
  rename(waist = da042s9)

health_2011 <- health_2011 %>%
  mutate(waist = case_when(
    is.na(waist) ~ 0,
    waist == 9 ~ 1,
    TRUE ~ waist
  ))

health_2011 %>%
  tabyl(waist)


############## 10. da042s10: Buttocks pain
var_label(health_2011$da042s10)

health_2011 <- health_2011 %>%
  rename(buttock = da042s10)

health_2011 <- health_2011 %>%
  mutate(buttock = case_when(
    is.na(buttock) ~ 0,
    buttock == 10 ~ 1,
    TRUE ~ buttock
  ))

health_2011 %>%
  tabyl(buttock)


############## 11. da042s11: Leg pain
var_label(health_2011$da042s11)

health_2011 <- health_2011 %>%
  rename(leg = da042s11)

health_2011 <- health_2011 %>%
  mutate(leg = case_when(
    is.na(leg) ~ 0,
    leg == 11 ~ 1,
    TRUE ~ leg
  ))

health_2011 %>%
  tabyl(leg)


############## 12. da042s12: knees pain
var_label(health_2011$da042s12)

health_2011 <- health_2011 %>%
  rename(knee = da042s12)

health_2011 <- health_2011 %>%
  mutate(knee = case_when(
    is.na(knee) ~ 0,
    knee == 12 ~ 1,
    TRUE ~ knee
  ))

health_2011 %>%
  tabyl(knee)


############## 13. da042s13: Ankle pain
var_label(health_2011$da042s13)

health_2011 <- health_2011 %>%
  rename(ankle = da042s13)

health_2011 <- health_2011 %>%
  mutate(ankle = case_when(
    is.na(ankle) ~ 0,
    ankle == 13 ~ 1,
    TRUE ~ ankle
  ))

health_2011 %>%
  tabyl(ankle)


############## 14. da042s14: Toes pain
var_label(health_2011$da042s14)

health_2011 <- health_2011 %>%
  rename(toes = da042s14)

health_2011 <- health_2011 %>%
  mutate(toes = case_when(
    is.na(toes) ~ 0,
    toes == 14 ~ 1,
    TRUE ~ toes
  ))

health_2011 %>%
  tabyl(toes)


############## 15. da042s15: Neck pain
var_label(health_2011$da042s15)

health_2011 <- health_2011 %>%
  rename(neck = da042s15)

health_2011 <- health_2011 %>%
  mutate(neck = case_when(
    is.na(neck) ~ 0,
    neck == 15 ~ 1,
    TRUE ~ neck
  ))

health_2011 %>%
  tabyl(neck)


####### ------------ summarize pain-2011 ----------------------------------------
health_2011 %>%
  summarise(across(
    c(
      head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
      buttock, leg, knee, ankle, toes, neck
    ),
    \(x) mean(x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean_value") %>%
  arrange(desc(mean_value))

# Survey year 2011, ID needs to adjust
pain_2011 <- health_2011 %>%
  dplyr::select(
    ID, householdID, head, shoulder, arm, wrist, fingers, chest, stomach,
    back, waist, buttock, leg, knee, ankle, toes, neck, prevalence
  )

pain_2011 <- pain_2011 %>%
  arrange(ID) %>%
  mutate(
    householdID = paste0(householdID, "0"),
    ID = paste0(householdID, substr(ID, nchar(ID) - 1, nchar(ID)))
  )

pain_2011 <- pain_2011 %>%
  mutate(pain_count = head + shoulder + arm + wrist + fingers + chest +
    stomach + back + waist + buttock + leg + knee + ankle + toes + neck)
pain_2011 %>% tabyl(pain_count)
pain_2011 %>% tabyl(prevalence)

pain_2011 <- pain_2011 %>%
  dplyr::select(
    ID, head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
    buttock, leg, knee, ankle, toes, neck, prevalence, pain_count
  )


pain_2011 <- pain_2011 %>%
  mutate(year = 2011)





########## ----------- survey year 2013 ------------- #########################
health_2013 <- read_dta("health_2013.dta")
dim(health_2013)

### Outcome check
var_label(health_2013$wb16)
health_2013 %>%
  tabyl(wb16)

health_2013 <- health_2013 %>%
  filter(!is.na(wb16))

health_2013 <- health_2013 %>%
  mutate(prevalence = case_when(
    wb16 == 1 ~ 0,
    wb16 == 2 | wb16 == 3 | wb16 == 4 | wb16 == 5 ~ 1
  ))

health_2013 %>%
  tabyl(prevalence)


####### ----------- Specific body-pain in 2013 ---------------------------------
####### 1. head
var_label(health_2013$da042s1)

health_2013 <- health_2013 %>%
  rename(head = da042s1)

health_2013 <- health_2013 %>%
  mutate(head = case_when(
    is.na(head) ~ 0,
    head == 1 ~ 1,
    TRUE ~ head
  ))

health_2013 %>%
  tabyl(head)


####### 2. shoulder
health_2013 <- health_2013 %>%
  rename(shoulder = da042s2)


health_2013 <- health_2013 %>%
  mutate(shoulder = case_when(
    is.na(shoulder) ~ 0,
    shoulder == 2 ~ 1,
    TRUE ~ shoulder
  ))

health_2013 %>%
  tabyl(shoulder)



####### 3. arm
health_2013 <- health_2013 %>%
  rename(arm = da042s3)



health_2013 <- health_2013 %>%
  mutate(arm = case_when(
    is.na(arm) ~ 0,
    arm == 3 ~ 1,
    TRUE ~ arm
  ))


health_2013 %>%
  tabyl(arm)

####### 4. wrist
health_2013 <- health_2013 %>%
  rename(wrist = da042s4)

health_2013 <- health_2013 %>%
  mutate(wrist = case_when(
    is.na(wrist) ~ 0,
    wrist == 4 ~ 1,
    TRUE ~ wrist
  ))

health_2013 %>%
  tabyl(wrist)

####### 5. fingers
health_2013 <- health_2013 %>%
  rename(fingers = da042s5)

health_2013 <- health_2013 %>%
  mutate(fingers = case_when(
    is.na(fingers) ~ 0,
    fingers == 5 ~ 1,
    TRUE ~ fingers
  ))

health_2013 %>%
  tabyl(fingers)

####### 6. chest
health_2013 <- health_2013 %>%
  rename(chest = da042s6)

health_2013 <- health_2013 %>%
  mutate(chest = case_when(
    is.na(chest) ~ 0,
    chest == 6 ~ 1,
    TRUE ~ chest
  ))

health_2013 %>%
  tabyl(chest)

####### 7 stomach
health_2013 <- health_2013 %>%
  rename(stomach = da042s7)

health_2013 <- health_2013 %>%
  mutate(stomach = case_when(
    is.na(stomach) ~ 0,
    stomach == 7 ~ 1,
    TRUE ~ stomach
  ))

health_2013 %>%
  tabyl(stomach)

####### 8. back
health_2013 <- health_2013 %>%
  rename(back = da042s8)

health_2013 <- health_2013 %>%
  mutate(back = case_when(
    is.na(back) ~ 0,
    back == 8 ~ 1,
    TRUE ~ back
  ))

health_2013 %>%
  tabyl(back)


####### 9. waist
health_2013 <- health_2013 %>%
  rename(waist = da042s9)

health_2013 <- health_2013 %>%
  mutate(waist = case_when(
    is.na(waist) ~ 0,
    waist == 9 ~ 1,
    TRUE ~ waist
  ))

health_2013 %>%
  tabyl(waist)


####### 10. buttock
health_2013 <- health_2013 %>%
  rename(buttock = da042s10)

health_2013 <- health_2013 %>%
  mutate(buttock = case_when(
    is.na(buttock) ~ 0,
    buttock == 10 ~ 1,
    TRUE ~ buttock
  ))

health_2013 %>%
  tabyl(buttock)


####### 11. leg
health_2013 <- health_2013 %>%
  rename(leg = da042s11)

health_2013 <- health_2013 %>%
  mutate(leg = case_when(
    is.na(leg) ~ 0,
    leg == 11 ~ 1,
    TRUE ~ leg
  ))

health_2013 %>%
  tabyl(leg)


####### 12. Knee
health_2013 <- health_2013 %>%
  rename(knee = da042s12)

health_2013 <- health_2013 %>%
  mutate(knee = case_when(
    is.na(knee) ~ 0,
    knee == 12 ~ 1,
    TRUE ~ knee
  ))

health_2013 %>%
  tabyl(knee)


####### 13. Ankle
health_2013 <- health_2013 %>%
  rename(ankle = da042s13)

health_2013 <- health_2013 %>%
  mutate(ankle = case_when(
    is.na(ankle) ~ 0,
    ankle == 13 ~ 1,
    TRUE ~ ankle
  ))

health_2013 %>%
  tabyl(ankle)


####### 14. Toes
health_2013 <- health_2013 %>%
  rename(toes = da042s14)

health_2013 <- health_2013 %>%
  mutate(toes = case_when(
    is.na(toes) ~ 0,
    toes == 14 ~ 1,
    TRUE ~ toes
  ))

health_2013 %>%
  tabyl(toes)


####### 15. Neck
health_2013 <- health_2013 %>%
  rename(neck = da042s15)

health_2013 <- health_2013 %>%
  mutate(neck = case_when(
    is.na(neck) ~ 0,
    neck == 15 ~ 1,
    TRUE ~ neck
  ))

health_2013 %>%
  tabyl(neck)


####### ---------------- Summarize 2013 ---------------- #######################
health_2013 %>%
  summarise(across(
    c(
      head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
      buttock, leg, knee, ankle, toes, neck
    ),
    \(x) mean(x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean_value") %>%
  arrange(desc(mean_value))


pain_2013 <- health_2013 %>%
  dplyr::select(
    ID, head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
    buttock, leg, knee, ankle, toes, neck, prevalence
  )

pain_2013 <- pain_2013 %>%
  mutate(pain_count = head + shoulder + arm + wrist + fingers + chest +
    stomach + back + waist + buttock + leg + knee + ankle + toes + neck)


pain_2013 <- pain_2013 %>%
  mutate(year = 2013)











############## ----------survey year 2015 -------------##########################
health_2015 <- read_dta("health_2015.dta")
dim(health_2015)

### outcome check
var_label(health_2015$da041)

health_2015 %>%
  tabyl(da041)

# we delete all the missing
health_2015 <- health_2015 %>%
  filter(!is.na(da041))

health_2015 <- health_2015 %>%
  mutate(prevalence = case_when(
    da041 == 2 ~ 0,
    da041 == 1 ~ 1
  ))

health_2015 %>%
  tabyl(prevalence)



####### ------------ Specific body-pain in 2015 ------------####################
# 1. head
var_label(health_2015$da042s1)

health_2015 <- health_2015 %>%
  rename(head = da042s1)

health_2015 <- health_2015 %>%
  mutate(head = case_when(
    is.na(head) ~ 0,
    head == 1 ~ 1,
    TRUE ~ head
  ))

health_2015 %>%
  tabyl(head)

# 2. shoulder
health_2015 <- health_2015 %>%
  rename(shoulder = da042s2)

health_2015 <- health_2015 %>%
  mutate(shoulder = case_when(
    is.na(shoulder) ~ 0,
    shoulder == 2 ~ 1,
    TRUE ~ shoulder
  ))

health_2015 %>%
  tabyl(shoulder)

# 3. arm
health_2015 <- health_2015 %>%
  rename(arm = da042s3)

health_2015 <- health_2015 %>%
  mutate(arm = case_when(
    is.na(arm) ~ 0,
    arm == 3 ~ 1,
    TRUE ~ arm
  ))

health_2015 %>%
  tabyl(arm)


# 4. wrist
health_2015 <- health_2015 %>%
  rename(wrist = da042s4)

health_2015 <- health_2015 %>%
  mutate(wrist = case_when(
    is.na(wrist) ~ 0,
    wrist == 4 ~ 1,
    TRUE ~ wrist
  ))

health_2015 %>%
  tabyl(wrist)


# 5. fingers
health_2015 <- health_2015 %>%
  rename(fingers = da042s5)

health_2015 <- health_2015 %>%
  mutate(fingers = case_when(
    is.na(fingers) ~ 0,
    fingers == 5 ~ 1,
    TRUE ~ fingers
  ))

health_2015 %>%
  tabyl(fingers)


# 6. chest
health_2015 <- health_2015 %>%
  rename(chest = da042s6)

health_2015 <- health_2015 %>%
  mutate(chest = case_when(
    is.na(chest) ~ 0,
    chest == 6 ~ 1,
    TRUE ~ chest
  ))

health_2015 %>%
  tabyl(chest)


# 7. stomach
health_2015 <- health_2015 %>%
  rename(stomach = da042s7)

health_2015 <- health_2015 %>%
  mutate(stomach = case_when(
    is.na(stomach) ~ 0,
    stomach == 7 ~ 1,
    TRUE ~ stomach
  ))

health_2015 %>%
  tabyl(stomach)

# 8. back
health_2015 <- health_2015 %>%
  rename(back = da042s8)

health_2015 <- health_2015 %>%
  mutate(back = case_when(
    is.na(back) ~ 0,
    back == 8 ~ 1,
    TRUE ~ back
  ))

health_2015 %>%
  tabyl(back)


# 9. waist
health_2015 <- health_2015 %>%
  rename(waist = da042s9)

health_2015 <- health_2015 %>%
  mutate(waist = case_when(
    is.na(waist) ~ 0,
    waist == 9 ~ 1,
    TRUE ~ waist
  ))

health_2015 %>%
  tabyl(waist)

# 10. buttock
health_2015 <- health_2015 %>%
  rename(buttock = da042s10)

health_2015 <- health_2015 %>%
  mutate(buttock = case_when(
    is.na(buttock) ~ 0,
    buttock == 10 ~ 1,
    TRUE ~ buttock
  ))

health_2015 %>%
  tabyl(buttock)


# 11. leg
health_2015 <- health_2015 %>%
  rename(leg = da042s11)

health_2015 <- health_2015 %>%
  mutate(leg = case_when(
    is.na(leg) ~ 0,
    leg == 11 ~ 1,
    TRUE ~ leg
  ))

health_2015 %>%
  tabyl(leg)


# 12. knee
health_2015 <- health_2015 %>%
  rename(knee = da042s12)

health_2015 <- health_2015 %>%
  mutate(knee = case_when(
    is.na(knee) ~ 0,
    knee == 12 ~ 1,
    TRUE ~ knee
  ))

health_2015 %>%
  tabyl(knee)


# 13. ankle
health_2015 <- health_2015 %>%
  rename(ankle = da042s13)

health_2015 <- health_2015 %>%
  mutate(ankle = case_when(
    is.na(ankle) ~ 0,
    ankle == 13 ~ 1,
    TRUE ~ ankle
  ))

health_2015 %>%
  tabyl(ankle)


# 14. toes
health_2015 <- health_2015 %>%
  rename(toes = da042s14)

health_2015 <- health_2015 %>%
  mutate(toes = case_when(
    is.na(toes) ~ 0,
    toes == 14 ~ 1,
    TRUE ~ toes
  ))

health_2015 %>%
  tabyl(toes)

# 15. neck
health_2015 <- health_2015 %>%
  rename(neck = da042s15)

health_2015 <- health_2015 %>%
  mutate(neck = case_when(
    is.na(neck) ~ 0,
    neck == 15 ~ 1,
    TRUE ~ neck
  ))

health_2015 %>%
  tabyl(neck)


########### ------- Summarize for data in 2015
health_2015 %>%
  summarise(across(
    c(
      head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
      buttock, leg, knee, ankle, toes, neck
    ),
    \(x) mean(x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean_value") %>%
  arrange(desc(mean_value))

pain_2015 <- health_2015 %>%
  dplyr::select(
    ID, head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
    buttock, leg, knee, ankle, toes, neck, prevalence
  )

pain_2015 <- pain_2015 %>%
  mutate(pain_count = head + shoulder + arm + wrist + fingers + chest +
    stomach + back + waist + buttock + leg + knee + ankle + toes + neck)

pain_2015 <- pain_2015 %>%
  mutate(year = 2015)





######### -------------------- Pain in survey year 2018 -------------------------
health_2018 <- read_dta("health_2018.dta")
dim(health_2018)

###### ------- Outcome check
health_2018 %>%
  tabyl(da041_w4)

health_2018 <- health_2018 %>%
  filter(!is.na(da041_w4))

health_2018 <- health_2018 %>%
  mutate(prevalence = case_when(
    da041_w4 == 1 ~ 0,
    da041_w4 >= 2 & da041_w4 <= 5 ~ 1
  ))

health_2018 %>%
  tabyl(prevalence)


###### -------------------- Specific pain in survey year 2018 ------------------
### 1. head
health_2018 <- health_2018 %>%
  rename(head = da042_s1)

health_2018 <- health_2018 %>%
  mutate(head = case_when(
    is.na(head) ~ 0,
    head == 1 ~ 1,
    TRUE ~ head
  ))

health_2018 %>%
  tabyl(head)

### 2. shoulder
health_2018 <- health_2018 %>%
  rename(shoulder = da042_s2)

health_2018 <- health_2018 %>%
  mutate(shoulder = case_when(
    is.na(shoulder) ~ 0,
    shoulder == 2 ~ 1,
    TRUE ~ shoulder
  ))

health_2018 %>%
  tabyl(shoulder)


### 3. arm
health_2018 <- health_2018 %>%
  rename(arm = da042_s3)

health_2018 <- health_2018 %>%
  mutate(arm = case_when(
    is.na(arm) ~ 0,
    arm == 3 ~ 1,
    TRUE ~ arm
  ))

health_2018 %>%
  tabyl(arm)


### 4. wrist
health_2018 <- health_2018 %>%
  rename(wrist = da042_s4)

health_2018 <- health_2018 %>%
  mutate(wrist = case_when(
    is.na(wrist) ~ 0,
    wrist == 4 ~ 1,
    TRUE ~ wrist
  ))

health_2018 %>%
  tabyl(wrist)


### 5. fingers
health_2018 <- health_2018 %>%
  rename(fingers = da042_s5)

health_2018 <- health_2018 %>%
  mutate(fingers = case_when(
    is.na(fingers) ~ 0,
    fingers == 5 ~ 1,
    TRUE ~ fingers
  ))

health_2018 %>%
  tabyl(fingers)

### 6. chest
health_2018 <- health_2018 %>%
  rename(chest = da042_s6)

health_2018 <- health_2018 %>%
  mutate(chest = case_when(
    is.na(chest) ~ 0,
    chest == 6 ~ 1,
    TRUE ~ chest
  ))

health_2018 %>%
  tabyl(chest)


### 7. stomach
health_2018 <- health_2018 %>%
  rename(stomach = da042_s7)

health_2018 <- health_2018 %>%
  mutate(stomach = case_when(
    is.na(stomach) ~ 0,
    stomach == 7 ~ 1,
    TRUE ~ stomach
  ))

health_2018 %>%
  tabyl(stomach)


### 8. back
health_2018 <- health_2018 %>%
  rename(back = da042_s8)

health_2018 <- health_2018 %>%
  mutate(back = case_when(
    is.na(back) ~ 0,
    back == 8 ~ 1,
    TRUE ~ back
  ))

health_2018 %>%
  tabyl(back)

### 9. waist
health_2018 <- health_2018 %>%
  rename(waist = da042_s9)

health_2018 <- health_2018 %>%
  mutate(waist = case_when(
    is.na(waist) ~ 0,
    waist == 9 ~ 1,
    TRUE ~ waist
  ))

health_2018 %>%
  tabyl(waist)

### 10. buttock
health_2018 <- health_2018 %>%
  rename(buttock = da042_s10)

health_2018 <- health_2018 %>%
  mutate(buttock = case_when(
    is.na(buttock) ~ 0,
    buttock == 10 ~ 1,
    TRUE ~ buttock
  ))

health_2018 %>%
  tabyl(buttock)

### 11. leg
health_2018 <- health_2018 %>%
  rename(leg = da042_s11)

health_2018 <- health_2018 %>%
  mutate(leg = case_when(
    is.na(leg) ~ 0,
    leg == 11 ~ 1,
    TRUE ~ leg
  ))

health_2018 %>%
  tabyl(leg)

### 12. knee
health_2018 <- health_2018 %>%
  rename(knee = da042_s12)

health_2018 <- health_2018 %>%
  mutate(knee = case_when(
    is.na(knee) ~ 0,
    knee == 12 ~ 1,
    TRUE ~ knee
  ))

health_2018 %>%
  tabyl(knee)

### 13. ankle
health_2018 <- health_2018 %>%
  rename(ankle = da042_s13)

health_2018 <- health_2018 %>%
  mutate(ankle = case_when(
    is.na(ankle) ~ 0,
    ankle == 13 ~ 1,
    TRUE ~ ankle
  ))

health_2018 %>%
  tabyl(ankle)

### 14. toes
health_2018 <- health_2018 %>%
  rename(toes = da042_s14)

health_2018 <- health_2018 %>%
  mutate(toes = case_when(
    is.na(toes) ~ 0,
    toes == 14 ~ 1,
    TRUE ~ toes
  ))

health_2018 %>%
  tabyl(toes)

### 15. neck
health_2018 <- health_2018 %>%
  rename(neck = da042_s15)

health_2018 <- health_2018 %>%
  mutate(neck = case_when(
    is.na(neck) ~ 0,
    neck == 15 ~ 1,
    TRUE ~ neck
  ))

health_2018 %>%
  tabyl(neck)



########### ------- Summarize in year of 2018
health_2018 %>%
  summarise(across(
    c(
      head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
      buttock, leg, knee, ankle, toes, neck
    ),
    \(x) mean(x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean_value") %>%
  arrange(desc(mean_value))

pain_2018 <- health_2018 %>%
  dplyr::select(
    ID, head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
    buttock, leg, knee, ankle, toes, neck, prevalence
  )

pain_2018 <- pain_2018 %>%
  mutate(pain_count = head + shoulder + arm + wrist + fingers + chest +
    stomach + back + waist + buttock + leg + knee + ankle + toes + neck)

pain_2018 <- pain_2018 %>%
  mutate(year = 2018)




################### -------------- Survey year 2020 -------------------##########
health_2020 <- read_dta("health_2020.dta")
dim(health_2020)

#### outcome check
health_2020 %>%
  tabyl(da027)

health_2020 <- health_2020 %>%
  filter(!is.na(da027))

health_2020 <- health_2020 %>%
  mutate(prevalence = case_when(
    da027 == 1 ~ 0,
    da027 >= 2 & da027 <= 5 ~ 1
  ))

health_2020 %>%
  tabyl(prevalence)

## 1. head
health_2020 <- health_2020 %>%
  rename(head = da028_s1)

health_2020 <- health_2020 %>%
  mutate(head = case_when(
    is.na(head) ~ 0,
    head == 1 ~ 1,
    TRUE ~ head
  ))

health_2020 %>%
  tabyl(head)

## 2. shoulder
health_2020 <- health_2020 %>%
  rename(shoulder = da028_s2)

health_2020 <- health_2020 %>%
  mutate(shoulder = case_when(
    is.na(shoulder) ~ 0,
    shoulder == 2 ~ 1,
    TRUE ~ shoulder
  ))

health_2020 %>%
  tabyl(shoulder)

## 3. arm
health_2020 <- health_2020 %>%
  rename(arm = da028_s3)

health_2020 <- health_2020 %>%
  mutate(arm = case_when(
    is.na(arm) ~ 0,
    arm == 3 ~ 1,
    TRUE ~ arm
  ))

health_2020 %>%
  tabyl(arm)

## 4. wrist
health_2020 <- health_2020 %>%
  rename(wrist = da028_s4)

health_2020 <- health_2020 %>%
  mutate(wrist = case_when(
    is.na(wrist) ~ 0,
    wrist == 4 ~ 1,
    TRUE ~ wrist
  ))

health_2020 %>%
  tabyl(wrist)


## 5. fingers
health_2020 <- health_2020 %>%
  rename(fingers = da028_s5)

health_2020 <- health_2020 %>%
  mutate(fingers = case_when(
    is.na(fingers) ~ 0,
    fingers == 5 ~ 1,
    TRUE ~ fingers
  ))

health_2020 %>%
  tabyl(fingers)


## 6. chest
health_2020 <- health_2020 %>%
  rename(chest = da028_s6)

health_2020 <- health_2020 %>%
  mutate(chest = case_when(
    is.na(chest) ~ 0,
    chest == 6 ~ 1,
    TRUE ~ chest
  ))

health_2020 %>%
  tabyl(chest)

## 7. stomach
health_2020 <- health_2020 %>%
  rename(stomach = da028_s7)

health_2020 <- health_2020 %>%
  mutate(stomach = case_when(
    is.na(stomach) ~ 0,
    stomach == 7 ~ 1,
    TRUE ~ stomach
  ))

health_2020 %>%
  tabyl(stomach)

## 8. back
health_2020 <- health_2020 %>%
  rename(back = da028_s8)

health_2020 <- health_2020 %>%
  mutate(back = case_when(
    is.na(back) ~ 0,
    back == 8 ~ 1,
    TRUE ~ back
  ))

health_2020 %>%
  tabyl(back)

## 9. waist
health_2020 <- health_2020 %>%
  rename(waist = da028_s9)

health_2020 <- health_2020 %>%
  mutate(waist = case_when(
    is.na(waist) ~ 0,
    waist == 9 ~ 1,
    TRUE ~ waist
  ))

health_2020 %>%
  tabyl(waist)

## 10. buttock
health_2020 <- health_2020 %>%
  rename(buttock = da028_s10)

health_2020 <- health_2020 %>%
  mutate(buttock = case_when(
    is.na(buttock) ~ 0,
    buttock == 10 ~ 1,
    TRUE ~ buttock
  ))

health_2020 %>%
  tabyl(buttock)

## 11. leg
health_2020 <- health_2020 %>%
  rename(leg = da028_s11)

health_2020 <- health_2020 %>%
  mutate(leg = case_when(
    is.na(leg) ~ 0,
    leg == 11 ~ 1,
    TRUE ~ leg
  ))

health_2020 %>%
  tabyl(leg)


## 12. knee
health_2020 <- health_2020 %>%
  rename(knee = da028_s12)

health_2020 <- health_2020 %>%
  mutate(knee = case_when(
    is.na(knee) ~ 0,
    knee == 12 ~ 1,
    TRUE ~ knee
  ))

health_2020 %>%
  tabyl(knee)



## 13. ankle
health_2020 <- health_2020 %>%
  rename(ankle = da028_s13)

health_2020 <- health_2020 %>%
  mutate(ankle = case_when(
    is.na(ankle) ~ 0,
    ankle == 13 ~ 1,
    TRUE ~ ankle
  ))

health_2020 %>%
  tabyl(ankle)


## 14. toes
health_2020 <- health_2020 %>%
  rename(toes = da028_s14)

health_2020 <- health_2020 %>%
  mutate(toes = case_when(
    is.na(toes) ~ 0,
    toes == 14 ~ 1,
    TRUE ~ toes
  ))

health_2020 %>%
  tabyl(toes)

## 15. neck
health_2020 <- health_2020 %>%
  rename(neck = da028_s15)

health_2020 <- health_2020 %>%
  mutate(neck = case_when(
    is.na(neck) ~ 0,
    neck == 15 ~ 1,
    TRUE ~ neck
  ))

health_2020 %>%
  tabyl(neck)


###### ------------------ Summarize for year 2020
health_2020 %>%
  summarise(across(
    c(
      head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
      buttock, leg, knee, ankle, toes, neck
    ),
    \(x) mean(x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean_value") %>%
  arrange(desc(mean_value))

pain_2020 <- health_2020 %>%
  dplyr::select(
    ID, head, shoulder, arm, wrist, fingers, chest, stomach, back, waist,
    buttock, leg, knee, ankle, toes, neck, prevalence
  )

pain_2020 <- pain_2020 %>%
  mutate(pain_count = head + shoulder + arm + wrist + fingers + chest +
    stomach + back + waist + buttock + leg + knee + ankle + toes + neck)

pain_2020 <- pain_2020 %>%
  mutate(year = 2020)



###### Combing the above data --------------------------------------------------
pain_com <- bind_rows(pain_2011, pain_2013, pain_2015, pain_2018, pain_2020)

pain_com <- pain_com %>%
  arrange(ID, year)


###### output ------------------------------------------------------------------
write_dta(pain_com, "pain_com.dta")








### chapter 2, 3rd revision & resubmt
library(ggplot2)

pain_com <- read_dta("pain_com.dta")

# 1. Body pain prevalence at every wave
pain_prevalence <- pain_com %>%
  group_by(year) %>%
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
  pivot_longer(cols = -year, names_to = "body_part", values_to = "prevalence")


ggplot(pain_prevalence, aes(x = year, y = prevalence, group = body_part, color = body_part)) +
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


# Order
pain_prevalence <- pain_prevalence %>%
  group_by(body_part) %>%
  mutate(max_prevalence = max(prevalence)) %>%
  ungroup() %>%
  mutate(body_part = reorder(body_part, -max_prevalence)) %>%
  mutate(prevalence = prevalence * 100)

# plot
ggplot(pain_prevalence, aes(x = year, y = prevalence, group = body_part, color = body_part)) +
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



# pain_count
mean_pain_count <- pain_com %>%
  group_by(year) %>%
  summarise(mean_pain_count = mean(pain_count, na.rm = TRUE))

ggplot(data = mean_pain_count, aes(x = year, y = mean_pain_count)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", size = 2) +
  geom_text(aes(label = round(mean_pain_count, 2)), # 添加具体数值
    vjust = -0.5, # 调整垂直位置
    size = 5, # 调整字体大小
    color = "black"
  ) +
  scale_x_continuous(
    breaks = c(2011, 2013, 2015, 2018, 2020),
    limits = c(2011, 2020)
  ) +
  labs(
    title = "Pain Count over Survey Years",
    x = "Year",
    y = "Pain Count"
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


# ------------------------------------------------------------------------------





