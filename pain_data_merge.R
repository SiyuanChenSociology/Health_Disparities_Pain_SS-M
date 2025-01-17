
setwd("/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 2/DA_github_SS&M/DA_github_SS&M")
dir()

### related packages -----------------------------------------------------------
library(pacman)
pacman::p_load(
  haven, ggplot2, dplyr, expss, scales, ggeffects,
  geepack, MASS, marginaleffects
)

library(dplyr)

### Data import ----------------------------------------------------------------
data1 <- read_dta("Aug_3rd.dta")
glimpse(data1)

pain_com <- read_dta("pain_com.dta")


# check id type
class(data1$id)
class(pain_com$ID)
pain_com$ID <- as.numeric(pain_com$ID)

colnames(pain_com)[colnames(pain_com) == "ID"] <- "id"

colnames(pain_com)[colnames(pain_com) == "year"] <- "wave"



# Merge, based on id & wave
data <- inner_join(data1, pain_com, by = c("id", "wave"))
write_dta(data, "pain.dta")






