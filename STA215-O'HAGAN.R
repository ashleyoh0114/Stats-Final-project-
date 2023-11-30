## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE QUANT_VAR1
table(data$Number_of_Income_Inequality)
mean(data$Number_of_Income_Inequality)
sd(data$Number_of_Income_Inequality)
summary(data$Number_of_Income_Inequality)

# EXAMINE QUANT_VAR2
table(data$Number_of_Addiction)
mean(data$Number_of_Addiction)
sd(data$Number_of_Addiction)

# EXAMINE QUAL_VAR1
table(data$Conversation_Topic)

# EXAMINE QUAL_VAR2
table(data$Gender_Leading_Conversation)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Conversation_Topic,data$Gender_Leading_Conversation)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Conversation_Topic,data$Gender_Leading_Conversation))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(Number_of_Income_Inequality ~ Conversation_Topic, data = data)
summary(anova_adapted)
anova_adapted <- aov(Number_of_Addiction ~ Gender_Leading_Conversation, data = data)
summary(anova_adapted)
anova_adapted <- aov(Number_of_Income_Inequality ~ Gender_Leading_Conversation, data = data)
summary(anova_adapted)
anova_adapted <- aov(Number_of_Addiction ~ Conversation_Topic, data = data)
summary(anova_adapted)


##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Number_of_Income_Inequality, data$Number_of_Addiction)


##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(data$Number_of_Addiction ~ data$Number_of_Income_Inequality, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(data$Number_of_Income_Inequality, data$Number_of_Addiction)
print(linear_plot)
mean(data$Number_of_Income_Inequality)
mean(data$Number_of_Addiction)
abline(linear_relationship, col = "red")
abline(a=NULL, b=NULL, h=1.25, v=0.4807692, col = "blue")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Number_of_Income_Inequality, residuals(linear_relationship))
abline(h = 0, col = "red")
