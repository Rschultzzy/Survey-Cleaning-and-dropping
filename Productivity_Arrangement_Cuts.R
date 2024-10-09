#load necessary libraries
library(RODBC)
library(tidyverse)
library(psych)
library(dplyr)
library(tidyr)

#Connect to server
dbsu <- odbcConnect("surveys")

Variable <-sqlTables(dbsu)
# view(Variable)

Hours1 <-sqlFetch(dbsu, "wkload24_q1q2.demo") %>%
  filter(valid == 1) #989 Cases (Respondents)
Hours2 <-sqlFetch(dbsu, "wkload24_q1q2.question_meta")
Hours3 <-sqlFetch(dbsu, "wkload24_q1q2.question_choices")
Hours4 <-sqlFetch(dbsu, "wkload24_q1q2.resp_text")
Hours5 <-sqlFetch(dbsu, "wkload24_q1q2.response") 

Hours1 <-subset(Hours1, rid != "R_6rlCh5WMjVbvEcl") #drop one case
Hours1 <-subset(Hours1, rid != "R_3D7qqPX4iwMOX69") #drop one case

# Create DF with all well necessary question data (All questions here are single select)
Productivity <-sqlFetch(dbsu,"wkload24_q1q2.response") %>%
  filter(rid %in% Hours1$rid) %>%
  mutate(
    response = as.numeric(response)
  ) %>%
  filter(question %in% c("QID45", "QID66", "QID26_1", "QID26_2", "QID29_1", "QID29_2")) %>%
  pivot_wider(names_from = question, values_from = response) %>%
  group_by(rid) %>%
  ungroup() #n = 969

Productivity <-subset(Productivity, QID26_1 >=1) #961 observations left
Productivity <-subset(Productivity, QID26_2 >=1) #960 observations

#Add column with hours billed total and hours worked total
Productivity <- Productivity %>%
  mutate(Billed = QID29_1 + QID29_2)

Productivity <- Productivity %>%
  mutate(Worked = QID26_1 + QID26_2)
  
#Create a column to flag where Billed is > than HRs worked
Productivity$Result <-ifelse(Productivity$Billed > Productivity$Worked, "Billed is greater", "Billed is less than")
view(Productivity)

table(Productivity$Result)
#22 Billed hours reported is greater than hours worked reported
#546 Billed hours reported is less than hours worked reported

#Drop cases where Billed is greater than hours worked
Productivity <-subset(Productivity, Result != "Billed is greater")
#546 Cases left

#Calculate mean hours worked - grouped by Age x Working Arrangement
hours_worked <-Productivity %>%
  group_by(QID45, QID66) %>%
  summarise(mean_sum = mean(QID26_1 + QID26_2)/2)

base <- Productivity %>%
  group_by(QID45, QID66) %>%
  summarise(distinct_respondents = n_distinct(rid))

#Calculate mean hours billed - grouped by Age x Working Arrangement
# Productivity2 <-na.omit(Productivity)

Productivity <-subset(Productivity, QID29_1 >=1) #569 observations
Productivity <-subset(Productivity, QID29_2 >=1) #568 observations

billed_hours <-Productivity %>%
  group_by(QID45, QID66) %>%
  summarise(mean_sum = mean(QID29_1 + QID29_2)/2)

base2 <- Productivity %>%
  group_by(QID45, QID66) %>%
  summarise(distinct_respondents = n_distinct(rid))





## ----- DO NOT RUN BELOW -------

# hours_billed <-Productivity %>%
#   group_by(QID45, QID66) %>%
#   summarise(mean_sum = mean((QID29_1 + QID29_2)/2), na.rm = TRUE)
# 
# hours_billed <-Productivity %>%
#   mutate(sum_col = QID29_1 + QID26_2) %>%
#   group_by(QID45, QID66) %>%
#   summarise(mean_sum = mean(sum_col), na.rm = TRUE)