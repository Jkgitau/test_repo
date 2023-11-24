library(tidyverse) #load tidyverse library
library(ggplot2)

rm(list=ls()) #clean the environment

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#'this file is saved as current directory;
#'all paths are now expressed relative to the current directory

clean_routine_data <- read.csv('./clean_routine_data.csv') 

clean_routine_data_Admin1 <- clean_routine_data %>%
  group_by(Admin1,month) %>%
  summarise(across(c(test_u5:conf_ov5),sum))

### repartition of cases by age

##### at the municipality level

cases_age <- clean_routine_data_Admin1 %>%
  group_by(Admin1) %>%
  summarise(across(c(conf_u5,conf_ov5),sum)) %>%
  pivot_longer(cols=c(conf_u5,conf_ov5),
               names_to = "age_group",values_to = "conf",names_prefix = "conf_")

ggplot(cases_age,
       aes(x=Admin1,y=conf,fill=age_group))+
  geom_col()

##### at the national level

cases_age_national <- cases_age %>%
  ungroup() %>%
  group_by(age_group) %>%
  summarise(conf=sum(conf))

ggplot(cases_age_national,
       aes(x="Greenland",y=conf,fill=age_group))+
  geom_col()+
  scale_y_continuous(labels = function(l) {trans = l / 1000})

### test positivity rate

TPR <- clean_routine_data_Admin1 %>%
  mutate(TPR_u5=conf_u5/test_u5,
         TPR_ov5=conf_ov5/test_ov5,
         month = factor(month,
                        levels = month.abb)) %>%
  select(Admin1,month,starts_with("TPR")) %>%
  pivot_longer(cols=starts_with("TPR"),
               names_to = "age_group",names_prefix = "TPR_",values_to = "TPR")

ggplot(TPR,
       aes(x=month,y=TPR,color=age_group,group=age_group))+
  geom_line()+
  facet_wrap(Admin1~.)

### seasonality of incidence

##### load population data
pop <- read.csv('./population.csv',sep=";")

unique(pop$Admin1)[!unique(pop$Admin1) %in% clean_routine_data_Admin1$Admin1]

pop$Admin1[pop$Admin1=="Qaasuitsup"]<-"Qasuitsup"

##### by month

incidence <- clean_routine_data_Admin1 %>%
  left_join(pop) %>%
  mutate(pop_total=pop_u5+pop_ov5,
         test_total=test_u5+test_ov5,
         conf_total=conf_u5+test_ov5,
         month = factor(month,
                        levels = month.abb)) %>%
  mutate(inc_conf=conf_total/pop_total)

ggplot(incidence,
       aes(x=month,y=inc_conf,group=Admin1))+
  geom_line()+
  facet_wrap(Admin1~.)

##### in 2018

incidence2018 <- incidence %>%
  group_by(Admin1,pop_total) %>%
  summarise(conf_total=sum(conf_total)) %>%
  mutate(inc_conf=conf_total/pop_total)

ggplot(incidence,
       aes(x=Admin1,y=inc_conf))+
  geom_col()
