library(tidyverse)
library(janitor)
library(readxl)
library(corrplot)


#Read files and Reverse Score items
df <- read_xlsx("CEMA_Year2_Data_DeID_1July21.xlsx") %>% 
  clean_names() %>% 
  mutate_at(vars(mlsq33, mlsq57, mlsq52, mlsq80, mlsq37, mlsq60, mlsq40), ~(8-.x)) #reverse score select items

#Compute summary scores by sub-scale or facet
df_score <- df %>% 
  group_by(part_id) %>% 
  mutate(intrinsic_goal_orientation = mean(c(mlsq1, mlsq16, mlsq22, mlsq24), na.rm = TRUE), #facet/scale scores
         extrinsic_goal_orientation = mean(c(mlsq7, mlsq11, mlsq13, mlsq30), na.rm = TRUE),
         task_value = mean(c(mlsq4, mlsq10, mlsq17, mlsq23, mlsq26, mlsq27), na.rm = TRUE),
         control_of_learning = mean(c(mlsq2, mlsq9, mlsq18, mlsq25), na.rm = TRUE),
         selfefficacy_for_learning_and_performance = mean(c(mlsq5, mlsq6, mlsq12, mlsq15, mlsq20, mlsq21, mlsq29, mlsq31), na.rm = TRUE),
         test_anxiety = mean(c(mlsq3, mlsq8, mlsq14, mlsq19, mlsq28), na.rm = TRUE),
         rehearsal = mean(c(mlsq39, mlsq46, mlsq59, mlsq72), na.rm = TRUE),
         elaboration = mean(c(mlsq53, mlsq62, mlsq64, mlsq67, mlsq69, mlsq81), na.rm = TRUE),
         organization = mean(c(mlsq32, mlsq42, mlsq49, mlsq63), na.rm = TRUE),
         critical_thinking = mean(c(mlsq38, mlsq47, mlsq51, mlsq66, mlsq71), na.rm = TRUE),
         metacognitive_selfregulation = mean(c(mlsq33, mlsq36, mlsq41, mlsq44, mlsq54, mlsq55, mlsq56, mlsq57, mlsq61, mlsq76, mlsq78, mlsq79), na.rm = TRUE),
         time_study_environment = mean(c(mlsq35, mlsq43, mlsq52, mlsq65, mlsq70, mlsq73, mlsq77, mlsq80), na.rm = TRUE),
         effort_regulation = mean(c(mlsq37, mlsq48, mlsq60, mlsq74), na.rm = TRUE),
         peer_learning = mean(c(mlsq34, mlsq45, mlsq50), na.rm = TRUE),
         help_seeking = mean(c(mlsq40, mlsq58, mlsq68, mlsq75), na.rm = TRUE)
         ) %>% 
    left_join(
      df %>%   
        select(part_id, mlsq1:mlsq31) %>% #category score for motivation
        gather(mlsq1:mlsq31, key=question, value=score) %>% 
        group_by(part_id) %>% 
        summarise(motivation = mean(score, na.rm = TRUE))
    ) %>% 
    left_join(
      df %>%   
        select(part_id, mlsq32:mlsq81) %>% #category score for learningstrategy
        gather(mlsq32:mlsq81, key=question, value=score) %>% 
        group_by(part_id) %>% 
        summarise(learningstrategy = mean(score, na.rm = TRUE))
    )

#Visualization: boxplot distributions by sub-scale
df_score %>% 
  gather(intrinsic_goal_orientation:learningstrategy, key=measure, value=score) %>% 
  mutate(category = 
         if_else(measure %in% c("intrinsic_goal_orientation", "extrinsic_goal_orientation", "task_value"), 'value comp', 
        if_else(measure %in% c('control_of_learning', 'selfefficacy_for_learning_and_performance'), "expectancy comp",
        if_else(measure %in% c('test_anxiety'), "affective comp",
        if_else(measure %in% c('rehearsal', 'elaboration', 'organization', 'critical_thinking', 'metacognitive_selfregulation'), "cognitive strategies", 
        if_else(measure %in% c('time_study_environment', 'effort_regulation', 'peer_learning', 'help_seeking'), "resource management strategies", "overall")))))) %>% 
  filter(category!="overall") %>% 
  mutate(category2 = if_else(category %in% c("value comp", "expectancy comp", "affective comp"), "motivation", "learning strategy")) %>% 
  ggplot(aes(x=reorder(measure, score, fun=mean), y=score)) + 
  geom_boxplot(aes(fill=category2)) +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual(name = "category", values = c("skyblue", "lightgreen")) + 
  xlab("")

#Visualization: correlation plot of sub-scales
corrplot(cor(df_score %>% ungroup %>% select(intrinsic_goal_orientation:help_seeking)), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), 
addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=1 , number.cex=1, tl.cex=.6)

#Results table
df_score %>% 
  gather(intrinsic_goal_orientation:learningstrategy, key=measure, value=score) %>% 
  mutate(category = 
           if_else(measure %in% c("intrinsic_goal_orientation", "extrinsic_goal_orientation", "task_value"), 'value comp', 
           if_else(measure %in% c('control_of_learning', 'selfefficacy_for_learning_and_performance'), "expectancy comp",
           if_else(measure %in% c('test_anxiety'), "affective comp",
           if_else(measure %in% c('rehearsal', 'elaboration', 'organization', 'critical_thinking', 'metacognitive_selfregulation'), "cognitive strategies", 
           if_else(measure %in% c('time_study_environment', 'effort_regulation', 'peer_learning', 'help_seeking'), "resource management strategies", "overall")))))) %>% 
filter(category!="overall") %>% 
  mutate(category2 = if_else(category %in% c("value comp", "expectancy comp", "affective comp"), "motivation scales", "learning strategy scales")) %>% 
  group_by(category2, category, measure) %>% 
  summarise(mean = round(mean(score), 2), sd = round(sd(score), 2)) %>%  
  arrange(category2, category, -mean) %>% 
  write.csv("scale_results.csv")

#Visualization of sub-scale mean item scores and standard deviations
df_score %>% 
  gather(intrinsic_goal_orientation:learningstrategy, key=measure, value=score) %>% 
  mutate(category = 
           if_else(measure %in% c("intrinsic_goal_orientation", "extrinsic_goal_orientation", "task_value"), 'value comp', 
                   if_else(measure %in% c('control_of_learning', 'selfefficacy_for_learning_and_performance'), "expectancy comp",
                           if_else(measure %in% c('test_anxiety'), "affective comp",
                                   if_else(measure %in% c('rehearsal', 'elaboration', 'organization', 'critical_thinking', 'metacognitive_selfregulation'), "cognitive strategies", 
                                           if_else(measure %in% c('time_study_environment', 'effort_regulation', 'peer_learning', 'help_seeking'), "resource management strategies", "overall")))))) %>% 
  filter(category!="overall") %>% 
  mutate(category2 = if_else(category %in% c("value comp", "expectancy comp", "affective comp"), "motivation scales", "learning strategy scales")) %>% 
  group_by(category2, category, measure) %>% 
  summarise(mean = round(mean(score), 2), sd = round(sd(score), 2)) %>% 
  ggplot(aes(x=mean, y=sd, color=category2, label=measure)) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(size = 3)+
  theme(legend.position = "top") 

#item analysis
df_score %>% 
  gather(mlsq1:mlsq81, key=item, value = score) %>% 
  mutate(reverse = if_else(item  %in% c('mlsq33', 'mlsq57', 'mlsq52', 'mlsq80', 'mlsq37', 'mlsq60', 'mlsq40'), "yes", "no")) %>% 
  group_by(item, reverse) %>% 
  summarise(mean = mean(score, na.rm = TRUE), sd = sd(score, na.rm = TRUE)) %>% 
  separate(item, into = c("item1", "item"), sep = "q") %>% 
  select(-item1) %>% 
  ggplot(aes(x=mean, y=sd, color=reverse)) +
  geom_jitter(size=.1) +
  geom_text(aes(label=item), size = 3.5) +
  scale_color_manual(values=c("darkgray", "red")) +
  theme(legend.position = "blank") +
  labs(caption = "Note: reverse scored items in red font")

