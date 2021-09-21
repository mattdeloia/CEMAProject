library(tidyverse)
library(janitor)
library(readxl)
library(corrplot)
library(superheat)

#Read files and Reverse Score items
df <- read_xlsx("CEMA_Year2_Data_DeID_1July21.xlsx") %>% 
  clean_names() %>% 
  mutate_at(vars(mslq33, mslq57, mslq52, mslq80, mslq37, mslq60, mslq40), ~(8-.x)) #reverse score select items

#Compute summary scores by sub-scale or facet
df_score <- df %>% 
  group_by(part_id) %>% 
  mutate(intrinsic_goal_orientation = mean(c(mslq1, mslq16, mslq22, mslq24), na.rm = TRUE), #facet/scale scores
         extrinsic_goal_orientation = mean(c(mslq7, mslq11, mslq13, mslq30), na.rm = TRUE),
         task_value = mean(c(mslq4, mslq10, mslq17, mslq23, mslq26, mslq27), na.rm = TRUE),
         control_of_learning = mean(c(mslq2, mslq9, mslq18, mslq25), na.rm = TRUE),
         selfefficacy_for_learning_and_performance = mean(c(mslq5, mslq6, mslq12, mslq15, mslq20, mslq21, mslq29, mslq31), na.rm = TRUE),
         test_anxiety = mean(c(mslq3, mslq8, mslq14, mslq19, mslq28), na.rm = TRUE),
         rehearsal = mean(c(mslq39, mslq46, mslq59, mslq72), na.rm = TRUE),
         elaboration = mean(c(mslq53, mslq62, mslq64, mslq67, mslq69, mslq81), na.rm = TRUE),
         organization = mean(c(mslq32, mslq42, mslq49, mslq63), na.rm = TRUE),
         critical_thinking = mean(c(mslq38, mslq47, mslq51, mslq66, mslq71), na.rm = TRUE),
         metacognitive_selfregulation = mean(c(mslq33, mslq36, mslq41, mslq44, mslq54, mslq55, mslq56, mslq57, mslq61, mslq76, mslq78, mslq79), na.rm = TRUE),
         time_study_environment = mean(c(mslq35, mslq43, mslq52, mslq65, mslq70, mslq73, mslq77, mslq80), na.rm = TRUE),
         effort_regulation = mean(c(mslq37, mslq48, mslq60, mslq74), na.rm = TRUE),
         peer_learning = mean(c(mslq34, mslq45, mslq50), na.rm = TRUE),
         help_seeking = mean(c(mslq40, mslq58, mslq68, mslq75), na.rm = TRUE)
         ) %>% 
    left_join(
      df %>%   
        select(part_id, mslq1:mslq31) %>% #category score for motivation
        gather(mslq1:mslq31, key=question, value=score) %>% 
        group_by(part_id) %>% 
        summarise(motivation = mean(score, na.rm = TRUE))
    ) %>% 
    left_join(
      df %>%   
        select(part_id, mslq32:mslq81) %>% #category score for learningstrategy
        gather(mslq32:mslq81, key=question, value=score) %>% 
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
  gather(mslq1:mslq81, key=item, value = score) %>% 
  mutate(reverse = if_else(item  %in% c('mslq33', 'mslq57', 'mslq52', 'mslq80', 'mslq37', 'mslq60', 'mslq40'), "yes", "no")) %>% 
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

df_heatmap <- df_score  %>%
  select(part_id, intrinsic_goal_orientation:help_seeking) %>% 
  column_to_rownames("part_id") %>% 
  mutate_all(scale) 

df_heatmap_t <- t(df_heatmap)

heatmap_plot <-  df_heatmap %>% 
  as.matrix()  %>% 
  superheat(title = "mslq Results (scaled scores)",
            pretty.order.cols = TRUE,
            pretty.order.rows = TRUE,
            row.dendrogram = FALSE,
            col.dendrogram = TRUE,
           yr=pnorm(rowMeans(df_heatmap ))*100,
           order.rows = order(rowMeans(df_heatmap )),
           yr.axis.name = "ave. trait score percentile",
            yr.plot.type = "bar",
         
            yr.num.ticks = 5, 
            heat.pal = c("red", "red", "white", "white", "green", "green"),
            heat.lim = c(-3,3),
            heat.pal.values = c(0,.3, .35, .65,.7,1),
            X.text = as.matrix(round(df_heatmap,1)),
            X.text.size = 3, 
            bottom.label.text.angle = 90,
            bottom.label.text.size = 3.5, 
            bottom.label.size = .6,
           bottom.label.text.alignment = "center",
            left.label.text.size = 2, 
            left.label.size = .1,
            title.size = 3)

ggsave("heat.pdf", plot = heatmap_plot$plot, width = 8.5, height = 11, units = "in")
  

heatmap_plot_t <-  df_heatmap_t %>% 
  as.matrix()  %>% 
  superheat(title = "mslq Results (scaled scores)",
            pretty.order.cols = TRUE,
            pretty.order.rows = TRUE,
            row.dendrogram = TRUE,
            col.dendrogram = TRUE,
            # yt =pnorm(colMeans(df_heatmap_t ))*100,
            # #order.cols =  order(colMeans(df_heatmap_t )),
            # yt.axis.name = "ave. pNorm",
            # yt.plot.type = "bar",
            # yt.num.ticks = 5, 
            heat.pal = c("red", "red", "white", "white", "green", "green"),
            heat.lim = c(-3,3),
            heat.pal.values = c(0,.3, .35, .65,.7,1),
            X.text = as.matrix(round(df_heatmap_t,1)),
            X.text.size = 2.5, 
            bottom.label.text.angle = 90,
            bottom.label.text.size = 2, 
            bottom.label.size = .1,
            bottom.label.text.alignment = "center",
            left.label.text.size = 3, 
            left.label.size = .4,
            title.size = 3)

ggsave("heat_3.pdf", plot = heatmap_plot_t$plot, width = 11, height = 8.5, units = "in")
