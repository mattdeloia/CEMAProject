library(tidyverse)
library(janitor)
library(readxl)
library(corrplot)
library(superheat)
library(visdat)
library(rstatix)

#Read files and Reverse Score items
df <- read_xlsx("CEMA_Year2_Data_DeID_Master_26Jan22.xlsx") %>% 
  clean_names() %>% 
  mutate_at(vars(mslq33, mslq57, mslq52, mslq77, mslq80, mslq37, mslq60, mslq40), ~(8-.x)) %>% #reverse score select items
  mutate_at(vars(mos_transfer, college_degree, advanced_degree, experience), tolower) %>% 
  mutate(part_id = as.character(part_id)) %>% 
  mutate_at(vars(mos_transfer, college_degree, advanced_degree, experience), ~if_else(.x=="yes", 1, 0)) %>% 
  mutate_at(vars(college_years, experience_years, experience, mos_transfer, college_degree, advanced_degree, experience), ~replace_na(.x, 0)) %>% 
  select(part_id, rank:cert5, everything(), -c(x98:x103))

vis_dat(df)

#Compute summary scores by sub-scale or facet
df_score <- df %>% 
  group_by(part_id) %>% 
  mutate(Intrinsic_Goal_Orientation = mean(c(mslq1, mslq16, mslq22, mslq24), na.rm = TRUE), #facet/scale scores
         Extrinsic_Goal_Orientation = mean(c(mslq7, mslq11, mslq13, mslq30), na.rm = TRUE),
         Task_Value = mean(c(mslq4, mslq10, mslq17, mslq23, mslq26, mslq27), na.rm = TRUE),
         Control_Beliefs_about_Learning = mean(c(mslq2, mslq9, mslq18, mslq25), na.rm = TRUE),
         Selfefficacy_for_Learning_and_Performance = mean(c(mslq5, mslq6, mslq12, mslq15, mslq20, mslq21, mslq29, mslq31), na.rm = TRUE),
         Test_Anxiety = mean(c(mslq3, mslq8, mslq14, mslq19, mslq28), na.rm = TRUE),
         Rehearsal = mean(c(mslq39, mslq46, mslq59, mslq72), na.rm = TRUE),
         Elaboration = mean(c(mslq53, mslq62, mslq64, mslq67, mslq69, mslq81), na.rm = TRUE),
         Organization = mean(c(mslq32, mslq42, mslq49, mslq63), na.rm = TRUE),
         Critical_Thinking = mean(c(mslq38, mslq47, mslq51, mslq66, mslq71), na.rm = TRUE),
         Metacognitive_Selfregulation = mean(c(mslq33, mslq36, mslq41, mslq44, mslq54, mslq55, mslq56, mslq57, mslq61, mslq76, mslq78, mslq79), na.rm = TRUE),
         Time_Study_Environment = mean(c(mslq35, mslq43, mslq52, mslq65, mslq70, mslq73, mslq77, mslq80), na.rm = TRUE),
         Effort_Regulation = mean(c(mslq37, mslq48, mslq60, mslq74), na.rm = TRUE),
         Peer_Learning = mean(c(mslq34, mslq45, mslq50), na.rm = TRUE),
         Help_Seeking = mean(c(mslq40, mslq58, mslq68, mslq75), na.rm = TRUE)
         ) %>% 
  ungroup()

write_rds(df_score, "MSLQ_scored.rds")

#Results table
df_score %>% 
  gather(Intrinsic_Goal_Orientation:Help_Seeking, key=measure, value=score) %>% 
  group_by(measure) %>% 
  summarise(mean = round(mean(score, na.rm = TRUE), 2), sd = round(sd(score, na.rm = TRUE), 2)) %>%  
  write_rds("scale_results.rds")

#Comparison of MOS transfers versus others

df_score %>% 
  gather(Intrinsic_Goal_Orientation:Help_Seeking, key=measure, value=score) %>% 
  group_by(measure, mos_transfer) %>% 
  summarise(mean = round(mean(score, na.rm = TRUE), 2), sd = round(sd(score, na.rm = TRUE), 2)) 

student_ttest <- df_score %>% 
  gather(Intrinsic_Goal_Orientation:Help_Seeking, key=measure, value=score) %>% 
   group_by(measure) %>% 
   t_test(score~mos_transfer)

df_score %>% 
  gather(Intrinsic_Goal_Orientation:Help_Seeking, key=measure, value=score) %>% 
  group_by(measure, mos_transfer) %>% 
  summarise(mean = round(mean(score, na.rm = TRUE), 2)) %>% 
  pivot_wider(names_from="mos_transfer", values_from = "mean") %>% 
  left_join(student_ttest) %>% 
  rename("yes"=`1`, "no"=`0`)

df_score2 <- df_score %>% 
  gather(Intrinsic_Goal_Orientation:Help_Seeking, key=measure, value=unscaled) %>% 
  mutate(component = 
           if_else(measure %in% c("Intrinsic_Goal_Orientation", "Extrinsic_Goal_Orientation", "Task_Value"), 'Value', 
                   if_else(measure %in% c('Control_Beliefs_about_Learning', 'Selfefficacy_for_Learning_and_Performance'), "Expectancy",
                           if_else(measure %in% c("Test_Anxiety"), "Affective",
                                   if_else(measure %in% c('Rehearsal', 'Elaboration', 'Organization', 'Critical_Thinking', 'Metacognitive_Selfregulation'), "Cognitive Strategies", 
                                           if_else(measure %in% c('Time_Study_Environment', 'Effort_Regulation', 'Peer_Learning', 'Help_Seeking'), "Resource Management Strategies", "overall")))))) %>%  
  mutate(category = if_else(component %in% c("Value", "Expectancy", "Affective"), "Motivation", "Strategies for Learning")) %>% 
  ungroup() %>% 
  left_join(
    df_score %>% select(part_id, Intrinsic_Goal_Orientation:Help_Seeking) %>% 
      mutate_if(is.numeric, scale) %>% 
      gather(Intrinsic_Goal_Orientation:Help_Seeking, key=measure, value=scaled)) %>% 
  select(-(mslq1:mslq81)) 
  

df_score2 %>% ggplot(aes(x=unscaled)) + geom_density() + facet_wrap(measure~.)

write_rds(df_score2, "MSLQ_scored2.rds")

#Visualization: boxplot distributions by sub-scale
df_score2  %>% filter(method=="unscaled") %>% 
  drop_na(score) %>% 
  ggplot(aes(x=reorder(measure, score, FUN=median), y=score)) + 
  geom_boxplot(aes(fill=category)) +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual(name = "category", values = c("skyblue", "lightgreen")) + 
  xlab("")

#Visualization: correlation plot of sub-scales
corrplot(cor(df_score %>% ungroup %>% select(Intrinsic_Goal_Orientation:Help_Seeking) %>% drop_na()), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), 
addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=1 , number.cex=1, tl.cex=.6)

#Visualization of sub-scale mean item scores and standard deviations
df_score2 %>% filter(method=="unscaled") %>% 
  group_by(category, component, measure) %>% 
  summarise(mean = round(mean(score, na.rm = TRUE), 2), sd = round(sd(score, na.rm = TRUE), 2)) %>% 
  ggplot(aes(x=mean, y=sd, color=category, label=measure)) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(size = 3)+
  theme(legend.position = "top") 

#item analysis
df_score %>% 
  gather(mslq1:mslq81, key=item, value = score) %>% 
  mutate(reverse = if_else(item  %in% c('mslq33', 'mslq57', 'mslq52', 'mslq80', 'mslq37', 'mslq60', 'mslq40', 'mslq77'), "yes", "no")) %>% 
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
  select(part_id, Intrinsic_Goal_Orientation:Help_Seeking) %>% 
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

df %>% group_by(mos_transfer, college_degree) %>% summarise(n=n())
