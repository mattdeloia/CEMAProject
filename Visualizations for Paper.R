library(Hmisc)
library(data.table)
library(flexdashboard)
library(foreign)
library(ggpubr)
library(readxl)
library(forcats)
library(highcharter)
library(DT)
options(digits = 2)
library(tidyverse)
library(ggpubr)
library(tidyverse)
library(dplyr)
library(plotly)
library(corrplot)
library(futile.matrix)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(randomForest)
library(corrr)
library(ggcorrplot)
library(mlbench)
library(caret)
library(stringi)
library(stringr)
library(rgr)
library(plotly)
library(Rmisc)
library(gridExtra)

#load data
df_interestweightings <- read.csv("InterestCategoryWeightings.csv")
df_Weights <- read.csv("All_Value_Weights.csv")

df <- read.spss("CEMA Combined Data Set 16Sep19.sav", use.value.label=TRUE, to.data.frame=TRUE) 
df<- df  %>% mutate(id=rownames(df))

#Tidy data and align to four groups
df$Group<-  gsub("RMIB non-CEMA","RMIB", df$Group, fixed=TRUE)
df$Group<-  gsub("CEMA","RMIB CEMA", df$Group, fixed=TRUE)
df$Group<-  gsub("Cyber_Operators","Cyber Professionals", df$Group, fixed=TRUE)
df$Group<- replace_na(df$Group, "Not Assigned")
df<-df %>% filter(Group!="Not Assigned")
df$Q3_13.Enteryour3digitMOSindicator <- toupper(df$Q3_13.Enteryour3digitMOSindicator)

summary<-df %>% filter(Group=="RASP") %>% group_by(Group, Q3_13.Enteryour3digitMOSindicator)  %>% dplyr::summarise(n=n()) %>% mutate(reorder(Q3_13.Enteryour3digitMOSindicator, n))

#Interest Data
df_AVID <- df %>%gather(`Combat_GGUM`:`Writing_GGUM`, key="Interest", value="Score")
df_AVID2<- df_AVID %>% separate(col = `Interest`, into=c("Interest"), sep = "_", remove="TRUE")
df_AVID2 <- df_AVID2[!is.na(df_AVID2$Score), ]

#STT Scores
df$STTrials <-as.numeric(df$STTrials)
df %>% filter(!is.na(CRT_total)) %>% summarySE(measurevar = "CRT_total", groupvars = c("Group"))

#GT Scores
df$Group <- gsub("RASP", "Other Specialties", df$Group, fixed="TRUE")

plot1<-df %>% filter(Group %in% c("Cyber Professionals", "Other Specialties")) %>% 
  ggplot()+geom_boxplot(aes(x=Group, y=GTSCORE, fill=Group)) +
    stat_summary (aes(x=Group, y=GTSCORE,label=round(..y..,2)), fun.y = "median", geom="text", size=3, color="red", vjust=-.5)+
  xlab("")+
  ylab("GT Score")+ 
    scale_fill_manual(values=c("green","gray", "white", "white")) + 
  theme(legend.title=element_text(color="black", size=10), legend.position="none") +
  geom_hline(aes(yintercept =110), color="red", size=1, linetype="dashed") + geom_text(aes(x=0, y=110, label=("GT Minimum for Cyber MOS"), hjust=0, vjust=-.5), size=3)

plot2<-df %>% filter(Group %in% c("Cyber Professionals", "Other Specialties")) %>% 
  ggplot() + geom_boxplot(aes(y=ISCTtotraw, x=Group, fill=Group, color=Group), alpha=.75)+
  xlab("")+
  ylab("ISCT Raw Score")+ 
  stat_summary (aes(x=Group, y=ISCTtotraw,label=round(..y..,2)), fun.y = "median", geom="text", size=3, color="red", vjust=-.5)+
  scale_color_manual(values=c("black","black"))+scale_fill_manual(values=c("green","gray"))+ 
  theme(legend.title=element_text(color="black", size=10), legend.position="none") 

plotGT_ISCT<- grid.arrange(plot1, plot2, ncol=2)
ggsave("GT_ISCT.jpg", plotGT_ISCT, width=6.5, height= 4, units= "in")
############################################################

df_AVID2$Interest <- factor(df_AVID2$Interest, levels=c("Information", "Electronics", "Mathematics", "Office", "Teaching", "Food","Mechanical","Writing", "Construction",  "Outdoor","Human","Protection", "Medical","Leadership","Physical", "Combat"))
#df_AVID2$Interest <- factor(df_AVID2$Interest, levels=rev(levels(df_AVID2$Interest)))

### Interest Results II
df_AVID2$Group <- gsub("RASP", "Other Specialties", df_AVID2$Group, fixed="TRUE")
df_AVID2 %>% filter (Group %in% c("Cyber Professionals", "Other Specialties"))  %>% ggplot() + 
  geom_jitter(aes(Interest, Score, color=Group), alpha=.5)+ 
  ggtitle("Interest Scores ") + 
  theme(legend.title=element_text(color="gray", size=10), legend.position="top") +
  scale_color_manual(name="Group", values = c("green", "lightblue")) +
  theme (axis.text.x = element_text(angle=20, size=8))  +
  ggsave("Interest Jitter Plot.jpg", width=4, height= 3, units= "in")


### Interest Results III
df_AVID2$Group <- gsub("RASP", "Other Specialties", df_AVID2$Group, fixed="TRUE")
df_AVID2_Summary <- df_AVID2  %>% filter(!is.na(Score), Group %in% c("Cyber Professionals", "Other Specialties")) %>%
  summarySE(measurevar = "Score", groupvars = c("Interest", "Group"))

df_AVID2_Summary %>% ggplot(aes(x=Interest, y=Score, group=Group, color=Group)) +
  geom_point(size=2) +
  geom_line(aes(linetype=Group)) +
  geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 ) +
  theme(legend.title=element_text(color="black", size=10), legend.position="top") + 
  ylab("Mean Score") +
  xlab ("Interest Dimension") +
  scale_linetype_manual(values=c("dashed", "blank")) +
  scale_color_manual(name="Group", values = c("green", "blue")) + ylim(-1.2,1.2) +
  geom_text(aes(label = round(Score, 1)), size=2, vjust=.0, hjust=-.5) +
  theme (axis.text.x = element_text(angle=20, size=8))+
  ggsave("Interest Point Plot.jpg", width=6.5, height= 4, units= "in")



#df_AVID2 %>% filter (Group %in% c("Cyber Professionals", "RASP"))  %>% ggplot() + geom_boxplot(aes(Interest, Score,fill=Group))+ ggtitle("Boxplot: Interest Scores (Cyber Professionals Benchmark)") + theme(legend.title=element_text(color="gray", size=10), legend.position="top") + theme (axis.text.x = element_text(angle=20, size=8))+scale_fill_manual(name="Cohort", values = c("green", "gray")
   
#Correlations

#AVID Categories and ISCT (All Groups)

dfcluster_All<- df2  %>% dplyr::select(id, STTrials, GTSCORE, ELSCORE, SCSCORE, STSCORE, ISCTtotraw, CRT_total,  Combat, Construction, Electronics, Food, Human, Information, Leadership, Mathematics, Mechanical, Medical, Office, Outdoor, Physical, Protection, Teaching, Writing)
dfcluster_All <- dfcluster_All %>% drop_na(GTSCORE, ISCTtotraw, CRT_total, STSCORE, STTrials)

dfcluster_All<-data.frame(dfcluster_All, row.names=1)

corrplot(cor(dfcluster_All), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag = FALSE, number.font=1, number.digits = 1, number.cex = .7)  

ClusterAll<-cor(dfcluster_All) 
ClusterAll<-as.data.frame(ClusterAll)
ClusterAll<-cbind(Criterion1=rownames(ClusterAll), ClusterAll)
ClusterAll<-remove_rownames(ClusterAll)
ClusterAll2<- ClusterAll %>% gather(STTrials:Writing, key=Criterion2, value=AllGroups)


#AVID Categories and ISCT (RASP)

dfcluster_RASP<- df2  %>% filter(Group=="RASP") %>% dplyr::select(id, STTrials, GTSCORE, ELSCORE, SCSCORE, STSCORE, ISCTtotraw, CRT_total,  Combat, Construction, Electronics, Food, Human, Information, Leadership, Mathematics, Mechanical, Medical, Office, Outdoor, Physical, Protection, Teaching, Writing)
dfcluster_RASP <- dfcluster_RASP %>% drop_na(GTSCORE, ISCTtotraw, CRT_total, STSCORE, STTrials)

dfcluster_RASP<-data.frame(dfcluster_RASP, row.names=1)

corrplot(cor(dfcluster_RASP), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag = FALSE, number.font=1, number.digits = 1, number.cex = .7)

ClusterRASP<-cor(dfcluster_RASP) 
ClusterRASP<-as.data.frame(ClusterRASP)
ClusterRASP<-cbind(Criterion1=rownames(ClusterRASP), ClusterRASP)
ClusterRASP<-remove_rownames(ClusterRASP)
ClusterRASP2<- ClusterRASP %>% gather(STTrials:Writing, key=Criterion2, value=RASP)

#AVID Categories and ISCT (Cyber Professionals)

dfcluster_CyberPro<- df2  %>% filter(Group=="Cyber Professionals") %>% dplyr::select(id, STTrials, GTSCORE, ELSCORE, SCSCORE, STSCORE, ISCTtotraw, CRT_total,  Combat, Construction, Electronics, Food, Human, Information, Leadership, Mathematics, Mechanical, Medical, Office, Outdoor, Physical, Protection, Teaching, Writing)
dfcluster_CyberPro <- dfcluster_CyberPro %>% drop_na(GTSCORE, ISCTtotraw, CRT_total, STSCORE, STTrials)

dfcluster_CyberPro<-data.frame(dfcluster_CyberPro, row.names=1)

corrplot(cor(dfcluster_CyberPro), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag = FALSE, number.font=1, number.digits = 1, number.cex = .7)                      

ClusterCyberPro<-cor(dfcluster_CyberPro) 
ClusterCyberPro<-as.data.frame(ClusterCyberPro)
ClusterCyberPro<-cbind(Criterion1=rownames(ClusterCyberPro), ClusterCyberPro)
ClusterCyberPro<-remove_rownames(ClusterCyberPro)
ClusterCyberPro2<- ClusterCyberPro %>% gather(STTrials:Writing, key=Criterion2, value=CyberPro)

#AVID Categories and ISCT (RMIB)

dfcluster_All<- df2  %>% filter(Group %in% c("RMIB")) %>% dplyr::select(id, STTrials, GTSCORE, ELSCORE, SCSCORE, STSCORE, ISCTtotraw, CRT_total,  Combat, Construction, Electronics, Food, Human, Information, Leadership, Mathematics, Mechanical, Medical, Office, Outdoor, Physical, Protection, Teaching, Writing)
dfcluster_All <- dfcluster_All %>% drop_na(GTSCORE, ISCTtotraw, CRT_total, STSCORE, STTrials)

dfcluster_All<-data.frame(dfcluster_All, row.names=1)

corrplot(cor(dfcluster_All), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag = FALSE, number.font=1, number.digits = 1, number.cex = .7)  
                                                                                                                       

#AVID Categories and ISCT (RMIB CEMA)

dfcluster_All<- df2  %>% filter(Group %in% c("RMIB CEMA")) %>% dplyr::select(id, STTrials, GTSCORE, ELSCORE, SCSCORE, STSCORE, ISCTtotraw, CRT_total,  Combat, Construction, Electronics, Food, Human, Information, Leadership, Mathematics, Mechanical, Medical, Office, Outdoor, Physical, Protection, Teaching, Writing)
dfcluster_All <- dfcluster_All %>% drop_na(GTSCORE, ISCTtotraw, CRT_total, STSCORE, STTrials)

dfcluster_All<-data.frame(dfcluster_All, row.names=1)

corrplot(cor(dfcluster_All), method="color", order="hclust", type="full", addrect=5, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag = FALSE, number.font=1, number.digits = 1, number.cex = .7) 

#ISCT Total Score Correlation Comparison
CorrelationComparison<-ClusterAll2 %>% left_join(ClusterRASP2) %>% left_join(ClusterCyberPro2) %>% filter(Criterion1=="ISCTtotraw", !(Criterion2 %in% c("ISCTtotraw", "ELSCORE", "SCSCORE", "STSCORE"))) %>% gather(AllGroups:CyberPro, key=Group, value=Correlation)  

CorrelationComparison %>% mutate(Criterion2= reorder(Criterion2, Correlation, FUN=mean)) %>% ggplot() + geom_point(aes(x=Criterion2, y=Correlation, color=Group), size=2) +geom_line(aes(x=Criterion2, y=Correlation), color="darkgray") + theme(legend.title=element_text(color="gray", size=10), legend.position="top") + theme (axis.text.x = element_text(angle=20, size=8)) + ylim(-1,1)+ ggtitle("Correlations with the ISCT (All, RASP, Cyber Pro)")+scale_color_manual(name="Cohort", values = c("red", "green", "blue")) + ggsave("Correlations with the ISCT.pdf", width=11, height= 8.5, units= "in")


#Density Plot of Cyber Fit Scores
df10 <- df_AllScores %>% mutate("Cyber_Fit"=(.40*Interest_Score  + .20*(GTSCORE.x/125) + 0*Personality_Score +.205*(ISCTtotraw.x/23) + .10*( CRT_total/7) + .10*(STTrials/4))) %>% dplyr::rename(`Group`=`Group.x`,`ST_Total`=`STTrials`, `GT_Score`=`GTSCORE.x`, `ISCT_Score`=`ISCTtotraw.x`, `MOS`=`Q3_13.Enteryour3digitMOSindicator`, `APFT`=`Q4_14.WhatwasyourlastArmyPhysicalFitnessTestAPFTscore`, `Degree` =`Q77.DoyouhaveanAssociateBachelororMaster.slevelcollegedegree`,`Academic_Field`=`Q8_18.MajorQuestionPleaselistdegreemajors`)

df10x <- df10[!(is.na(df10$Cyber_Fit)),]
CyberFit_mean2<-df10x %>% filter(Group=="Cyber Pro") %>% summarise(mean(Cyber_Fit))
CyberFit_mean <- CyberFit_mean2$`mean(Cyber_Fit)`
CyberFit_sd4<-df10x %>% filter(Group=="Cyber Pro")%>%summarise(sd(Cyber_Fit))
CyberFit_sd <- CyberFit_sd4$`sd(Cyber_Fit)`
CyberFit_sd2<-CyberFit_mean-CyberFit_sd
CyberFit_sd3<-CyberFit_mean-2*CyberFit_sd

df10x$Group<- factor(df10x$Group, levels=c("Cyber Pro", "RASP"))
df10x$Group<-  gsub("Cyber Pro","Cyber Professionals", df10x$Group, fixed=TRUE)
df10x$Group<-  gsub("RASP","Other Specialties", df10x$Group, fixed=TRUE)
df10x %>% filter (Group!="RMIB") %>% ggplot()+geom_density(aes(x = Cyber_Fit, fill=Group), alpha=.75, adjust=2) + 
  geom_vline(aes(xintercept = CyberFit_sd3), color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept = CyberFit_sd2), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = CyberFit_mean), color="black")+ 
  geom_text(aes(x = CyberFit_sd3, y=0, label=("2sd"), hjust=0, vjust=-1), size=4)+
  geom_text(aes(x = CyberFit_sd2, y=0, label=("1sd"), hjust=0, vjust=-1), size=4)+
  geom_text(aes(x = CyberFit_mean, y=0, label=("cyber pro. mean"), hjust=0, vjust=-1), size=4)+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  xlab("Cyber Fit Score") +
  scale_fill_manual(name="Group", values = c("green", "gray")) + theme(legend.title=element_text(color="black", size=10), legend.position="top") + xlim(.4,1.1) +ggsave("Cyber Fit Density Plot.jpg", width=6.5, height= 4, units= "in")

### RASP Cyber Fit Table

df11<- df10 %>% filter (Group =="RASP") %>% dplyr::select (id, Group, MOS, `Cyber_Fit`, Interest_Score,  GT_Score, Personality_Score, ISCT_Score, CRT_total, ST_Total, APFT, Degree, Academic_Field) %>% mutate_if(is.numeric, round, digits=2)

df11$Degree<-  gsub("yes","Higher Degree", df11$Degree, fixed=TRUE)
df11$Degree<-  gsub("no","Sec. Education", df11$Degree, fixed=TRUE)

datatable (df11, rownames=FALSE, options=list(pageLength=45)) %>% 
  formatStyle('Cyber_Fit', 
              backgroundSize = '100% 90%', 
              backgroundRepeat='no-repeat', 
              backgroundPosition = 'center')%>%
  formatStyle('GT_Score', color = styleInterval (c(108, 115), c("red", "black", "green"))) %>%
  formatStyle ('Interest_Score', color = styleInterval(.76, c("black", "green"))) %>%
  formatStyle ('ST_Total', color = styleInterval(3, c("black", "green"))) %>%
  formatStyle ('Personality_Score', color = styleInterval(.73, c("black", "green"))) %>%
  formatStyle ('CRT_total', color = styleInterval(3, c("black", "green"))) %>%
  formatStyle ('GT_Score', backgroundColor = styleEqual (0 , "yellow")) %>%
  formatStyle ('Personality_Score', backgroundColor = styleEqual (.5 , "yellow")) %>%
  formatStyle ('ISCT_Score', backgroundColor = styleEqual (0 , "yellow")) %>%
  formatStyle ('Cyber_Fit', color = styleInterval( c(CyberFit_sd3, CyberFit_sd2), c("red", "black", "green")))
#  formatStyle ('ST_Total', backgroundColor = styleEqual (NA , "yellow")) %>%
 
write.csv(df11, "df11.csv")

#Correlation of Performance Ratings and ISCT
df20<-df
df20<-df20[!is.na(df20$CEMAoverallperformance_corrected),]

performanceNum <- function(x){
  case_when(
    x == "top performer" ~ 5,
    x == "above average" ~ 4,
    x == "average" ~ 3,
    x == "below average" ~ 2,
    x == "poor performer" ~ 1
  )
 } 
  df20 <- df20 %>%
    mutate_at(vars(`CEMAoverallperformance_corrected`), performanceNum)
  
  df21<- df20 %>% dplyr::select( id, CEMAoverallperformance_corrected, ISCTtotraw, Group)
  df21<-df21[!is.na(df21$ISCTtotraw),]
  
  df21 %>% dplyr::select (CEMAoverallperformance_corrected, ISCTtotraw) %>% cor()
  df21 %>% filter (Group=="CEMA") %>% dplyr::select (CEMAoverallperformance_corrected, ISCTtotraw) %>% cor()
  df21 %>% filter (Group=="Cyber_Operators") %>% dplyr::select (CEMAoverallperformance_corrected, ISCTtotraw) %>% cor()
  

  