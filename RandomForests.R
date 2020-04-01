library(randomForest)

#Read Data

df <- read.spss("CEMA Combined Data Set 16Sep19.sav", use.value.label=TRUE, to.data.frame=TRUE)
id <- rownames(df)
df <- cbind(id=id, df) 

#Tidy data and align to four groups
df$Group<-  gsub("RMIB non-CEMA","RMIB", df$Group, fixed=TRUE)
df$Group<-  gsub("CEMA","RMIB CEMA", df$Group, fixed=TRUE)
df$Group<-  gsub("Cyber_Operators","Cyber Pro", df$Group, fixed=TRUE)
df$Group<-replace_na(df$Group, "Not Assigned")
df<-df %>% filter(Group!="Not Assigned")
id_remove <- c("291", "299", "304", "307", "308", "330", "337", "341", "343", "354", "359", "360", "370", "374", "375", "384", "325", "346")


#Remove missing values and filter out Cyber Pro outliers 
df50 <- df %>% dplyr::select (id, Group, Combat_GGUM:Writing_GGUM) %>% filter (Group %in% c("Cyber Pro", "RASP" ), id %nin% id_remove)
count <- df50 %>% dplyr::group_by (Group) %>% dplyr::summarise(count=n())
df50<-df50 %>% dplyr::select(-id) 
df50$Group <- factor(df50$Group) 

df51 <- na.omit(df50)
count2 <- df51 %>% dplyr::group_by (Group) %>% dplyr::summarise(count=n())
count
count2

#mutate_at(vars(starts_with("A")), funs(ifelse(is.na(.),median(., na.rm = TRUE),.))) 


#Radom Forest Modeling################################
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(102)
train <- sample(nrow(df51), .7*nrow(df51), replace = FALSE)
TrainSet <- df51[train,]
ValidSet <- df51[-train,]
#summary(TrainSet)
#summary(ValidSet)
# Create a Random Forest model with default parameters
model1 <- randomForest(Group ~ ., data = TrainSet, importance = TRUE)
model1

model2 <- randomForest(Group ~ ., data = TrainSet, ntree = 400, mtry = 3, importance = TRUE, proximity=TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Group)


# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$Group)                    
table(predValid,ValidSet$Group)

# To check important variables
importance(model2)        
varImpPlot(model2) 
model2

#Display of results
#df_AVID_Results<- read.csv("AVID.csv")
#df_AVID_Results2 <- df_AVID_Results %>% mutate("Prediction" = ifelse(MeanDecreaseAccuracy >=2, "Best", ifelse(MeanDecreaseAccuracy >= 1.3, "Good", if_else(MeanDecreaseAccuracy>=-.1, "Minor", "Remove"))))
#summary(df_AVID_Results2$MeanDecreaseAccuracy)

# Tuning Number of Trees
oob.error.data <-data.frame(Trees=rep(1:nrow(model2$err.rate), times =3), Type=rep(c("OOB", "Cyber Pro", "RASP"), each=nrow(model2$err.rate)), Error=c(model2$err.rate[,"OOB"], model2$err.rate[, "Cyber Pro"], model2$err.rate[, "RASP"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+ geom_line(aes(color=Type))# Using For loop to identify the right mtry for model

oob.values <- vector(length=20)
for (i in 1:20) {
  temp.model <- randomForest(Group ~ ., data = TrainSet, ntree = 300, mtry = i, importance = TRUE)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values


### AVID Analysis 
df55<-read_xls("RandomForests_Results.xls")
df55 %>%  
mutate(Category = reorder(Category, DecreaseAccuracy, FUN=sum)) %>%
  ggplot()+geom_col(aes(x=Category,y=DecreaseAccuracy), fill="darkgreen") + coord_flip() +
  stat_summary (aes(x=Category, y=DecreaseAccuracy,label=round(..y..,1)), fun.y = "sum", geom="text", size=3, color="black", hjust=-.2) +
  ylab("Mean Decrease Accuracy") + 
  xlab ("AVID Dimension") + 
  ggtitle("Analysis of AVID Predictive Validity") + ylim(0,15) + theme(legend.title=element_text(color="blue", size=10), legend.position="top") +ggsave("AVID Analysis.jpg", width=6.5, height= 4, units= "in")

