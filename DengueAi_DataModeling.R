#IST 707 Data Modeling
#(Fresh Code)

library(lubridate)
library(caret)
library(reshape2)
library(ggplot2)

iq.df <- read.csv("\\\\hd.ad.syr.edu\\03\\b018d0\\Documents\\Desktop\\Courses\\IST 707\\Final Poster\\Iquistos_Clean v2.csv",
                  header = TRUE)
sj.df <- read.csv("\\\\hd.ad.syr.edu\\03\\b018d0\\Documents\\Desktop\\Courses\\IST 707\\Final Poster\\SanJuan_Clean v2.csv",
                  header = TRUE)

sj.df$city <- "San Juan"
iq.df$city <- "Iquitos"

dengue.df <- rbind(sj.df, iq.df)
dengue.df$month <- month(dengue.df$week_start_date)
dengue.df$year <- year(dengue.df$week_start_date)

uni.years <- unique(year(dengue.df$week))

train.index <- NA
for (i in 1:12) {
  sub.iq.index <- which(month(dengue.df$week_start_date) == i & dengue.df$city == "Iquitos")
  sub.sj.index <- which(month(dengue.df$week_start_date) == i & dengue.df$city == "San Juan")
  train.index <- c(train.index
                   , sample(sub.iq.index, length(sub.iq.index) * 0.8, replace = TRUE)
                   , sample(sub.sj.index, length(sub.sj.index) * 0.8, replace = TRUE))
}
train.index <- train.index[-1]

train.df <- dengue.df[train.index, ]
test.df <- dengue.df[-train.index, ]

#Bar plot - City x Month x Total Cases / Pop

bp1.df <- data.frame(tapply(dengue.df$total_cases / dengue.df$population * 100000
                            , list(dengue.df$month, dengue.df$city)
                            , FUN = sum))
bp1.df$month <- 1:12

bp1.df <- melt(bp1.df, id = "month")

bp1.plot <- ggplot(data = bp1.df, aes(x = month, y = value, fill = variable))
bp1.plot <- bp1.plot + geom_col(stat="identity",position="dodge")
bp1.plot <- bp1.plot + scale_x_discrete(limit = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
bp1.plot <- bp1.plot + theme_minimal()
bp1.plot


#Line Plots

lp1.df <- data.frame(tapply(dengue.df$avgtemp
                            , list(dengue.df$month, dengue.df$city)
                            , FUN = mean))
lp1.df$month <- 1:12

lp1.df <- melt(lp1.df, id = "month")

lp1.plot <- ggplot(data = dengue.df, aes(x = as.date(week_start_date), y = avgtemp, col = city))
lp1.plot <- lp1.plot + geom_line(stat = "identity",position = "dodge")
#lp1.plot <- lp1.plot + scale_x_discrete(limit = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
lp1.plot <- lp1.plot + theme_minimal()
lp1.plot


#Linear / Logistic Regression

lmmodel1 <- train(total_cases ~ population + mintemp + maxtemp + avgtemp + precipitation + city
                  , data = train.df, method = "lm"
                  , preProcess = c("range")
)
print(lmmodel1)
lmmodel1$finalModel
plot(lmmodel1$finalModel)

preProcess = c("center", "scale"))
print(model_lm2)

pred.lm1 <- predict(lmmodel1, newdata = test.df, type = "raw")
real_predict <- data.frame(cbind(real = test.df$total_cases, predict = pred.lm1, 
                                 square_diff = (test.df$total_cases - pred.lm1)^2))
str(real_predict)

SSE <- sum(real_predict$square_diff)
TSS <- var(test.df$total_cases) * (length(test.df$total_cases) + 1)
R_squared <- 1 - SSE/TSS
R_squared
# 0.07815186 - Congrats!

boxplot(dengue.df$total_cases / dengue.df$population * 1000000 ~ dengue.df$city, horizontal = TRUE, outline = FALSE)
median(dengue.df$total_cases / dengue.df$population * 1000000)

#more than 6 cases per 100.000 people would be considered high incidence of Dengue Fever

quantile(train.df$total_cases)

train.df2 <- train.df
train.df2$total_cases_class <- cut(train.df2$total_cases / train.df2$population * 1000000
                                   , breaks = c(-1, 6, 10000)
                                   , labels = c("Low","High"))

modelglm1 <- train(total_cases_class ~ population + mintemp + maxtemp + avgtemp + precipitation + city 
                   + air_temperature + specific_humidity + dew_point_temperature + month
                   , data = train.df2
                   , method = "glm")

colnames(train.df2)

print(modelglm1)
summary(modelglm1)
varImp(modelglm1)

test.df2 <- test.df
test.df2$total_cases_class <- cut(test.df2$total_cases / test.df2$population * 1000000
                                  , breaks = c(-1, 6, 10000)
                                  , labels = c("Low","High"))

pred.lr1 <- predict(modelglm1, newdata = test.df2, type = "raw")
real_predict <- table(Labeled = test.df2$total_cases_class, predict = pred.lr1)
prop.table(real_predict, 1)

#KNN

knn1 <- train(total_cases_class ~ population + mintemp + maxtemp + avgtemp + precipitation + city 
              + air_temperature + specific_humidity + dew_point_temperature + month
              , data = train.df2, method = "knn")

pred_knn <- predict(knn1, newdata = test.df2)

prop.table(table(Labeled = test.df2$total_cases_class, predict = pred_knn), 1)
plot(confusionMatrix(pred_knn, test.df2$total_cases_class))

#SVM

svm1 <- train(total_cases_class ~ population + mintemp + maxtemp + avgtemp + precipitation + city 
              + air_temperature + specific_humidity + dew_point_temperature + month
              , data = train.df2
              , method = "svmLinear"
              , preProcess = c("center", "scale")
              , trControl = trainControl(method = "boot", number = 25)
              , tuneGrid = expand.grid(C = seq(0, 1, 0.05)))

pred_svm <- predict(svm1, newdata = test.df2)

prop.table(table(Labeled = test.df2$total_cases_class, predict = pred_svm), 1)

print(svm1)


#Decision Tree

dtree.m1 <- train(total_cases_class ~ population + mintemp + maxtemp + avgtemp + precipitation + city 
                  + air_temperature + specific_humidity + dew_point_temperature + month
                  , data = train.df2
                  , method = "rpart"
                  , tuneLength = 10
                  #, control = rpart.control(minsplit = 5, minbucket = 6, maxdepth = 8)
)

plot(dtree.m1$finalModel)

library(rpart.plot)
rpart.plot(dtree.m1$finalModel, box.palette="RdBu", shadow.col="gray", nn=TRUE)

pred.rpart <- predict(dtree.m1, newdata = test.df2, type = "raw")

prop.table(table(pred.rpart, test.df2$total_cases_class),1)
