install.packages('caret')
install.packages('leaps')
install.packages('class')
install.packages('stats')
install.packages('e1071')
install.packages('ROCR')
install.packages('neuralnet')
install.packages('ROSE')
install.packages('gridExtra')
install.packages('corrplot')
install.packages('randomForest')
install.packages('glmnet')
install.packages('party')
install.packages('pROC')

set.seed(9999)

install.packages('caret')

library(tibble)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(dplyr)
library(randomForest)
library('pROC')
library(glmnet)
library(party)
library(neuralnet)
library('ROSE')
library('caret')
library('leaps')
library('class')
library('stats')
library('e1071')
library('ROCR')

## HOW TO USE ##
data <- read.table("/card.csv",sep=",",skip=1,header=T)
data$PAY_1 = data$PAY_0

nrow(data) == nrow(na.omit(data))
# True --> no missing data in the dataset

#pie chart y
a = nrow(filter(data, default.payment.next.month==1))
b = nrow(filter(data, default.payment.next.month==0))
y_sum = c(a,b)
piepercent<- round(100*y_sum/sum(y_sum), 1)
label = c(paste(as.character(piepercent[1]), '%'), paste(as.character(piepercent[2]), '%'))
pie(y_sum, labels = label, main = "Dependent Variable", col = c('red', 'blue'))
legend("topright", c("Default (1)","No Default (0)"), cex = 0.8,fill = c('red', 'blue'))

# limit balance
options(scipen=10000)
hist(data$LIMIT_BAL, main = 'Balance each customer has',
     xlab = 'Balance ($)',
     ylab = 'Observations', axes = F, col='steelblue',
     ylim = c(0,8000))
axis(1,at=seq(0,1000000,by=200000))
axis(2,at=seq(0,8000,by=2000))

#Histogram chart
columns = c('AGE', 'SEX', 'EDUCATION', 'MARRIAGE')
data_hist = data[columns]
par(mfrow=c(2,2))
for (i in 1:4) {
  hist(data_hist[,i], main = columns[i], xlab = 'Independent Variable', col = 'steelblue')
}


pay_columns <- c("PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6")
bill_columns <- c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")
pay_amt_columns <- c("PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")
unique_values = c(-2,-1,0,1,3,4,5,6,7,8)

plots <- list()
for (col in pay_columns) {
  plot_hist <- ggplot(data, aes_string(x = col)) +
    geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
    ggtitle(paste("Histogram of", col)) +
    xlab(col) +
    ylab("Count") +
    scale_x_continuous( breaks = -2:8) +
    scale_y_continuous(limits = c(0, 18000), breaks = seq(0, 18000, 2000)) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title.x = element_text(size = 12, vjust = -0.2),
          axis.title.y = element_text(size = 12, vjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "#f0f0f0"),
          panel.grid.minor = element_blank())
  plots[[col]] <- plot_hist
}

grid.arrange(grobs = plots, ncol = 3)

corr = cor(select(data,c(pay_amt_columns,bill_columns,pay_columns)))
corrplot(corr)

# does anyone get an error here?
bill_data <- data[, bill_columns]
pay_data <- data[, pay_amt_columns]

bill_data_long <- reshape2::melt(bill_data)
pay_data_long <- reshape2::melt(pay_data)
bill_data_long <- data.frame(
  id = rep(bill_data$id, each = ncol(bill_data) - 1),
  variable = rep(names(bill_data)[-1], nrow(bill_data)),
  value = as.vector(bill_data[, -1])
)


plots <- list()
for (i in 1:6) {
  p = ggdensity(bill_data[,i], xlab = 'Amount ($)', main = bill_columns[i])
  plots[[i]] <- p
}
grid.arrange(grobs = plots, ncol = 3)


plots <- list()
for (i in 1:6) {
  p = ggdensity(pay_data[,i], xlab = 'Amount ($)', main = pay_amt_columns[i])
  plots[[i]] <- p
}
grid.arrange(grobs = plots, ncol = 3)

filtered_data = data %>%
  filter(BILL_AMT1 < quantile(data$BILL_AMT1, .75) + 4* IQR(data$BILL_AMT1)) %>%
  filter(BILL_AMT2 < quantile(data$BILL_AMT2, .75) + 4* IQR(data$BILL_AMT2)) %>%
  filter(BILL_AMT3 < quantile(data$BILL_AMT3, .75) + 4* IQR(data$BILL_AMT3)) %>%
  filter(BILL_AMT4 < quantile(data$BILL_AMT4, .75) + 4* IQR(data$BILL_AMT4)) %>%
  filter(BILL_AMT5 < quantile(data$BILL_AMT5, .75) + 4* IQR(data$BILL_AMT5)) %>%
  filter(BILL_AMT6 < quantile(data$BILL_AMT6, .75) + 4* IQR(data$BILL_AMT6)) %>%
  
  filter(PAY_AMT1 < quantile(data$PAY_AMT1, .75) + 4* IQR(data$PAY_AMT1)) %>%
  filter(PAY_AMT2 < quantile(data$PAY_AMT2, .75) + 4* IQR(data$PAY_AMT2)) %>%
  filter(PAY_AMT3 < quantile(data$PAY_AMT3, .75) + 4* IQR(data$PAY_AMT3)) %>%
  filter(PAY_AMT4 < quantile(data$PAY_AMT4, .75) + 4* IQR(data$PAY_AMT4)) %>%
  filter(PAY_AMT5 < quantile(data$PAY_AMT5, .75) + 4* IQR(data$PAY_AMT5)) %>%
  filter(PAY_AMT6 < quantile(data$PAY_AMT6, .75) + 4* IQR(data$PAY_AMT6))

bill_data <- filtered_data[, bill_columns]
pay_data <- filtered_data[, pay_amt_columns]

bill_data_long <- reshape2::melt(bill_data)
pay_data_long <- reshape2::melt(pay_data)

ggplot(bill_data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot( alpha = 0.5) +
  xlab("Bill Column") +
  ylab("Bill Amount") +
  ggtitle("Distribution of Bill Amounts") +
  # ylim()
  stat_boxplot(geom ='errorbar') +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, vjust = -0.2),
        axis.title.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#f0f0f0"),
        panel.grid.minor = element_blank())

ggplot(pay_data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot( alpha = 0.5) +
  xlab("Pay Column") +
  ylab("Pay Amount") +
  ggtitle("Distribution of Pay Amounts") +
  stat_boxplot(geom ='errorbar') +
  #ylim(c(0,100000))+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, vjust = -0.2),
        axis.title.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#f0f0f0"),
        panel.grid.minor = element_blank())


getStats <- function(predicted_train, predicted_test, train_default, test_default){
  confusion.matrix = matrix(1:12, nrow = 6, dimnames = list(c("Accuracy","Precision","Sensitivity", "False Negative Rate", "Harmonic Mean","Avg Class Accuracy"), c("Train","Test")))
  
  mean(predicted_train == train_default)
  confusion.matrix[1,1] = mean(predicted_train == train_default)
  confusion.matrix[1,2] = mean(predicted_test == test_default)
  
  train_table = table(pred=predicted_train,actual=train_default)
  TP = train_table[2,2]
  TN = train_table[1,1]
  FP = train_table[2,1]
  FN = train_table[1,2]
  confusion.matrix[2,1] = TP / (TP + FP)
  confusion.matrix[3,1] = TP / (TP + FN)
  confusion.matrix[4,1] = FN / (TP + FN)
  confusion.matrix[5,1] = 1 / (0.5 * ((TN+FN)/TN +(TP+FP)/TP))
  confusion.matrix[6,1] = (0.5 * (TN/(TN+FN) +TP/(TP+FP)))
  
  
  test_table = table(pred=predicted_test,actual=test_default)
  TP = test_table[2,2]
  TN = test_table[1,1]
  FP = test_table[2,1]
  FN = test_table[1,2]
  confusion.matrix[2,2] = TP / (TP + FP)
  confusion.matrix[3,2] = TP / (TP + FN)
  confusion.matrix[4,2] = FN / (TP + FN)
  confusion.matrix[5,2] = 1 / (0.5 * ((TN+FN)/TN +(TP+FP)/TP))
  confusion.matrix[6,2] = (0.5 * (TN/(TN+FN) +TP/(TP+FP)))
  
  
  confusion.matrix
}

getStat <- function(predicted, dataset){
  confusion.matrix = matrix(1:6, nrow = 6, dimnames = list(c("Accuracy","Precision","Sensitivity", "False Negative Rate", "Harmonic Mean","Avg Class Accuracy")))
  
  mean(predicted == dataset)
  confusion.matrix[1,1] = mean(predicted == dataset)
  
  test_table = table(pred=predicted,actual=dataset)
  TP = test_table[2,2]
  TN = test_table[1,1]
  FP = test_table[2,1]
  FN = test_table[1,2]
  confusion.matrix[2,1] = TP / (TP + FP)
  confusion.matrix[3,1] = TP / (TP + FN)
  confusion.matrix[4,1] = FN / (TP + FN)
  confusion.matrix[5,1] = 1 / (0.5 * ((TN+FN)/TN +(TP+FP)/TP))
  confusion.matrix[6,1] = (0.5 * (TN/(TN+FN) +TP/(TP+FP)))
  
  confusion.matrix
}


dataset <- data
dataset$PAY_1 <- dataset$PAY_0
dataset$EDUCATION[dataset$EDUCATION == 0] <- 4
dataset$EDUCATION[dataset$EDUCATION == 5] <- 4
dataset$EDUCATION[dataset$EDUCATION == 6] <- 4
dataset$MARRIAGE[dataset$MARRIAGE == 0] <- 3
dataset$PAY_1[dataset$PAY_1 == '-2'] <- 0
dataset$PAY_1[dataset$PAY_1 == '-1'] <- 0
dataset$PAY_2[dataset$PAY_2 == '-2'] <- 0
dataset$PAY_2[dataset$PAY_2 == '-1'] <- 0
dataset$PAY_3[dataset$PAY_3 == '-2'] <- 0
dataset$PAY_3[dataset$PAY_3 == '-1'] <- 0
dataset$PAY_4[dataset$PAY_4 == '-2'] <- 0
dataset$PAY_4[dataset$PAY_4 == '-1'] <- 0
dataset$PAY_5[dataset$PAY_5 == '-2'] <- 0
dataset$PAY_5[dataset$PAY_5 == '-1'] <- 0
dataset$PAY_6[dataset$PAY_6 == '-2'] <- 0
dataset$PAY_6[dataset$PAY_6 == '-1'] <- 0

dataset$ID <- as.integer(dataset$ID)
dataset$LIMIT_BAL <- as.integer(dataset$LIMIT_BAL)
dataset$SEX <- as.factor(dataset$SEX)
dataset$EDUCATION <- as.factor(dataset$EDUCATION)
dataset$MARRIAGE <- as.factor(dataset$MARRIAGE)
dataset$AGE <- as.integer(dataset$AGE)
dataset$PAY_0 <- as.integer(dataset$PAY_0)
dataset$PAY_1 <- as.integer(dataset$PAY_1)
dataset$PAY_2 <- as.integer(dataset$PAY_2)
dataset$PAY_3 <- as.integer(dataset$PAY_3)
dataset$PAY_4 <- as.integer(dataset$PAY_4)
dataset$PAY_5 <- as.integer(dataset$PAY_5)
dataset$PAY_6 <- as.integer(dataset$PAY_6)
dataset$BILL_AMT1 <- as.integer(dataset$BILL_AMT1)
dataset$BILL_AMT2 <- as.integer(dataset$BILL_AMT2)
dataset$BILL_AMT3 <- as.integer(dataset$BILL_AMT3)
dataset$BILL_AMT4 <- as.integer(dataset$BILL_AMT5)
dataset$BILL_AMT5 <- as.integer(dataset$BILL_AMT5)
dataset$BILL_AMT6 <- as.integer(dataset$BILL_AMT6)
dataset$PAY_AMT1 <- as.integer(dataset$PAY_AMT1)
dataset$PAY_AMT2 <- as.integer(dataset$PAY_AMT2)
dataset$PAY_AMT3 <- as.integer(dataset$PAY_AMT3)
dataset$PAY_AMT4 <- as.integer(dataset$PAY_AMT4)
dataset$PAY_AMT5 <- as.integer(dataset$PAY_AMT5)
dataset$PAY_AMT6 <- as.integer(dataset$PAY_AMT6)
dataset$Y <- as.factor(dataset$`default.payment.next.month`)
dataset = subset(dataset, select = -c(PAY_0, BILL_AMT1,BILL_AMT2,BILL_AMT4, BILL_AMT5,BILL_AMT6,default.payment.next.month))


set.seed(9999)

n = length(dataset$ID)
index <- 1:nrow(dataset)
testindex <- sample(index, trunc(n)/4)
test.data <- dataset[testindex,]
train.data <- dataset[-testindex,]


#OVUN sampling

train.data <- ovun.sample(Y ~ ., data = train.data, method = "both", p = 0.5, seed = 1234, N = 7500)$data


train.data$Y = as.factor(train.data$Y)



logit <- glm(as.factor(`Y`) ~., data = train.data, family = "binomial")



all.logit.stats <- c()
thresholds <- seq(0.1, 0.9, by = 0.1)
for (t in thresholds) {
  predict_logit.train <- predict(logit, train.data, type = "response")
  predict_logit.train <- ifelse(predict_logit.train > t, 1, 0)
  predict_logit <- predict(logit, test.data, type = "response")
  
  predict_logit <- ifelse(predict_logit > t, 1, 0)
  
  train.stat <- getStat(predict_logit.train, train.data$Y)
  test.stat <- getStat(predict_logit, test.data$Y)
  all.logit.stats <- cbind(all.logit.stats, train.stat[,1], test.stat[,1])
  # all.logit.stats <- cbind(all.logit.stats, test.stat[,1])
}


predict_logit.train <- predict(logit, train.data, type = "response")

colnames(all.logit.stats) <- c("0.1 train", "0.1 test",
                               "0.2 train", "0.2 test",
                               "0.3 train", "0.3 test",
                               "0.4 train", "0.4 test",
                               "0.5 train", "0.5 test",
                               "0.6 train", "0.6 test",
                               "0.7 train", "0.7 test",
                               "0.8 train", "0.8 test",
                               "0.9 train", "0.9 test")

all.logit.stats


predict_logit.train <- predict(logit, train.data, type = "response")
predict_logit.train

colnames(all.logit.stats) <- c("0.1 train", "0.1 test",
                               "0.2 train", "0.2 test",
                               "0.3 train", "0.3 test",
                               "0.4 train", "0.4 test",
                               "0.5 train", "0.5 test",
                               "0.6 train", "0.6 test",
                               "0.7 train", "0.7 test",
                               "0.8 train", "0.8 test",
                               "0.9 train", "0.9 test")

all.logit.stats

# threshold = 0.5
predict_logit.train <- predict(logit, train.data, type = "response")
predict_logit.train <- ifelse(predict_logit.train > 0.5, 1, 0)
predict_logit <- predict(logit, test.data.encoded, type = "response")
predict_logit <- ifelse(predict_logit > 0.5, 1, 0)

logit_stat <- all.logit.stats[,5]

# ROC-AUC

library(ROCR)

# plotting the ROC curve
p <- predict(logit, test.data, type="response")
pr <- prediction(p, test.data$Y)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# calculating the area under curve (AUC)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# confusion matrix for threshold = 0.5
pred_result <- ifelse(p > 0.5, 1, 0)
table(actual = test.data$Y, pred = pred_result)

# confusion matrix for threshold = 0.3
new_pred_result <- ifelse(p > 0.3, 1, 0)
table(actual = test.data$Y, pred = new_pred_result)
```

dataset = data
dataset$PAY_1 = dataset$PAY_0
dataset$EDUCATION[dataset$EDUCATION == 0] <- 4
dataset$EDUCATION[dataset$EDUCATION == 5] <- 4
dataset$EDUCATION[dataset$EDUCATION == 6] <- 4
dataset$MARRIAGE[dataset$MARRIAGE == 0] <- 3
dataset$PAY_1[dataset$PAY_1 == '-2'] <- 0
dataset$PAY_1[dataset$PAY_1 == '-1'] <- 0
dataset$PAY_2[dataset$PAY_2 == '-2'] <- 0
dataset$PAY_2[dataset$PAY_2 == '-1'] <- 0
dataset$PAY_3[dataset$PAY_3 == '-2'] <- 0
dataset$PAY_3[dataset$PAY_3 == '-1'] <- 0
dataset$PAY_4[dataset$PAY_4 == '-2'] <- 0
dataset$PAY_4[dataset$PAY_4 == '-1'] <- 0
dataset$PAY_5[dataset$PAY_5 == '-2'] <- 0
dataset$PAY_5[dataset$PAY_5 == '-1'] <- 0
dataset$PAY_6[dataset$PAY_6 == '-2'] <- 0
dataset$PAY_6[dataset$PAY_6 == '-1'] <- 0

dataset$ID <- as.integer(dataset$ID)
dataset$LIMIT_BAL <- as.integer(dataset$LIMIT_BAL)
dataset$SEX <- as.factor(dataset$SEX)
dataset$EDUCATION <- as.factor(dataset$EDUCATION)
dataset$MARRIAGE <- as.factor(dataset$MARRIAGE)
dataset$AGE <- as.integer(dataset$AGE)
dataset$PAY_0 <- as.factor(dataset$PAY_0)
dataset$PAY_1 <- as.factor(dataset$PAY_1)
dataset$PAY_2 <- as.factor(dataset$PAY_2)
dataset$PAY_3 <- as.factor(dataset$PAY_3)
dataset$PAY_4 <- as.factor(dataset$PAY_4)
dataset$PAY_5 <- as.factor(dataset$PAY_5)
dataset$PAY_6 <- as.factor(dataset$PAY_6)
dataset$BILL_AMT1 <- as.integer(dataset$BILL_AMT1)
dataset$BILL_AMT2 <- as.integer(dataset$BILL_AMT2)
dataset$BILL_AMT3 <- as.integer(dataset$BILL_AMT3)
dataset$BILL_AMT4 <- as.integer(dataset$BILL_AMT5)
dataset$BILL_AMT5 <- as.integer(dataset$BILL_AMT5)
dataset$BILL_AMT6 <- as.integer(dataset$BILL_AMT6)
dataset$PAY_AMT1 <- as.integer(dataset$PAY_AMT1)
dataset$PAY_AMT2 <- as.integer(dataset$PAY_AMT2)
dataset$PAY_AMT3 <- as.integer(dataset$PAY_AMT3)
dataset$PAY_AMT4 <- as.integer(dataset$PAY_AMT4)
dataset$PAY_AMT5 <- as.integer(dataset$PAY_AMT5)
dataset$PAY_AMT6 <- as.integer(dataset$PAY_AMT6)
dataset$Y <- as.factor(dataset$`default.payment.next.month`)

dataset = subset(dataset, select = -c(PAY_0, BILL_AMT1,BILL_AMT2,BILL_AMT4, BILL_AMT5,BILL_AMT6,default.payment.next.month))


set.seed(9999)

n = length(dataset$ID)
index <- 1:nrow(dataset)
testindex <- sample(index, trunc(n)/4)
test.data <- dataset[testindex,]
train.data <- dataset[-testindex,]

train.data <- ovun.sample(Y ~ ., data = train.data, method = "both", p = 0.5, seed = 1234, N = 7500)$data

output.tree <- ctree(
  as.factor(Y) ~ .,
  data = train.data)
dt_test = predict(output.tree, test.data[,-20])
dt_train = predict(output.tree, train.data[,-20])

getStats(dt_train, dt_test, train.data[,20], test.data[,20])

model_rf<-randomForest(Y ~ ., data = train.data)
preds_test <-predict(model_rf,test.data[,-20])
preds_train <-predict(model_rf,train.data[,-20])
getStats(preds_train, preds_test, train.data[,20], test.data[,20])


rf_importance = data.frame(importance(model_rf))


rf_importance <- rf_importance %>% arrange(desc(MeanDecreaseGini))
rf_importance

model_rf<-randomForest( Y ~ PAY_1 + BILL_AMT3	+ PAY_AMT1 + PAY_AMT2	+ LIMIT_BAL	+ AGE + PAY_AMT3, data = train.data)
preds_test <-predict(model_rf,test.data[,-20])
preds_train <-predict(model_rf,train.data[,-20])
getStats(preds_train, preds_test, train.data[,20], test.data[,20])


ntrees = seq(400,800, by =100)
statistics = c()

for (n in ntrees) {
  model_rf_new<-randomForest(Y ~ .,
                             data = train.data,
                             ntree = n)
  predicted_train<-predict(model_rf_new,train.data[,-20])
  predicted_test<-predict(model_rf_new,test.data[,-20])
  confusion.matrix = getStats(predicted_train, predicted_test, train.data[,20], test.data[,20])
  statistics = cbind(statistics,confusion.matrix[,2])
  
}
colnames(statistics) = ntrees
statistics


intercept_only <- lm(default.payment.next.month ~ 1, data = dataset2)
all <- lm(default.payment.next.month ~ ., data = dataset2)
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward$anova

forward$coefficients



set.seed(9999)
dataset = data
dataset$Y <- as.integer(dataset$`default.payment.next.month`)
dataset$PAY_1 = dataset$PAY_0
dataset = subset(dataset, select = -c(PAY_0, BILL_AMT1,BILL_AMT2,BILL_AMT4, BILL_AMT5,BILL_AMT6,default.payment.next.month))

n = length(dataset$ID)
index <- 1:nrow(dataset)
testindex <- sample(index, trunc(n)/4)
test.data <- dataset[testindex,]
train.data <- dataset[-testindex,]
train.data <- ovun.sample(Y ~ ., data = train.data, method = "both", p = 0.5, seed = 1234, N = 7500)$data
train.data.nn <- as.data.frame(lapply(train.data, as.numeric))
test.data.nn <- as.data.frame(lapply(test.data, as.numeric))

summary(train.data.nn)

nnet=neuralnet(Y~., train.data.nn , hidden = c(4,4), linear.output = FALSE)
#total neurons < 2/3 * 20 + 1 abt 8)
preds_test <-ifelse(predict(nnet,test.data.nn[,-20]) > 0.5 ,1,0)
preds_train <-ifelse(predict(nnet,train.data.nn[,-20]) > 0.5, 1, 0)
getStats(preds_train, preds_test, train.data.nn[,20], test.data.nn[,20])


preds_test <-ifelse(predict(nnet,test.data.nn[,-20]) > 0.60 ,1,0)
preds_train <-ifelse(predict(nnet,train.data.nn[,-20]) > 0.60, 1, 0)
getStats(preds_train, preds_test, train.data.nn[,20], test.data.nn[,20])

train_table = table(pred=preds_test,actual=test.data.nn[,20])

train_table

all.knn.stats <- c()
kvalues <- seq(50, 200, by=50)
for (k in kvalues) {
  set.seed(9999)
  knn <- knn(train=train.data[,-ncol(train.data)],
             test=test.data[,-ncol(test.data)],
             cl=train.data$Y,
             k=k)
  
  test.stat <- getStat(knn, test.data[,ncol(test.data)])
  
  all.knn.stats <- cbind(all.knn.stats, test.stat[,1])
}
colnames(all.knn.stats) <- kvalues
all.knn.stats

all.svm.stats <- c()
costs <- c(0.1, 1, 10)
for (cost in costs) {
  svm.model <- svm(Y ~., data = train.data, type = "C-classification", kernel = "linear", cost = cost)
  # results.train <- predict(svm.model, train.data[,-ncol(train.data)])
  results.test <-  predict(svm.model, test.data[,-ncol(test.data)])
  # train.stat <- getStat(results.train, train.data[,ncol(train.data)])
  test.stat <- getStat(results.test, test.data$Y)
  all.svm.stats <- cbind(all.svm.stats, test.stat[,1])
}

colnames(all.svm.stats) <- costs
all.svm.stats # accuracy is best when cost is 0.1

all.svm.stats <- c()
costs <- c(0.1, 1, 10)
for (cost in costs) {
  svm.model <- svm(Y ~., data = train.data, type = "C-classification", kernel = "linear", cost = cost)
  # results.train <- predict(svm.model, train.data[,-ncol(train.data)])
  results.test <-  predict(svm.model, test.data[,-ncol(test.data)])
  # train.stat <- getStat(results.train, train.data[,ncol(train.data)])
  test.stat <- getStat(results.test, test.data$Y)
  all.svm.stats <- cbind(all.svm.stats, test.stat[,1])
}

colnames(all.svm.stats) <- costs
all.svm.stats # accuracy is best when cost is 0.1

all.svm.stats <- c()
kernels <- c("linear", "radial", "polynomial", "sigmoid")
for (kernel in kernels) {
  svm.model <- svm(Y ~., data = train.data, type = "C-classification", kernel = kernel, cost = 10)
  # results.train <- predict(svm.model, train.data[,-ncol(train.data)])
  results.test <-  predict(svm.model, test.data[,-ncol(test.data)])
  # train.stat <- getStat(results.train, train.data[,ncol(train.data)])
  test.stat <- getStat(results.test, test.data$Y)
  all.svm.stats <- cbind(all.svm.stats, test.stat[,1])
}
all.svm.stats # accuracy is best when kernel = "radial"

all.svm.stats <- c()
#using crossmodel
svm.crossmodel<-svm(Y ~ . , data=dataset, cross=5, type="C-classification", kernel="radial", cost=0.1)
results.test <-  predict(svm.crossmodel, dataset[,-ncol(dataset)])
test.stat <- getStat(results.test, dataset$Y)
all.svm.stats <- cbind(all.svm.stats, test.stat[,1])
all.svm.stats

# changing up the weights
all.svm.stats <- c()
weights <- seq(0.2, 0.8, by = 0.2)
for (weight in weights) {
  svm.model <- svm(Y ~., data = train.data, type = "C-classification", kernel = "radial", cost = 0.1, class.weights = c("0" = weight, "1" = (1 - weight)))
  results.test <-  predict(svm.model, test.data[,-ncol(test.data)])
  test.stat <- getStat(results.test, test.data$Y)
  all.svm.stats <- cbind(all.svm.stats, test.stat[,1])
}
colnames(all.svm.stats) <- weights
all.svm.stats

svm.model <- svm(Y ~., data = train.data, type = "C-classification",
                 kernel = "radial", cost = 0.1, class.weights = c("0" = 0.5, "1" = 0.5))
results.test <-  predict(svm.model, test.data[,-ncol(test.data)])
test.stat <- getStat(results.test, test.data$Y)
test.stat
