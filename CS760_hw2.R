df <- data.frame(c = c(0.95,0.85,0.8,0.7,0.55,0.45,0.4,0.3,0.2,0.1),
                 y=c(1,1,0,1,1,0,1,1,0,0))
TP <- 0
FP <- 0
last_TP <- 0

for (i in 1:10) {
  if (i>1 & df$y[i] == 0 & TP > last_TP ) {
    if (df$c[i] != df$c[i-1]) {
      FPR = FP /4
      TPR = TP /6
      print(df$c[i])
      print(c(FPR,TPR)) 
    }
  }
  if (df$y[i] == 1){
    TP <- TP+1
  }
  else{
    FP <- FP+1
  }
}
FPR = FP /4
TPR = TP /6
print(c(FPR,TPR))

ROC <- data.frame(FPR=c(0,0,0.25,0.5,0.75,1),TPR=c(0,1/3,2/3,1,1,1))

library(ggplot2)
ggplot(ROC) +
  aes(x = FPR, y = TPR) +
  geom_line(colour = "#112446") +
  theme_minimal()


# Q1

library(proxy)
library(pals)
library(brew)

D2z <- read.table("H:/Course/CS 760/hw3-1/data/D2z.txt", quote="\"", comment.char="")
colnames(D2z) <- c("x1","x2","y")
testD2z <- data.frame(x1 = rep(seq(-2,2,0.1),41),
                      x2 = rep(seq(-2,2,0.1), each=41),
                      y = rep(-1,1681))

dist_matrix <- dist(D2z[,1:2],testD2z[,1:2])

for (i in 1:1681) {
  testD2z[i,3] <- D2z$y[which.min(dist_matrix[,i])]  
}

D2z$y <- as.factor(D2z$y)
testD2z$y <- as.factor(testD2z$y)

ggplot(data = testD2z ,aes(x = x1, y = x2, colour = y)) +
  geom_point(shape = "circle", size = 1.5) +
  geom_point(data = D2z ,shape = "triangle down filled", size = 2) +
  scale_color_hue(direction = 1) +
  theme_minimal()

# Q2

library(data.table)
emails <- as.data.frame(fread("H:/Course/CS 760/hw3-1/data/emails.csv"))

emails_train <- {}
emails_test <- {}

emails_train[[1]] <- emails[1001:5000,]
emails_test[[1]] <- emails[1:1000,]
emails_train[[2]] <- emails[c(1:1000,2001:5000),]
emails_test[[2]] <- emails[1001:2000,]
emails_train[[3]] <- emails[c(1:2000,3001:5000),]
emails_test[[3]] <- emails[2001:3000,]
emails_train[[4]] <- emails[c(1:3000,4001:5000),]
emails_test[[4]] <- emails[3001:4000,]
emails_train[[5]] <- emails[c(1:4000),]
emails_test[[5]] <- emails[4001:5000,]

pred <- rep(-1,1000)
dist_matrix <- {}
for (i in 1:5) {
  dist_matrix[[i]] <- dist(emails_train[[i]][,2:3001],emails_test[[i]][,2:3001])
}

for (i in 1:5) {
  for (j in 1:1000) {
    pred[j] <- emails_train[[i]][,3002][which.min(dist_matrix[[i]][,j])]  
  }
  emails_test[[i]][,"KNN_1"] <- pred
}

table(emails_test[[1]][,"KNN_1"],emails_test[[1]][,3002])
table(emails_test[[2]][,"KNN_1"],emails_test[[2]][,3002])
table(emails_test[[3]][,"KNN_1"],emails_test[[3]][,3002])
table(emails_test[[4]][,"KNN_1"],emails_test[[4]][,3002])
table(emails_test[[5]][,"KNN_1"],emails_test[[5]][,3002])

(592+233)/1000
(613+240)/1000
(624+238)/1000
(611+240)/1000
(543+232)/1000

233/(123+233)
240/(110+240)
238/(92+238)
240/(95+240)
232/(151+232)

233/(52+233)
240/(37+240)
238/(46+238)
240/(54+240)
232/(74+232)

# Q3

for (i in 1:5) {
  x <- t(as.matrix(emails_train[[i]][,2:3001]))
  y <- as.vector(t((emails_train[[i]][,3002])))
  
  theta <- rep(0.1,3000)
  dtheta <- 1
  iter <- 1
  
  while (dtheta > 1e-4 & iter < 2000) {
    sigma <- as.vector(theta %*% x)
    y_hat <- as.vector(1 / (1+exp(-sigma)))
    dy <- as.vector((y_hat - y) %*% t(x))
    #  dtheta <- norm(dy)
    dtheta <- abs(norm(as.matrix(theta),type = "2") - norm(theta - 0.5/4000 * dy,type = "2"))
    theta <- theta - 0.5 / 4000 * dy
    iter <- iter + 1
  }
  
  y_hat_test <- as.vector(1 / (1+exp(-as.vector(theta %*% t(as.matrix(emails_test[[i]][,2:3001]))))))
  emails_test[[i]][,"logistic"] <- ifelse(y_hat_test>0.5,1,0)
  print(table(emails_test[[i]][,"logistic"],emails_test[[i]][,3002]))
}

for (i in 1:5) {
  confusion_matix <- table(emails_test[[i]][,"logistic"],emails_test[[i]][,3002])
  accu <- (confusion_matix[1,1] + confusion_matix[2,2])/1000
  precise <- confusion_matix[2,2] / (confusion_matix[2,1] + confusion_matix[2,2])
  recall <- confusion_matix[2,2] / (confusion_matix[1,2] + confusion_matix[2,2])
  print(c(accu,precise,recall))
}


(692+231)/1000
(674+255)/1000
(696+213)/1000
(683+220)/1000
(649+69)/1000

231/(23+231)
255/(49+255)
213/(20+213)
220/(23+220)
69/(45+69)

231/(54+231)
255/(22+255)
213/(74+220)
251/(43+251)
293/(13+293)



# Q4

library(kit)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:5) {
  for (j in 1:1000) {
    pred[j] <- getmode(emails_train[[i]][,3002][topn(dist_matrix[[i]][,j], 3,decreasing = F, index = T)])
  }
  emails_test[[i]][,"KNN_3"] <- pred
}

for (i in 1:5) {
  for (j in 1:1000) {
    pred[j] <- getmode(emails_train[[i]][,3002][topn(dist_matrix[[i]][,j], 5,decreasing = F, index = T)])
  }
  emails_test[[i]][,"KNN_5"] <- pred
}

for (i in 1:5) {
  for (j in 1:1000) {
    pred[j] <- getmode(emails_train[[i]][,3002][topn(dist_matrix[[i]][,j], 7,decreasing = F, index = T)])
  }
  emails_test[[i]][,"KNN_7"] <- pred
}

for (i in 1:5) {
  for (j in 1:1000) {
    pred[j] <- getmode(emails_train[[i]][,3002][topn(dist_matrix[[i]][,j], 9,decreasing = F, index = T)])
  }
  emails_test[[i]][,"KNN_9"] <- pred
}

mean(
  (table(emails_test[[1]][,"KNN_1"],emails_test[[1]][,3002])[1,1] +
   table(emails_test[[1]][,"KNN_1"],emails_test[[1]][,3002])[2,2])/1000,
  (table(emails_test[[2]][,"KNN_1"],emails_test[[2]][,3002])[1,1] +
   table(emails_test[[2]][,"KNN_1"],emails_test[[2]][,3002])[2,2])/1000,
  (table(emails_test[[3]][,"KNN_1"],emails_test[[3]][,3002])[1,1] +
   table(emails_test[[3]][,"KNN_1"],emails_test[[3]][,3002])[2,2])/1000,
  (table(emails_test[[4]][,"KNN_1"],emails_test[[4]][,3002])[1,1] +
   table(emails_test[[4]][,"KNN_1"],emails_test[[4]][,3002])[2,2])/1000,
  (table(emails_test[[5]][,"KNN_1"],emails_test[[5]][,3002])[1,1] +
   table(emails_test[[5]][,"KNN_1"],emails_test[[5]][,3002])[2,2])/1000)


mean(
(table(emails_test[[1]][,"KNN_3"],emails_test[[1]][,3002])[1,1] +
 table(emails_test[[1]][,"KNN_3"],emails_test[[1]][,3002])[2,2])/1000,
(table(emails_test[[2]][,"KNN_3"],emails_test[[2]][,3002])[1,1] +
 table(emails_test[[2]][,"KNN_3"],emails_test[[2]][,3002])[2,2])/1000,
(table(emails_test[[3]][,"KNN_3"],emails_test[[3]][,3002])[1,1] +
 table(emails_test[[3]][,"KNN_3"],emails_test[[3]][,3002])[2,2])/1000,
(table(emails_test[[4]][,"KNN_3"],emails_test[[4]][,3002])[1,1] +
 table(emails_test[[4]][,"KNN_3"],emails_test[[4]][,3002])[2,2])/1000,
(table(emails_test[[5]][,"KNN_3"],emails_test[[5]][,3002])[1,1] +
 table(emails_test[[5]][,"KNN_3"],emails_test[[5]][,3002])[2,2])/1000)

mean(
  (table(emails_test[[1]][,"KNN_5"],emails_test[[1]][,3002])[1,1] +
   table(emails_test[[1]][,"KNN_5"],emails_test[[1]][,3002])[2,2])/1000,
  (table(emails_test[[2]][,"KNN_5"],emails_test[[2]][,3002])[1,1] +
   table(emails_test[[2]][,"KNN_5"],emails_test[[2]][,3002])[2,2])/1000,
  (table(emails_test[[3]][,"KNN_5"],emails_test[[3]][,3002])[1,1] +
   table(emails_test[[3]][,"KNN_5"],emails_test[[3]][,3002])[2,2])/1000,
  (table(emails_test[[4]][,"KNN_5"],emails_test[[4]][,3002])[1,1] +
   table(emails_test[[4]][,"KNN_5"],emails_test[[4]][,3002])[2,2])/1000,
  (table(emails_test[[5]][,"KNN_5"],emails_test[[5]][,3002])[1,1] +
   table(emails_test[[5]][,"KNN_5"],emails_test[[5]][,3002])[2,2])/1000)

mean(
  (table(emails_test[[1]][,"KNN_7"],emails_test[[1]][,3002])[1,1] +
   table(emails_test[[1]][,"KNN_7"],emails_test[[1]][,3002])[2,2])/1000,
  (table(emails_test[[2]][,"KNN_7"],emails_test[[2]][,3002])[1,1] +
   table(emails_test[[2]][,"KNN_7"],emails_test[[2]][,3002])[2,2])/1000,
  (table(emails_test[[3]][,"KNN_7"],emails_test[[3]][,3002])[1,1] +
   table(emails_test[[3]][,"KNN_7"],emails_test[[3]][,3002])[2,2])/1000,
  (table(emails_test[[4]][,"KNN_7"],emails_test[[4]][,3002])[1,1] +
   table(emails_test[[4]][,"KNN_7"],emails_test[[4]][,3002])[2,2])/1000,
  (table(emails_test[[5]][,"KNN_7"],emails_test[[5]][,3002])[1,1] +
   table(emails_test[[5]][,"KNN_7"],emails_test[[5]][,3002])[2,2])/1000)

mean(
  (table(emails_test[[1]][,"KNN_9"],emails_test[[1]][,3002])[1,1] +
   table(emails_test[[1]][,"KNN_9"],emails_test[[1]][,3002])[2,2])/1000,
  (table(emails_test[[2]][,"KNN_9"],emails_test[[2]][,3002])[1,1] +
   table(emails_test[[2]][,"KNN_9"],emails_test[[2]][,3002])[2,2])/1000,
  (table(emails_test[[3]][,"KNN_9"],emails_test[[3]][,3002])[1,1] +
   table(emails_test[[3]][,"KNN_9"],emails_test[[3]][,3002])[2,2])/1000,
  (table(emails_test[[4]][,"KNN_9"],emails_test[[4]][,3002])[1,1] +
   table(emails_test[[4]][,"KNN_9"],emails_test[[4]][,3002])[2,2])/1000,
  (table(emails_test[[5]][,"KNN_9"],emails_test[[5]][,3002])[1,1] +
   table(emails_test[[5]][,"KNN_9"],emails_test[[5]][,3002])[2,2])/1000)


purn <- data.frame(k=c(1,3,5,7,9),Average_accuracy=c(0.825,0.847,0.838,0.838,0.843))

ggplot(purn) +
  aes(x = k, y = Average_accuracy) +
  geom_line(colour = "#112446") +
  theme_minimal()





# Q5

library(dplyr)

train <- emails[1:4000,]
test <- emails[4001:5000,]

dist_test_train <- dist(train[,2:3001],test[,2:3001])
for (j in 1:1000) {
  pred[j] <- mean(train[,3002][topn(dist_test_train[,j], 5,decreasing = F, index = T)])
}
test[,"KNN_5"] <- pred

x <- t(as.matrix(train[,2:3001]))
y <- as.vector(t((train[,3002])))
theta <- rep(0.1,3000)
dtheta <- 1
iter <- 1

while (dtheta > 1e-5 & iter < 2000) {
  sigma <- as.vector(theta %*% x)
  y_hat <- as.vector(1 / (1+exp(-sigma)))
  dy <- as.vector((y_hat - y) %*% t(x))
  #  dtheta <- norm(dy)
  dtheta <- abs(norm(as.matrix(theta),type = "2") - norm(theta - 0.5 /4000 * dy,type = "2"))
  theta <- theta - 0.5 / 4000 * dy
  iter <- iter + 1
}

test[,"logistic"] <- as.vector(1 / (1+exp(-as.vector(theta %*% t(as.matrix(test[,2:3001]))))))

result <- test %>% dplyr::select(Prediction,KNN_5,logistic)

library(ROCit)
par( mfrow= c(2,1) )
ROCit_obj <- rocit(as.numeric(result$logistic),as.factor(result$Prediction))
plot(ROCit_obj)
title("logistic regression")
ROCit_obj <- rocit(as.numeric(result$KNN_5),as.factor(result$Prediction))
plot(ROCit_obj)
title("kNN, k=5")




library(ROCR)

pred <- prediction(result$KNN_5, result$Prediction)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE,lwd = 4)

pred <- prediction(result$logistic, result$Prediction)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE,lwd = 4)











