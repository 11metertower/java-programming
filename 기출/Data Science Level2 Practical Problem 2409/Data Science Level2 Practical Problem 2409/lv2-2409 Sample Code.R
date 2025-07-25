
##### Set 1 ####################################################################
library(data.table)
raw01 = fread('lv2-2409.csv')
dim(raw01)

list_fts <- c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11')

#### Question 1 ###############################################################
df210 <- copy(raw01)

# 1-1
list_diff <- c()
for(col in list_fts){
  seq00 <- df210[df210$DEFECT==0,][[col]]
  seq01 <- df210[df210$DEFECT==1,][[col]]
  Q01 <- as.vector(quantile(seq00, 0.25))
  Q02 <- as.vector(quantile(seq00, 0.5))
  Q03 <- as.vector(quantile(seq00, 0.75))
  Q11 <- as.vector(quantile(seq01, 0.25))
  Q12 <- as.vector(quantile(seq01, 0.5))
  Q13 <- as.vector(quantile(seq01, 0.75))
  diff <- abs(Q01-Q11) + abs(Q02-Q12) + abs(Q03-Q13)
  list_diff = c(list_diff, diff)
}

# 1-2
res21 <- max(list_diff)
ans21 <- round(res21, 0)
print(res21, digits=16)
print('ans21')
print(ans21, digits=16)

#### Question 2 ###############################################################
df220 <- copy(raw01)

# 2-1
for(col in list_fts){
  max00 <- max(df220[[col]])
  min00 <- min(df220[[col]])
  df220[[col]] <- (df220[[col]]-min00)/(max00-min00)
}

# 2-2
list_sd <- c()
for(col in list_fts){
  sd00 <- sd(df220[[col]])
  list_sd <- c(list_sd, sd00)
}
df_tmp <- as.data.frame(cbind(list_fts, list_sd))
VAR2 <- df_tmp[order(-list_sd),][1:2,'list_fts']; VAR2

# 2-3
cor_val <- cor(df220[,..VAR2])[1,2]; cor_val
cor_abs <- abs(cor_val); cor_abs

res22 <- cor_abs
ans22 <- round(res22, 3)
print(res22, digits=16)
print('ans22')
print(ans22)

#### Question 3 ###############################################################
df231 <- copy(raw01)

# 3-1
library(caret)
scl00 <- preProcess(df231[,..list_fts], method=c('center','scale'))
df_tmp <- predict(scl00, df231[,..list_fts])
df232 <- cbind(df231[,c('PROD_ID','DEFECT')],df_tmp)

# 3-2
train23 <- df232[df232$PROD_ID%%5!=0]
test23 <- df232[df232$PROD_ID%%5 ==0]
dim(train23)
dim(test23)

# 3-3, 4
library(class)
list_acc = c()
for(k_ in c(3,5,7)){
  set.seed(1234)
  arr_pred <- knn(train=train23[,..list_fts], test= test23[,..list_fts],
                  cl=train23$DEFECT, k=k_)
  arr_act <- test23$DEFECT
  acc <- sum(arr_pred==arr_act)/length(arr_pred)
  list_acc <- c(list_acc, acc)
}
list_acc
k_fin <- c(3,5,7)[which.max(list_acc)]

# 3-5
mean_acc <- mean(list_acc)

res23 <- mean_acc
ans23 <- round(res23, 3)
print(res23, digits=16)
print('ans23')
print(ans23)


#### Summary ###################################################################
print(ans21)
print(ans22)
print(ans23)