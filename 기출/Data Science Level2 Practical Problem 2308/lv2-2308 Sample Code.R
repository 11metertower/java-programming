library(data.table)
library(dplyr)

options(digits=16)
df_raw01 = fread("lv2-2308.csv")
dim(df_raw01)
str(df_raw01)

#### Question 1
df210 = copy(df_raw01)

#1-1
sc00 <- caret::preProcess(df210[, 2:15], method = c('center','scale'))
df211 <- predict(sc00, df210)

summary(df211)

#1-2
df_tmp00 <- df211[df211$EYE_CLOSED==0,2:15]
df_tmp01 <- df211[df211$EYE_CLOSED==1,2:15]

df_mean00<-sapply(df_tmp00, mean)
df_mean01<-sapply(df_tmp01, mean)
df_abs <- abs(df_mean00 - df_mean01)

df_abs <- as.data.frame(df_abs)
df_abs <- cbind(newColName = rownames(df_abs), df_abs)
df_abs02 <- as.data.frame(df_abs[order(df_abs$df_abs, decreasing = TRUE), ])

list_03 = df_abs02[1:3, 'newColName']
list_11 = df_abs02[4:14, 'newColName']

# 1-3
df212 <- df211[df211$EYE_CLOSED==1,]


numeric(length=19)


# 1-4
vec_col00 <- c()
vec_col01 <- c()
vec_mean <- c()

for (col00 in list_03){
  for (col01 in list_11){
    arr_tmp00 <- numeric(length=19)
    arr_tmp01 <- numeric(length=19)
    srs_tmp00 <- df212[,..col00]
    srs_tmp01 <- df212[,..col01]
    for (i in 1:19){
      num_q = 0.05 + (i-1)*0.05
      arr_tmp00[i] = quantile(as.data.frame(srs_tmp00)[,1], num_q)
      arr_tmp01[i] = quantile(as.data.frame(srs_tmp01)[,1], num_q)
    }
    num_mean00 = mean(abs(arr_tmp00 - arr_tmp01))
    
    vec_col00 <- append(vec_col00, col00)
    vec_col01 <- append(vec_col01, col01)
    vec_mean <- append(vec_mean, num_mean00)
  }
}

df_tmp00 <- data.frame(vec_col00, vec_col01, vec_mean)
df_tmp01 <- df_tmp00[order(df_tmp00$vec_mean , decreasing = FALSE),]
str_col <- df_tmp01[1, 2]

res21 <- mean(as.data.frame(df210[,..str_col])[,1])
ans21 <- round(res21, 2)


#### Question 2
df220 <- copy(df_raw01)

# 2-1
list_cols <- colnames(df220[,2:15])

num_mean <- sapply(df220[,2:15], mean)
num_sd <- sapply(df220[,2:15], sd)
num_cv <- num_sd/num_mean

df_cv <- as.data.frame(num_cv)

df_cv02 <- cbind(fts = rownames(df_cv), df_cv)
rownames(df_cv02) <- 1:nrow(df_cv02)
df_cv03 <- df_cv02[order(df_cv02$num_cv, decreasing = TRUE),]

list_05 <- df_cv03[1:5,'fts']
df221 <- as.data.frame(df220[, ..list_05])


# 2-2
df222 <- copy(df221)

for (col in 1:ncol(df222)){
  num_q1 <- quantile(df221[,col], 0.25)
  print(num_q1)
  num_q3 <- quantile(df221[,col], 0.75)
  num_iqx <- num_q3 - num_q1
  # print(dim(df222))
  df222 <- df222[(df222[,col]<=num_q3 + 1.5*num_iqx) & 
                   (df222[,col]>=num_q1 - 1.5*num_iqx),]
}

dim(df222)

# 2-3
for (col in 1:ncol(df222)){
  df222[,col] <- (df222[,col] - min(df222[,col]))/
    (max(df222[,col]) - min(df222[,col]))
}

# 2-4
for (i in 1:5){
  set.seed(1234)
  cl <- kmeans(df222[, -i], centers = 2, nstart=150)
  print(nrow(df222[cl$cluster==1,]))
  print(mean(as.data.frame(df220[match(rownames(df222[cl$cluster==1,]), 
                                       rownames(df220)), 'EYE_CLOSED'])[,]))
  print(nrow(df222[cl$cluster==2,]))
  print(mean(as.data.frame(df220[match(rownames(df222[cl$cluster==2,]), 
                                       rownames(df220)), 'EYE_CLOSED'])[,]))
}

ans22 <- round(5268/nrow(df222),2)
  


#### Question 3
df230 <- copy(df_raw01)

# 3-1
train230 <- df230[df230$PTC_ID%%5!=0 ,]
test230 <- df230[df230$PTC_ID%%5==0 ,]

dim(train230)
dim(test230)

# 3-2

train231 <- as.data.frame(train230[,2:15])

vec_fts = c()
cols00 <- colnames(train231)
for (i in 1:14){
  lm00 <- lm(paste(cols00[i], "~."), data=train231)
  num_r2 <- summary(lm00)$adj.r.squared
  vec_fts <- append(vec_fts, num_r2)
}

list_06 <- cols00[vec_fts<=0.95]


# 3-3

train232 <- as.data.frame(train230[,..list_06])
train232 <- cbind(train232, train230[, 'EYE_CLOSED'])
test232 <- as.data.frame(test230[,..list_06])
test232 <- cbind(test232, test230[, 'EYE_CLOSED'])

set.seed(1234)
log00 <- glm(EYE_CLOSED~.,data=train232,family=binomial)

arr_pred <- as.vector(predict(log00, test232, type='response'))

for (i in 1:length(arr_pred)){
  if (arr_pred[i] >=0.5){
    arr_pred[i] <- 1
  }
  else if (arr_pred[i] < 0.5){
    arr_pred[i] <- 0
  }
}

arr_act <- as.vector(test232[,'EYE_CLOSED'])

res23 <- sum((arr_pred==1) & (arr_act==1))/sum((arr_act==1))
ans23 <- round(res23, 3)

ans23


### ANSWER SUMMARY #############################################################
ans21; ans22; ans23
