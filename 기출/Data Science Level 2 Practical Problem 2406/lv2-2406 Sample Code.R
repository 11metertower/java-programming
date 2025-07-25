

library(data.table)
library(dplyr)

raw00 = fread("lv2-2406.csv")
dim(raw00)

### Question 1
df210 = copy(raw00)

# 1-1
list_ent = c()
for(col in c('PART02','PART03','PART04','PART05','PART06')){
  e01 <- 0
  for(i in c('A','B')){
    e00 <- 0
    for(j in c('G','B')){
      p00 <- sum((df210[[col]]==i) & (df210$CLASS==j))/sum(df210[[col]]==i)
      e00 <- e00 - p00*log2(p00)
    } 
    e01 <- e01 + (sum(df210[[col]]==i)/dim(df210)[1])*e00  
  }
  list_ent <- c(list_ent, c(e01))
}

list_ent

# 1-2
res21 <- min(list_ent) + max(list_ent)
ans21 <- round(res21, 2)
options(digits=10)
print(res21)
print(ans21)

### 2
df220 <- copy(raw00)

# 2-1
list_cols <- c('PART00','PART01','PART07','PART08','PART09')

library(caret)
df_dum <- df220[,..list_cols]
obj00 <- dummyVars(~., df_dum, sep="_", fullRank=TRUE)
df_dum <- predict(obj00, df_dum)  
df221 <- cbind(as.data.frame(df220$CLASS), df_dum)

list_dum <- colnames(df_dum)
colnames(df221) <- c(c('CLASS'), list_dum)

# 2-2
list_inf <- c()
for(col in list_dum){
  pa0 <- sum((df221[[col]]==0) & (df221$CLASS=='G'))/sum(df221[[col]]==0)
  pa1 <- sum((df221[[col]]==1) & (df221$CLASS=='G'))/sum(df221[[col]]==1)
  val00 <- max(pa0/(pa1+0.01), pa1/(pa0+0.01))
  list_inf <- c(list_inf, c(val00))
}
list_inf

res22 <- mean(rev(sort(list_inf))[1:10])
ans22 <- round(res22, 3)
print(res22)
print(ans22)

### 3
df230 <- copy(raw00)

# 3-1
list_cols = c('PART10','PART11','PART12','PART13')

library(caret)
df_dum <- df230[,..list_cols]
obj00 <- dummyVars(~., df_dum, sep="_", fullRank=TRUE)
df_dum <- predict(obj00, df_dum)  
df231 <- cbind(as.data.frame(df220$CLASS), df_dum)

list_dum <- colnames(df_dum)
colnames(df231) <- c(c('CLASS'), list_dum)

# 3-2
train230 <- df231[df230$PRD_ID %% 5!=0, ]
test230 <- df231[df230$PRD_ID %% 5==0, ]

# 3-3
library(rpart)
train230$CLASS <- as.factor(train230$CLASS)
fm23 = as.formula(paste0('CLASS~',paste0(list_dum, collapse='+')))
set.seed(1234); DT00 = rpart(fm23, train230, maxdepth=9, cp=0)

# 3-4
srs_col <- list_dum
srs_imp <- as.vector(DT00$variable.importance)
df_imp <- as.data.frame(cbind(srs_col, srs_imp))
df_imp$srs_imp <- as.numeric(df_imp$srs_imp)

# 3-5
list_acc <- c()
test230$CLASS <- as.factor(test230$CLASS)
for(i in c(5, 10, 14)){
  list_vars <- df_imp$srs_col[1:i]
  fm23 <- as.formula(paste0('CLASS~',paste0(list_vars, collapse='+')))
  set.seed(1234); DT01 <- rpart(fm23, train230, maxdepth=9, cp=0)
  arr_pred <- as.vector(predict(DT01, test230[,list_vars], type='class'))
  arr_act <- as.character(test230$CLASS)
  acc00 <- sum(arr_pred==arr_act)/dim(test230)[1]
  list_acc <- c(list_acc, acc00)
}
res23<-mean(list_acc)
ans23<-round(res23, 3)
print(res23)
print(ans23)


### Summary
print(paste('Q1 Result:', ans21))
print(paste('Q2 Result:', ans22))
print(paste('Q3 Result:', ans23))
