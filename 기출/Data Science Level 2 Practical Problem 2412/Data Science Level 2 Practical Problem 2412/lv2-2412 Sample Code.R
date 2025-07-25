library(data.table)
set2 = fread('lv2-2412.csv')
dim(set2)

#### Question 1 ###############################################################
df240 <- copy(set2)

# 1-1
DF40_A <- df240[df240$MOTOR_TYPE=='A']
DF40_B <- df240[df240$MOTOR_TYPE=='B']

# 1-2
list_diff = c()
for(col in c('ENERGY_CONS','WATER_CONS','NOISE')){
  list_col <- c(col, 'SCORE')
  COR_A <- cor(DF40_A[,..list_col])[1, 2]
  COR_B <- cor(DF40_B[,..list_col])[1, 2]
  diff00 <- abs(COR_A-COR_B)
  list_diff = c(list_diff, diff00)
}

# 1-3
sum(list_diff)

# Answer 1
res24 <- sum(list_diff)
ans24 <- round(res24, 3)
print(res24, digits=16)
ans24


#### Question 2 ###############################################################
df250 <- copy(set2)

# 2-1
library(caret)
list_fts = c('WEIGHT','ENERGY_CONS','WATER_CONS','SPIN_SPEED','NOISE')
preprocessed_data <- preProcess(df250[,..list_fts], method=c("center","scale"))
standardized_data <- predict(preprocessed_data, df250[,..list_fts])

list_fts_s = c()
for(col in list_fts){
  list_fts_s <- c(list_fts_s, paste(col, '_S', sep=""))
}

colnames(standardized_data) <- list_fts_s
df251 <- cbind(df250, standardized_data)

# 2-2, 2-3
library(kmeans)
c<-3
set.seed(1234)
km00 <- kmeans(df251[,..list_fts_s], centers = c)
srs00 <- km00$cluster

list_mean = c()
for(cls in 1:c){
  list_mean = c(list_mean, mean(unlist(df251[srs00==cls, 'SCORE'])))
}
max(list_mean)

# Answer 2
res25 <- max(list_mean)
ans25 <- round(res25, 2)
print(res25, digits=16)
ans25


#### Question 3 ###############################################################
df260 = copy(set2)

# 3-1
library(fastDummies)
list_col <- c('MOTOR_TYPE','MATERIAL')
df_dum <- dummy_cols(df260[,..list_col], remove_first_dummy = TRUE)
df_dum <- df_dum[, 3:ncol(df_dum)]
list_dum <- colnames(df_dum)
df261 <- cbind(df260, df_dum)

# 3-2
train26 <- df261[df261$PRD_ID%%5 !=0]
test26 <- df261[df261$PRD_ID%%5 ==0]

# 3-3
list_fts <- c(c('WEIGHT','ENERGY_CONS','WATER_CONS','SPIN_SPEED','NOISE'),list_dum)
formula <- as.formula(paste("SCORE ~ ", paste(list_fts, collapse = " + ")))
LR00 <- lm(formula, data = train26)

# 3-4
arr_pred <- predict(LR00, test26[,..list_fts])
arr_act <- test26$SCORE
mse <- mean((arr_pred-arr_act)**2)

# Answer 3
res26 <- mse
ans26 <- round(res26, 3)
print(res26, digits=16)
ans26

#### Summary ###################################################################
print(ans24)
print(ans25)
print(ans26)