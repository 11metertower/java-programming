library(data.table)
library(dplyr)

SURVEY = fread('lv2-2403.csv', encoding='UTF-8')
dim(SURVEY)
str(SURVEY)

##### Question 1
# 1-1
DF1 = copy(SURVEY[!is.na(SALARY)])
dim(DF1)

# 1-2
AVG1 = mean(DF1$SALARY); AVG1 

# 1-3
hist(DF1$SALARY)
DF1[, m := mean(SALARY), YEARS]
DF1[, s := sd(SALARY), YEARS]
DF1_out = DF1[(SALARY>=m-1.5*s)&(SALARY<=m+1.5*s)]
nrow(DF1)-nrow(DF1_out)

# 1-4
AVG2 = mean(DF1_out$SALARY); AVG2
abs(AVG2-AVG1)

# Answer 1
ans21f = abs(AVG2-AVG1)
print(ans21f, digits=16)
ans21 = round(ans21f)
print(ans21)


##### Question 2
# 2-1
DF2 = copy(SURVEY[AGE=='25-34 years old'])
dim(DF2)

SURVEY
# 2-2
DF2[, SALARY := ifelse(is.na(SALARY), median(SALARY, na.rm=T), SALARY), YEARS]
DF2 = DF2[order(D_ID)]
sum(is.na(DF2$SALARY))

# 2-3
library(stringr)
DF2[, TOOL_CNT := str_count(TOOL, ';')+1]

# 2-4
library(caret)
SVAR = c('SATIS','SALARY','MONITORS','TOOL_CNT')
scaler = preProcess(DF2[, ..SVAR], method=c('center','scale'))
scaled = predict(scaler, DF2[,..SVAR])
SVAR_S = paste0(SVAR,'_S')
colnames(scaled) = SVAR_S
DF2 = cbind(DF2, scaled)

# 2-5
set.seed(1234)
km = kmeans(DF2[,..SVAR_S], 2)
DF2$clst = km$cluster

# 2-6
GRATIO = DF2[, .(G=sum(GENDER=='Male')/sum(GENDER=='Female')*100), clst]
GRATIO

# Answer 2
ans22f = abs(diff(GRATIO$G))
print(ans22f, digits=16)
ans22 = round(ans22f, 2)
ans22


##### Question 3
# 3-1
DF3 = copy(SURVEY[(!is.na(SALARY))&(GENDER!='')])
dim(DF3)

# 3-2
library(stringr)
DF3[, GIT_YN := 1*str_detect(TOOL, 'Git')]
length(DF3[GIT_YN==1, unique(TOOL)])

# 3-3
DVAR = c('YEARS','GENDER','AGE')
DF3$YEARS = as.factor(DF3$YEARS); sort(unique(DF3$YEARS))
DF3$GENDER = as.factor(DF3$GENDER); sort(unique(DF3$GENDER))
DF3$AGE = as.factor(DF3$AGE); sort(unique(DF3$AGE))
dummer = dummyVars(~., DF3[,..DVAR], sep='.')
dummed = predict(dummer, DF3)
colnames(dummed) = gsub(' ','',colnames(dummed))
colnames(dummed) = gsub('-','_',colnames(dummed))
colnames(dummed)
DF3 = cbind(DF3, dummed)

# 3-4
TRAIN3 = DF3[D_ID%%3!=0]; dim(TRAIN3)
TEST3  = DF3[D_ID%%3==0]; dim(TEST3)

# 3-5
library(rpart)
RVAR = c('HOBBY','OPENSOURCE','CORPORATE','SALARY','MONITORS','GIT_YN',colnames(dummed))
RVAR; length(RVAR)
FM3 = as.formula(paste('SATIS',paste(RVAR,collapse='+'),sep='~')); FM3
set.seed(1234); MDL3 = rpart(FM3, TRAIN3, maxdepth = 7, cp = 0)

# 3-6
TEST3$PRED = predict(MDL3, TEST3)
MAPE = TEST3[, 100*mean(abs((PRED-SATIS)/SATIS))]
MAPE

# Answer 3
ans23f = MAPE
print(ans23f, digits=16)
ans23 = round(ans23f, 1)
ans23


####### Summary ################################################################
ans21; ans22; ans23