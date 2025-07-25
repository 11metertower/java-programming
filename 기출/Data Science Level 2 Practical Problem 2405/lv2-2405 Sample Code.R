
library(data.table)
library(dplyr)
FAC = fread('lv2-2405.csv')
str(FAC)
dim(FAC)
summary(FAC)

### Question 1 ################################################################
# 1-1
FAC0 = copy(FAC[FAIL_YN==0])
FAC1 = copy(FAC[FAIL_YN==1])
dim(FAC0); dim(FAC1)

# 1-2
cvval = c('TRQ_PWR','DUR','NS','TEMP_DIFF')
CV0 = FAC0[, sapply(.SD, function(x) {sd(x)/mean(x)}), .SDcols=cvval]
CV1 = FAC1[, sapply(.SD, function(x) {sd(x)/mean(x)}), .SDcols=cvval]
CV0; CV1

# 1-3
CV_diff = abs(CV0-CV1)
CV_diff
ans24_0 = max(CV_diff)
print(ans24_0, digits=16)

# Answer 1
ans24 = round(ans24_0, 3)
ans24

### Question 2 ################################################################
# 2-1
DF5 = copy(FAC[!is.na(ROT_SPD)])
dim(DF5)

# 2-2
library(caret)
zvar = c('ROT_SPD','TRQ_PWR','DUR','NS','TEMP_DIFF')
zvars = paste0(zvar,'_S')
scaler = preProcess(DF5[,..zvar], method=c('scale','center'))
DF5s = predict(scaler, DF5)
colnames(DF5s) = ifelse(colnames(DF5s)%in%zvar, zvars, colnames(DF5s))
DF5s

# 2-3
set.seed(1234); DF5s$clst = kmeans(DF5s[,..zvars], 3, nstart=5)$cluster
table(DF5s$clst)

# 2-4
DF5o = DF5s[, .(FAIL_RT = 100*mean(FAIL_YN==1), N=.N), clst]
DF5o[order(FAIL_RT)]
FAIL_RT_DIFF = DF5o[, abs(max(FAIL_RT)-min(FAIL_RT))]
print(FAIL_RT_DIFF, digits=16)

# Answer 2
ans25 = round(FAIL_RT_DIFF, 2)
ans25

### Question 3 ################################################################
# 3-1
DF6 = copy(FAC)
DF6[, ROT_SPD := ifelse(is.na(ROT_SPD), median(ROT_SPD, na.rm=T), ROT_SPD)]
summary(DF6$ROT_SPD)

# 3-2
TRAIN6 = copy(DF6[PRD_ID%%3!=0])
TEST6 = copy(DF6[PRD_ID%%3==0])
dim(TRAIN6); dim(TEST6)

# 3-3
library(rpart)
TRAIN6$FAIL_YN = as.factor(TRAIN6$FAIL_YN)
VAR6 = c('ROT_SPD','TRQ_PWR','DUR','NS','TEMP_DIFF')
FM6 = as.formula(paste0('FAIL_YN~',paste0(VAR6, collapse='+')))
D = c(3,5,7)
set.seed(1234); RP3 = rpart(FM6, TRAIN6, maxdepth=D[1], cp=0, minsplit=3)
set.seed(1234); RP5 = rpart(FM6, TRAIN6, maxdepth=D[2], cp=0, minsplit=3)
set.seed(1234); RP7 = rpart(FM6, TRAIN6, maxdepth=D[3], cp=0, minsplit=3)

# 3-4
TRAIN6$PRED3 = predict(RP3, TRAIN6, type='class')
TRAIN6$PRED5 = predict(RP5, TRAIN6, type='class')
TRAIN6$PRED7 = predict(RP7, TRAIN6, type='class')
RECALL = c(TRAIN6[FAIL_YN==1, mean(FAIL_YN==PRED3)],
           TRAIN6[FAIL_YN==1, mean(FAIL_YN==PRED5)],
           TRAIN6[FAIL_YN==1, mean(FAIL_YN==PRED7)])
names(RECALL) = D
RECALL
FIN_TXT = paste0('FIN_RP = RP',names(which.max(RECALL)))
print(FIN_TXT)
eval(parse(text=FIN_TXT))

# 3-5
TEST6$PRED = predict(FIN_RP, TEST6, type='class')
table(TEST6$PRED, TEST6$FAIL_YN)
RECALL6 = TEST6[FAIL_YN==1, mean(FAIL_YN==PRED)]
print(RECALL6, digits=16)

# Answer 3
ans26 = round(RECALL6, 3)
ans26

################################################################################
##### Summary
ans24; ans25; ans26