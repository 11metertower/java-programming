
library(data.table)
library(caret)
EXPT = fread('lv2-2410.csv')
EXPT

### Question 1 ################################################################
# 1-1
cols = c('MOTOR_SPEED','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11')
m = EXPT[, sapply(.SD, mean), .SDcols=cols]; m
s = EXPT[, sapply(.SD, sd), .SDcols=cols]; s
cv = s/m; cv

# 1-2
cv_abs = abs(cv); cv_abs

# Answer 1
ans24f = max(cv_abs); print(ans24f, digits=16)
ans24 = round(ans24f, 2); ans24


### Question 2 ################################################################
# 2-1
DF25 = copy(EXPT)
DF25[, HIGH_SPEED := 1*(MOTOR_SPEED>1)]

# 2-2
TRAIN25 = DF25[PID%%5!=0]; dim(TRAIN25)
TEST25 = DF25[PID%%5==0]; dim(TEST25)

# 2-3
library(rpart)
TRAIN25$HIGH_SPEED = as.factor(TRAIN25$HIGH_SPEED)
form25 = as.formula('HIGH_SPEED ~ F1+F2+F3+F4+F5+F6+F7+F8+F9+F10+F11')
set.seed(1234); mdl25 = rpart(form25, data=TRAIN25, cp=0, maxdepth=3)

# 2-4
TEST25$PRED = predict(mdl25, TEST25, type='class')
ftable(TEST25$HIGH_SPEED, TEST25$PRED)
rec = TEST25[HIGH_SPEED==1, mean(HIGH_SPEED==PRED)]; rec

# Answer 2
ans25f = rec; print(ans25f, digits=16)
ans25 = round(ans25f, 3); ans25

### Question 3 ################################################################
# 3-1
DF260 = copy(EXPT)
cols = c('F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8','F9', 'F10', 'F11')
DF260$OVER = rowSums(DF260[, sapply(.SD, function(x) 1*(x>1.5)), .SDcols=cols])
DF26 = copy(DF260[OVER==0])
dim(DF26)

# 3-2
TRAIN26 = DF26[PID%%6!=0]; dim(TRAIN26)
TEST26 = DF26[PID%%6==0]; dim(TEST26)

# 3-3
form26 = as.formula('MOTOR_SPEED ~ F1+F2+F3+F4+F5+F6+F7+F8+F9+F10+F11')
mdl26 = lm(form26, data=TRAIN26)
mdl26

# 3-4
TEST26$PRED = predict(mdl26, TEST26)
MAE = TEST26[, mean(abs(PRED-MOTOR_SPEED))]; MAE

# Answer 3
ans26f = MAE; print(ans26f, digits=16)
ans26 = round(ans26f, 3); ans26


### Summary ####################################################################
ans24; ans25; ans26