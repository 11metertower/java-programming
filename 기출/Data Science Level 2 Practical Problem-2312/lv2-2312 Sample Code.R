
library(data.table)
TRN = fread('lv2-2312.csv')
dim(TRN)
str(TRN)

### Question 1 ################################################################
# 1-1
DF24 = copy(TRN[CAT_ID=='C16'])

# 1-2
DF24[, GPRC := ceiling(UPRC/100000)*100000]
DF24[, .(min(UPRC), max(UPRC)), GPRC][order(GPRC)]

# 1-3
DF24 = DF24[, .(SQTY=sum(QTY)), GPRC][order(GPRC)]

# 1-4
DF24[, ELAS := ((shift(SQTY,-1)-SQTY)/SQTY)/((shift(GPRC,-1)-GPRC)/GPRC)]
DF24[, CUMR := cumsum(SQTY)/sum(SQTY)]
DF24

# 1-5
ELAS24 = mean(DF24$ELAS, na.rm=T)
print(ELAS24, digits=16)

# Answer 1
print(abs(ELAS24), digis=16)
ans24 = round(abs(ELAS24), 3)
ans24 

### Question 2 ################################################################
# 2-1
DF25 = copy(TRN[CUST_TP=='NON-MEMBER'&STR_NM=='NORTH', ])
dim(DF25)

# 2-2
DF25[, TRNS_TM := substr(TRNS_TM, 1,2)]

# 2-3
DF5 = DF25[, .N, .(TRNS_DT, TRNS_TM)]
head(DF5)
n = DF5[, .N]; n
n

# 2-4
L5 = round(DF5[, mean(N)],1); L5

# 2-5
A5 = exp(-L5)*L5^2/factorial(2); A5 # Method 1: exp(-L5)*L5^2/factorial(2)
A5 = dpois(2, lambda=L5); A5        # Method 2: dpois(2, lambda=L5)

# 2-6
UB = L5 + 1.96*sqrt(L5/n); UB
LB = L5 - 1.96*sqrt(L5/n); LB
B5 = abs(UB - LB)
B5

# Answer 2
print(A5+B5, digits=16)
ans25 = round(A5+B5,3)
ans25

### Question 3 ################################################################
# 3-1
TRN[, AMT := UPRC*QTY]

# 3-2
DF6 = TRN[, .(SAMT=sum(AMT)), .(TRNS_DT, TRNS_DOW, STR_NM, CAT_ID)]
DF6 = DF6[order(TRNS_DT, STR_NM, CAT_ID)]
dim(DF6)

# 3-3
DF6[, TAMT := ifelse(SAMT>median(SAMT), 'H','L'), CAT_ID]
dim(DF6)

# 3-4
library(caret)
DF6$TRNS_DOW = as.character(DF6$TRNS_DOW)
DF6$CAT_ID = as.character(DF6$CAT_ID)
dummer = dummyVars(~.,DF6[,.(TRNS_DOW, STR_NM, CAT_ID)])
dummed = predict(dummer, DF6)
dum_nm = colnames(dummed)
dum_nm
length(dum_nm)
DF6 = cbind(DF6, dummed)

# 3-5
DF6[, RN := .I]
TRAIN6 = DF6[RN%%6!=0][, -c('RN')]; dim(TRAIN6)
TEST6  = DF6[RN%%6==0][, -c('RN')]; dim(TEST6)

# 3-6
library(naivebayes)
mdl26 = multinomial_naive_bayes(as.matrix(TRAIN6[, ..dum_nm]), TRAIN6$TAMT)
mdl26

# 3-7
TEST6$PRED = predict(mdl26, as.matrix(TEST6[, ..dum_nm]))
ACC6 = as.numeric(TEST6[, .(ACC=mean(TAMT==PRED)*100)])

# Answer 3
print(ACC6, digits=16)
ans26 = round(ACC6, 2)
ans26

### Summary ####################################################################
ans24; ans25; ans26

################################################################################