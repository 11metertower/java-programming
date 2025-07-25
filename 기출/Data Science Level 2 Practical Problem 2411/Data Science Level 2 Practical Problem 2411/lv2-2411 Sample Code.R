
library(data.table)
library(caret)
SALES = fread('lv2-2411.csv')
dim(SALES)
SALES

### Question 1 ################################################################
# 1-1
cors = cor(SALES[, .(SALES, MD1, MD2, MD3, TEMP, FUEL, CPI, UNEMP)])[1, -1]
abs_cors = sort(abs(cors), decreasing=T)
abs_cors

# 1-2
max_abs_cors = max(abs_cors)
max_abs_cors

# Answer 1
ans21f = max_abs_cors
ans21 = round(ans21f, 3)
print(ans21f, digits=16)
print(ans21)

### Question 2 ################################################################
# 2-1
DF22 = copy(SALES[DT=='2024-04-22'])
dim(DF22)

# 2-2
feats = c('SALES', 'MD1', 'MD2', 'MD3', 'TEMP', 'FUEL', 'CPI', 'UNEMP')
featss = paste0(feats, '_S')
std = preProcess(DF22[,..feats], method=c('scale','center'))
DF22s = predict(std, DF22[,..feats])
colnames(DF22s) = featss
DF22 = cbind(DF22, DF22s)

# 2-3
set.seed(1234); km = kmeans(DF22[,..featss], 4)
DF22$cluster = km$cluster

# 2-4
DF22_AGG = DF22[, .(.N, SALES_M = mean(SALES), UNEMP_M = mean(UNEMP)), cluster]
DF22_AGG

# Answer 2
ans22f = DF22_AGG[which.max(DF22_AGG$SALES_M), UNEMP_M]
ans22 = round(ans22f, 3)

print(ans22f, digits=16)
print(ans22)


### Question 3 ################################################################
# 3-1
DF23 = copy(SALES)
DF23[, SEASON1 := 1*(substring(DF23$DT, 6, 7)%in%c('03','04','05'))]
DF23[, SEASON2 := 1*(substring(DF23$DT, 6, 7)%in%c('06','07','08'))]
DF23[, SEASON3 := 1*(substring(DF23$DT, 6, 7)%in%c('09','10','11'))]

# 3-2
TRAIN23 = DF23[TT=='TRAIN']; dim(TRAIN23)
TEST23 = DF23[TT=='TEST']; dim(TEST23)

# 3-3
form23 = as.formula('SALES~HOLI+MD1+MD2+MD3+TEMP+FUEL+CPI+UNEMP+SEASON1+SEASON2+SEASON3')
mdl23 = lm(form23, TRAIN23)

# 3-4
TEST23$PRED = predict(mdl23, TEST23)
MAPE23 = TEST23[, mean(abs((PRED-SALES)/SALES))*100]

# Answer 3
ans23f = MAPE23
ans23 = round(ans23f, 2)

print(ans23f, digits=16)
print(ans23)

### Summary ####################################################################
ans21; ans22; ans23

################################################################################
