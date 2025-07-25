library(data.table)
set1 = fread('lv2-2311.csv')

#Q1
df1 = copy(set1)
df1
p_list = unique(df1$PRODUCT_TYPE)
c_name = colnames(df1)
tmp = data.table(matrix(nrow = 0, ncol = length(c_name)))
colnames(tmp) = c_name

for (product in p_list){
  tmp1 = df1[PRODUCT_TYPE == product]
  q1_t = quantile(tmp1[, SHIPPING_TIME], .25)
  q3_t = quantile(tmp1[, SHIPPING_TIME], .75)
  iqr_t = q3_t - q1_t
  
  q1_c = quantile(tmp1[, SHIPPING_COST], .25)
  q3_c = quantile(tmp1[, SHIPPING_COST], .75)
  iqr_c = q3_c - q1_c
  tmp2 = tmp1[(SHIPPING_TIME >= q1_t - iqr_t) &  (SHIPPING_TIME <= q3_t + iqr_t)]
  tmp3 = tmp2[(SHIPPING_COST >= q1_c - iqr_c) &  (SHIPPING_COST <= q3_c + iqr_c)]  
  
  tmp = rbind(tmp3, tmp)
} 

a = tmp[,mean(SHIPPING_COST),PRODUCT_TYPE]
b = tmp[,mean(SHIPPING_TIME),PRODUCT_TYPE]
a[,SHIPPING_TIME:= b$V1]

a[, RATIO:= V1 / SHIPPING_TIME]
ans24 = min(a$RATIO)
print(paste('Q1 Result:', ans24))
print(paste('Q1 Answer:', round(ans24, 3)))

# Q2
df2 = copy(set1)
library(caret)
z_scaler = preProcess(df2[,c('PRODUCT_SOLD', 'REVENUE', 'PRODUCT_VOL','MANUFACTURING_TIME','MANUFACTURING_COST')], method = c('center','scale'))
df2[,c('PRODUCT_SOLD_S', 'REVENUE_S', 'PRODUCT_VOL_S','MANUFACTURING_TIME_S','MANUFACTURING_COST_S') := predict(z_scaler, df2)[,c('PRODUCT_SOLD', 'REVENUE', 'PRODUCT_VOL','MANUFACTURING_TIME','MANUFACTURING_COST')]]

n_scaler = preProcess(df2[,c('PRODUCT_SOLD', 'REVENUE', 'PRODUCT_VOL','MANUFACTURING_TIME','MANUFACTURING_COST')], method = c('range'))
df2[,c('PRODUCT_SOLD_N', 'REVENUE_N', 'PRODUCT_VOL_N','MANUFACTURING_TIME_N','MANUFACTURING_COST_N') := predict(n_scaler, df2)[,c('PRODUCT_SOLD', 'REVENUE', 'PRODUCT_VOL','MANUFACTURING_TIME','MANUFACTURING_COST')]]

library(cluster)

set.seed(100)
df2[,z_cluster:= kmeans(df2[,c('PRODUCT_SOLD_S', 'REVENUE_S', 'PRODUCT_VOL_S','MANUFACTURING_TIME_S','MANUFACTURING_COST_S')], centers = 3, nstart = 100)[1]]

set.seed(101)
df2[,n_cluster:= kmeans(df2[,c('PRODUCT_SOLD_N', 'REVENUE_N', 'PRODUCT_VOL_N','MANUFACTURING_TIME_N','MANUFACTURING_COST_N')], centers = 3, nstart = 100)[1]]

df2[,.N,z_cluster]
df2[,.N,n_cluster]

table(df2$z_cluster)
zmax = which.max(table(df2$z_cluster))
table(df2$n_cluster)
mmax = which.max(table(df2$n_cluster))

res1 = df2[z_cluster == zmax & n_cluster == mmax ]

A = nrow(res1[PRODUCT_TYPE == 'PC' ]) / nrow(res1)

ans25 = A
print(paste('Q2 Result:', ans25))
print(paste('Q2 Answer:', round(ans25, 3)))


# Q3
df3 = copy(set1)
train = df3[SCM_NO %% 5 != 0]
test = df3[SCM_NO %% 5 == 0]


train = train[,c('PRODUCT_SOLD', 'SHIPPING_TIME', 'SHIPPING_COST', 'PRODUCT_VOL', 'MANUFACTURING_TIME', 'MANUFACTURING_COST','TOTAL_COST')]
test = test[,c('SCM_NO', 'PRODUCT_SOLD', 'SHIPPING_TIME', 'SHIPPING_COST', 'PRODUCT_VOL', 'MANUFACTURING_TIME', 'MANUFACTURING_COST','TOTAL_COST')]

lr = lm(TOTAL_COST~., train)
summary(lr)
confint(lr, level = 0.95)
C = confint(lr)['SHIPPING_COST',1]; C
D = confint(lr)['SHIPPING_COST',2]; D

test1 = test[SCM_NO == 870]
test1 = test1[,-c('SCM_NO')]

E = predict(lr, test1)

ans26 = abs(C) + abs(D) + abs(E)

print(paste('Q3 Result:', ans26))
print(paste('Q3 Answer:', round(ans26, 1)))




### Summary
print(paste('Q1 Result:', ans24))
print(paste('Q2 Result:', ans25))
print(paste('Q3 Result:', ans26))

print(paste('Q1 Answer:', round(ans24, 3)))
print(paste('Q2 Answer:', round(ans25, 3)))
print(paste('Q3 Answer:', round(ans26, 1)))

