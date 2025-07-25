
library(dplyr)
library(dplyr)
library(data.table)



dt2 = fread('lv2-2310.csv', encoding = "UTF-8")
dim(dt2)
head(dt2)

#####################
## Q1
####################

df4= copy(dt2)
names(df4)
# 1-1.
df4$outlier=0
for (i in 3:4){
  feature = as.numeric(unlist(df4[,..i]))
  q1 = quantile(feature, 0.25)
  q3 = quantile(feature, 0.75)
  iqr = q3-q1
  df4$outlier = ifelse((df4[,..i]<q1-1.5*iqr)|(df4[,..i]>q3+1.5*iqr),1,df4$outlier) 
}
df41 = df4[df4$outlier==0]

# 1-2.
df41$YEAR = as.numeric(substr(df41$DATE,0,4))

# 1-3.
avg24 = df41 %>% group_by(YEAR, TYPE) %>% summarise(volume_avg = mean(VOLUME),
                                            price_avg =mean(PRICE))

avg24 = as.data.frame(avg24)
# 1-4.
ratio1=(avg24[((avg24$YEAR==2018)&(avg24$TYPE=='class1')),'volume_avg']-avg24[((avg24$YEAR==2017)&(avg24$TYPE=='class1')),'volume_avg'])/avg24[((avg24$YEAR==2017)&(avg24$TYPE=='class1')),'volume_avg']
ratio2=(avg24[((avg24$YEAR==2019)&(avg24$TYPE=='class2')),'volume_avg']-avg24[((avg24$YEAR==2018)&(avg24$TYPE=='class2')),'volume_avg'])/avg24[((avg24$YEAR==2018)&(avg24$TYPE=='class2')),'volume_avg']        

# 1-5.
ratio3=(avg24[((avg24$YEAR==2018)&(avg24$TYPE=='class1')),'price_avg']-avg24[((avg24$YEAR==2017)&(avg24$TYPE=='class1')),'price_avg'])/avg24[((avg24$YEAR==2017)&(avg24$TYPE=='class1')),'price_avg']
ratio4=(avg24[((avg24$YEAR==2019)&(avg24$TYPE=='class2')),'price_avg']-avg24[((avg24$YEAR==2018)&(avg24$TYPE=='class2')),'price_avg'])/avg24[((avg24$YEAR==2018)&(avg24$TYPE=='class2')),'price_avg'] 

ans24 = ratio1+ratio2+ratio3+ratio4
ans24
##############################
## Q2
###############################

df5 = copy(dt2)

# 2-1.
library('caret')
var5 = c('PRICE','VOLUME')
df5s = predict(preProcess(df5[,..var5], method=c('range')), df5)

# 2-2.
df51=fastDummies::dummy_cols(df5s, select_columns = c('REGION'), remove_first_dummy =T)

# 2-3.
train5 = df51[df51$DATE<='2019-01-31',]
dim(train5)
test5 = df51[df51$DATE>='2019-02-01',]
dim(test5)

# 2-4.
library(class)
var5 = c('PRICE','VOLUME','REGION_B','REGION_C','REGION_D')
k_vec = c(5,7,9)
k5=0
acc5=0
for (i in k_vec){
  set.seed(1234)
  ml5 = class::knn(train5[, ..var5], test5[, ..var5], cl=train5$TYPE, k=i)
  acc_ml5 = sum(test5$TYPE==ml5)/nrow(test5)
  print(acc_ml5)
  if(acc_ml5>acc5){
    acc5 = acc_ml5
    k5=i
  }
}
k5
acc5

ans25 = k5 + acc5
ans25



#############
## Q3
#############

df6 = copy(dt2)

# 3-1.
df6$year = as.numeric(substr(df6$DATE,0,4))

df6$YEAR_NUM = ifelse(df6$year==2017, 1, ifelse(df6$year ==2018,2, ifelse(df6$year==2019,3,4)))
df6 = df6[,-c('year')]
df6$MONTH = as.numeric(substr(df6$DATE,6,7))

# 3-2.
df61=fastDummies::dummy_cols(df6, select_columns = c('REGION','TYPE'), remove_first_dummy =T)
df61 = df61[,-c('REGION','TYPE')]

# 3-3.
df61 = df61[,-c('SEQ','DATE')]
lm6_3 = lm(PRICE~.+1, df61)
pred6 = predict(lm6_3, df61)

# 3-4.
rmse1 = sqrt(mean((pred6-df61$PRICE)**2))

# 3-5.
cor(df61[,c('PRICE','VOLUME','YEAR_NUM','MONTH')])

var6 = c('VOLUME')

# 3-6.
lm6_6 = lm(PRICE~VOLUME+1, df61)
pred61 = predict(lm6_6, df61)

# 3-7.
rmse2 = sqrt(mean((pred61-df61$PRICE)**2))
rmse2

# 3-8,
val6 = lm6_6$coefficients[var6]
ans26 = as.numeric(abs(rmse1-rmse2)+abs(val6))
ans26

########################################
## Answers Summary
#####################################
cat('Q1: ', ans24,'\n')
cat('Q2: ', ans25,'\n')
cat('Q3: ', ans26,'\n')

print(round(ans24, 3))
print(round(ans25, 2))
print(round(ans26, 2))