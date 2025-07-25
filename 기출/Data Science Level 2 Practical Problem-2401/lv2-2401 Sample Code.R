

library(dplyr)
library(data.table)

set2 = fread('lv2-2401.csv', encoding = "UTF-8")
dim(set2)
head(set2)

#####################################################
## Q1
#####################################################

df4 = copy(set2)

# Step 1-1.
df4$AGE_P=ifelse(df4$AGE<20, '10s', 
       ifelse((df4$AGE>=20)&(df4$AGE<30), '20s',
              ifelse((df4$AGE>=30)&(df4$AGE<40), '30s',
                     ifelse((df4$AGE>=40)&(df4$AGE<50), '40s',
                            ifelse(df4$AGE>=60, 'other', '50s'
  
)))))

# Step 1-2.
Agg24 = df4[, .(AVG_SPEND=mean(SPENDING), CUST_CNT=length(unique(C_ID))), AGE_P]
Agg24

# Step 1-3.
ans24 = Agg24[which.max(Agg24$AVG_SPEND)][,CUST_CNT]
ans24

#####################################################
## Q2
####################################################

df5 = copy(set2)

# Step 2-1
library('caret')
scale5<-preProcess(df5[,c('AGE','INCOME','WORK_EXP', 'FAMILY','SPENDING')], method=c('center','scale'))
df5s<-predict(scale5, df5)

# Step 2-2~3.
var2= c('AGE','INCOME','WORK_EXP', 'FAMILY','SPENDING')
library('cluster')
ks = c(3,4)
sil2 = vector()
for(i in ks){
  set.seed(1234)
  km2 = kmeans(df5s[,..var2], centers =i)
  sil = mean(silhouette(km2$cluster, dist(df5s[,..var2]))[,"sil_width"])
  print(paste0(i,' : ', sil))
  sil2 = append(sil, sil2)
}

max_sil = max(sil2)
print(max_sil, digits=16)

ans25 = round(max_sil, 3)
ans25

#####################################################
## Q3
####################################################

df6 = copy(set2)

# Step 3-1.
df6f=df6[df6$AGE>=14,]
df6f = df6f[df6f$JOB!='',]
dim(df6f)

# Step 3-2.
df6o<-fastDummies::dummy_cols(df6f,select_columns = c('GENDER','JOB'), remove_first_dummy = T)
names(df6o)
df6o=df6o[,-c('GENDER','JOB')]
names(df6o)

# Step 3-3.
train6 = df6o[df6o$C_ID%%3!=0,-c('C_ID', 'SPENDING')]
names(train6)
dim(train6)
test6 = df6o[df6o$C_ID%%3==0,-c('C_ID', 'SPENDING')]
dim(test6)

# Step 3-4.
library(rpart)
train6$GRADE = as.factor(train6$GRADE)
set.seed(1234)
dt3<-rpart::rpart(GRADE~., train6, control=rpart.control(cp=0, minsplit=2, maxdepth=5))
res6_1 = copy(test6)
res6_1$pred=predict(dt3, test6, type="class")

# Step 3-5.
a6 = mean(res6_1$GRADE==res6_1$pred)*100
a6
ans26 = round(a6, 2)
ans26

########################################
## Answers Summary
#####################################

ans24
ans25
ans26