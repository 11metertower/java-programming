

library(data.table)
set1 = fread('lv2-2309.csv')

# Question 1
dt1 = copy(set1)
# Step 1-1~3
job_list = unique(dt1[,JOB_CODE])
city_list = unique(dt1[,CITY_CATEGORY])

library(caret)

max_val = -1
max_job = ''
for (job in job_list){
  tmp1 = dt1[JOB_CODE == job]
  for (city in city_list){
    tmp2 = tmp1[CITY_CATEGORY == city]
    varr = abs(cor(tmp2$STAY_YEARS, tmp2$PURCHASE))
    if(varr > max_val){
      max_val = varr
      max_job = job
    }
  }
} 

A = max_val
B = dt1[JOB_CODE == max_job, mean(PURCHASE)]

ans21 = A * B
print(paste('Q1 Result:', ans21))
print(paste('Q1 Answer:', round(ans21, 3)))


# Question 2
dt2 = copy(set1)

# Step 2-1
dt2_1 = dt2[,.N, c('GENDER','JOB_CODE','CITY_CATEGORY', 'MARITAL_STATUS')]
setorder(dt2_1, cols = -'N')
dt2_2 = dt2_1[c(1:20)]

outlier_list = c()
# Step 2-2~3
for (i in 1:20){
  tmp = dt2[GENDER == dt2_2[i, GENDER] & JOB_CODE == dt2_2[i,JOB_CODE] 
            & CITY_CATEGORY == dt2_2[i, CITY_CATEGORY] & MARITAL_STATUS == dt2_2[i, MARITAL_STATUS]]
  
  q1 = quantile(tmp[, PURCHASE], .25)
  q3 = quantile(tmp[, PURCHASE], .75)
  iqr = q3 - q1
  
  total_len = nrow(tmp)
  
  outlier = nrow(tmp[(PURCHASE < q1 - iqr) | (PURCHASE > q3 + iqr)])
  
  outlier_list = append(outlier_list, outlier/total_len)
}

ans22 = max(outlier_list)
print(paste('Q2 Result:', ans22))
print(paste('Q2 Answer:', round(ans22, 3)))



# Question 3
dt3 = copy(set1)

# Step 3-1
dt3[, VIP:= ifelse(PURCHASE<=20000, 0,1)]

# Step 3-2
dat_dummies = dt3[, .(GENDER = as.factor(GENDER),
                      USER_AGE = as.factor(USER_AGE),
                      JOB_CODE = as.factor(JOB_CODE),
                      CITY_CATEGORY = as.factor(CITY_CATEGORY),
                      MARITAL_STATUS = as.factor(MARITAL_STATUS))]

enc = dummyVars(~., dat_dummies, sep="_", fullRank=TRUE)
enced = predict(enc, dat_dummies)
dt3_1 = cbind(dt3[,c('VIP', 'STAY_YEARS')], enced)

lr = glm(VIP~., dt3_1, family=binomial, maxit=4)

dt3_2 = data.table(STAY_YEARS=3, GENDER='M', USER_AGE='36-45', JOB_CODE='G', CITY_CATEGORY='B',
                   MARITAL_STATUS='Yes')

test = predict(enc, dt3_2)

test_1 = cbind(dt3_2[,.(STAY_YEARS)], predict(enc, dt3_2))
ans23 = predict(lr, test_1, type='response')


print(paste('Q3 Result:', ans23))
print(paste('Q3 Answer:', round(ans23, 4)))
