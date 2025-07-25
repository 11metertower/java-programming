library(data.table)

dt = fread('lv2-2307.csv')


## Question 1
dt4 = copy(dt)

# step 1-1
dt4$DAY = strftime(strptime(dt4$SIG_DATE, '%Y-%m-%d') , '%w')

dt4[, WEEKEND:= ifelse((DAY == 0) | (DAY == 6), 'Yes','No' )]

# step 1-2
q1 = quantile(dt4$SIGNAL, .25)
q3 = quantile(dt4$SIGNAL, .75)
iqr = q3 - q1

dt4[, OUTLIER_S := ifelse((SIGNAL >= q1 - 1.5*iqr) & (SIGNAL <= q3 + 1.5 * iqr), 0, 1 )]

A = nrow(dt4[(WEEKEND == 'Yes') & (OUTLIER_S == 1)]) / nrow(dt4[WEEKEND == 'Yes'])
B = nrow(dt4[(WEEKEND == 'No') & (OUTLIER_S == 1)]) / nrow(dt4[WEEKEND == 'No'])

ans1 = A / B
print(paste('Q1 Result:', ans1))
print(paste('Q1 Answer:', round(ans1, 3)))



# Question 2
dt5 = copy(dt)

# Step 2-1
n = nrow(dt5)
S = sd(dt5$SETUP)
t = 1.96
x = mean(dt5$SETUP)

A = -t*S/(n**0.5) + x
B = t*S/(n**0.5) + x


# Step 2-2
dt5_1 = dt5[SETUP != 0]
tec_mean = dt5_1[,mean(SETUP),TECHNOLOGY]

dt5_2 = merge(dt5, tec_mean, by.x = "TECHNOLOGY", by.y = "TECHNOLOGY")

setorder(dt5_2, cols = 'SIG_ID')

dt5_2[, SETUP_F := ifelse(SETUP == 0, V1, SETUP)]

# Step 2-3
n = nrow(dt5_2)
S = sd(dt5_2$SETUP_F)
t = 1.96
x = mean(dt5_2$SETUP_F)

C = -t*S/(n**0.5) + x
D = t*S/(n**0.5) + x

ans2 = abs(D-A)

print(paste('Q2 Result:', ans2))
print(paste('Q2 Answer:', round(ans2, 3)))


## Q3
dt6 = copy(dt)

# Step 3-1
t_list = unique(dt6$TECHNOLOGY)

for(t in t_list){
  tmp = dt6[TECHNOLOGY == t]
  tmp_1 = tmp[SPEED > 0]
  b = round(coef(lm(SPEED~ SIGNAL + 0, tmp_1)),3)
  tmp[,SPEED_F := ifelse(SPEED <= 0, b * SIGNAL, SPEED)]
  if (t == 'LTE'){
    dt6_1 = tmp
  }else{
    dt6_1 = rbind(dt6_1, tmp)
  }
  
}
setorder(dt6_1, cols = 'SIG_ID')

dt6_1[, SPEED := SPEED_F]
dt6_1 = dt6_1[, -c('SPEED_F')]

# Step 3-2
dt6_1[, TARGET := ifelse(MOS >= 4, 1, 0)]


#Step 3-4
train = dt6_1[SIG_ID %% 4 != 0]
test = dt6_1[SIG_ID %% 4 == 0]

train = train[, -c('SIG_ID', 'SIG_DATE', 'RESULT', 'TECHNOLOGY','SETUP','MOS')]
test = test[, -c('SIG_ID', 'SIG_DATE', 'RESULT', 'TECHNOLOGY','SETUP','MOS')]
#Step 3-5
set.seed(1234)
lr = glm(TARGET~., train, family=binomial)

predict(lr, test, type='response')

test$pred = (predict(lr, test, type='response') >= 0.5)*1
#Step 3-6
ans3 = mean(test[,pred == TARGET])
print(paste('Q3 Result:', ans3))
print(paste('Q3 Answer:', round(ans3, 3)))
