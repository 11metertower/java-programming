
library(data.table)
POWER = fread('lv2-2408.csv')
str(POWER)

### Question 1 ################################################################
# 1-1
df1 = copy(POWER)
df1[, OUT1 := 1*(F1>quantile(F1,0.5)+2*(quantile(F1,0.75)-quantile(F1,0.25))), .(SEASON, FAC)]
df1[, OUT2 := 1*(F2>quantile(F2,0.5)+2*(quantile(F2,0.75)-quantile(F2,0.25))), .(SEASON, FAC)]
df1[, OUT3 := 1*(F3>quantile(F3,0.5)+2*(quantile(F3,0.75)-quantile(F3,0.25))), .(SEASON, FAC)]
df1[, OUT4 := 1*(F4>quantile(F4,0.5)+2*(quantile(F4,0.75)-quantile(F4,0.25))), .(SEASON, FAC)]
df1[, OUT5 := 1*(F5>quantile(F5,0.5)+2*(quantile(F5,0.75)-quantile(F5,0.25))), .(SEASON, FAC)]
df1[, OUT6 := 1*(F6>quantile(F6,0.5)+2*(quantile(F6,0.75)-quantile(F6,0.25))), .(SEASON, FAC)]
df1[, OUT := 1*((OUT1+OUT2+OUT3+OUT4+OUT5+OUT6)>0)]

# 1-2
df1_out = df1[, .(MEAN_OUT=mean(OUT)), .(SEASON, FAC)]
df1_out = df1_out[order(-MEAN_OUT)]
df1_out

# Answer 1
ans21f = max(df1_out$MEAN_OUT)
ans21 = round(ans21f, 4)

print(ans21f, digits=16)
print(ans21)

### Question 2 ################################################################
# 2-1
df2 = copy(POWER[FAC=='Northern'])
dim(df2)

# 2-2
df2[, TOTAL := F1+F2+F3+F4+F5+F6]

# 2-3
cri = data.table(z=c(1.64, 1.96, 2.33, 2.58), cump=c(0.95, 0.975, 0.99, 0.995))
cri[, conf_level := 1-(1-cump)*2]; cri
cri_use = cri[round(conf_level, 2)==0.90, z]; cri_use

confs = df2[, .(LB = mean(TOTAL) - cri_use*220/sqrt(.N), 
                UB = mean(TOTAL) + cri_use*220/sqrt(.N)), SEASON]
confs[, len := UB-LB]

# 2-4
confs
max(confs$len)

# Answer 2
ans22f = max(confs$len)
ans22 = round(ans22f, 1)

print(ans22f, digits=16)
print(ans22)


### Question 3 ################################################################
# 3-1
df3 = copy(POWER)
df3 = df3[(F1!=0)&(F2!=0)&(F3!=0)&(F4!=0)&(F5!=0)&(F6!=0)]
df3 = df3[order(DATE, FAC)]
dim(df3)

# 3-2
df3[, F6_NEXT := shift(F6, -1), FAC]
df3 = df3[!is.na(F6_NEXT)]
df3[FAC=='Northern']

# 3-3
TRAIN3 = df3[DATE<'2022-09-01']
TEST3 = df3[DATE>='2022-09-01']
dim(TRAIN3); dim(TEST3)

# 3-4
mdl3 = lm(F6_NEXT ~ F1+F2+F3+F4+F5+F6, TRAIN3)
summary(mdl3)

# 3-5
TEST3$PRED = predict(mdl3, TEST3)
MAPE = 100*mean(TEST3[, abs((PRED-F6_NEXT)/F6_NEXT)]); MAPE

# Answer 3
ans23f = MAPE
ans23 = round(ans23f, 3)

print(ans23f, digits=16)
print(ans23)


##### Summary ##################################################################
ans21; ans22; ans23
