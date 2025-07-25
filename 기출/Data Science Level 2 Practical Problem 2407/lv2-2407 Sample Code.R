
library(data.table)
library(dplyr)

CULTURE = fread('lv2-2407.csv')
dim(CULTURE)
str(CULTURE)
summary(CULTURE)
head(CULTURE)

##### Question 1 ##############################################################
df4 = copy(CULTURE)

# 1-1
var4 = c(names(df4)[2:4], names(df4)[6:11]); var4
cv = df4[,sapply(.SD, function(x) sd(x)/mean(x)), .SDcols=var4]; cv

# 1-2
library(moments)
sk = df4[,sapply(.SD, function(x) skewness(x)), .SDcols=var4]; sk

# 1-3
corr = cor(df4[, .(SCORE, NEMP, NEMP_PER_HEAD, INT_RATIO, S1, S2, S3, S4, S5)])
corr = corr['SCORE', colnames(corr)!='SCORE']
corr

# Answer 1
res24 = max(cv) + min(sk) + max(corr)
print(res24, digits=16)

ans24 = round(res24, 3)
print(ans24)

##### Question 2 ##############################################################
df5 = copy(CULTURE)

# 2-1
df5 = df5[F_RATIO != -999]
dim(df5)

# 2-2
library(caret)
var5 = names(df5)[2:10]; var5
var5s = paste0(var5,'_S')
scaler = preProcess(df5[,..var5], method = c('scale','center'))
scaled = predict(scaler, df5[,..var5])
colnames(scaled) = var5s
df5s = cbind(df5, scaled)

# 2-3
set.seed(1234); df5s$km2 = kmeans(df5s[,..var5s], 2)$cluster
set.seed(1234); df5s$km3 = kmeans(df5s[,..var5s], 3)$cluster
set.seed(1234); df5s$km4 = kmeans(df5s[,..var5s], 4)$cluster

# 2-4
library(cluster)
sil2 = mean(silhouette(df5s$km2, dist(df5s[,..var5s]))[,"sil_width"])
sil3 = mean(silhouette(df5s$km3, dist(df5s[,..var5s]))[,"sil_width"])
sil4 = mean(silhouette(df5s$km4, dist(df5s[,..var5s]))[,"sil_width"])
sils = c(sil2, sil3, sil4)
names(sils) = 2:4
evaltext = paste0('df5s$fin_clst = df5s$km',names(which.max(sils)))
eval(parse(text=evaltext))

# Answer 2
ans25 = max(count(df5s, fin_clst))
print(ans25)

##### Question 3 ##############################################################
df6 = copy(CULTURE)

# 3-1
fratio_re = mean(df6[F_RATIO != -999]$F_RATIO)
df6[, F_RATIO := ifelse(F_RATIO == -999, fratio_re, F_RATIO)]

# 3-2
train6 = df6[C_ID%%5!=0,-c('C_ID')]
test6 = df6[C_ID%%5==0,-c('C_ID')]
dim(train6); dim(test6)

# 3-3
var6 = names(df6)[2:10]; var6
mdl6 = lm(paste0('SCORE ~', paste(var6, collapse='+')), train6)

# 3-4
A6 = max(abs(mdl6$coefficients)); A6

# 3-5
pred6 = predict(mdl6, test6)
B6 = mean(abs((pred6-test6$SCORE)/test6$SCORE))*100; B6

# Answer 3
res26 = A6 + B6
print(res26, digits=16)

ans26 = round(res26, 4)
ans26


####### Summary ################################################################
ans24; ans25; ans26