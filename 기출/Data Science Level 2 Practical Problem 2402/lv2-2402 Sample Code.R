library(dplyr)
library(data.table)


dt2 = fread('lv2-2402.csv', encoding = "UTF-8")
dim(dt2)

head(dt2)

#####################################################
## Q1
#####################################################

df4 = copy(dt2)

# 1-1.
df4[, LIKE_SCORE := 
      5*(LIKES=='yes'&LIKE_TYPE=='heart emoji')+
      4*(LIKES=='yes'&LIKE_TYPE=='thumbs up')+
      3*(LIKES=='yes'&LIKE_TYPE=='clap')+
      2*(LIKES=='yes'&LIKE_TYPE=='laughing')+
      1*(LIKES=='yes'&LIKE_TYPE=='')+
      (-5)*(LIKES=='no'&LIKE_TYPE=='fire')+
      (-1)*(LIKES=='no'&LIKE_TYPE=='')]

# 1-2.
AGG24 = df4[, .(NUM_LIKES=sum(LIKES=='yes'), 
                NUM_USER=length(USER_ID), 
                SUM_SCORE=sum(LIKE_SCORE)), POST_ID]

# 1-3.
AGG24[, POPULAR := NUM_LIKES + SUM_SCORE/NUM_USER]
ans24 = max(AGG24$POPULAR)

ans24

#####################################################
## Q2
#####################################################

df5 = copy(dt2)

# 2-1.
b1 = df5 %>% group_by(USER_ID) %>% summarise(NUM_POST=n())
b1 = b1 %>% arrange(USER_ID)

b2 = df5 %>% group_by(USER_ID, LIKES) %>% summarise(NUM_LIKE = n())
b2 = data.frame(b2)
b2 = b2[b2$LIKES=='yes',]
b2 = b2[,c('USER_ID','NUM_LIKE')]
b2=b2 %>% arrange(USER_ID)

agg25 = cbind(b1, b2[, c('NUM_LIKE')])
names(agg25) = c('USER_ID','NUM_POST', 'NUM_LIKE')

# 2-2.
library('caret')
agg25s = predict(preProcess(agg25[,c('NUM_POST','NUM_LIKE')], method=c('scale','center')), agg25)


library(cluster)
set.seed(1234)
agg25s$cluster = kmeans(agg25s[,c('NUM_POST','NUM_LIKE')], centers = 2)[1]$cluster

# 2-4.
group_a = agg25s[agg25s$cluster==1, c('USER_ID')]
group_b = agg25s[agg25s$cluster==2, c('USER_ID')]

# 2-5.
df5_5 = df5[df5$USER_ID %in% group_a,]
avg_a = mean(df5_5$NUM_HASHTAG)

# 2-6.
df5_6 = df5[df5$USER_ID %in% group_b,]
avg_b = mean(df5_6$NUM_HASHTAG)

ans25 = abs(avg_a-avg_b)
ans25

#####################################################
## Q3
#####################################################

df6_1 = copy(dt2)
dim(df6_1)

# 3-1.
c5 = data.frame(df6_1 %>% group_by(POST_ID, CREATED_DATE) %>% summarise(NUM_USER = n(),
                                                        AVG_HASHTAG = mean(NUM_HASHTAG)))
c6 = data.frame(df6_1 %>% group_by(POST_ID, CREATED_DATE, LIKES) %>% summarise(NUM_LIKES = n()))
c6 = c6[c6$LIKES=='yes',]
c6 = c6[,c('POST_ID','CREATED_DATE', 'NUM_LIKES')]
c5 = c5 %>% arrange(POST_ID)
c6 = c6 %>% arrange(POST_ID)
agg26 = cbind(c5, c6[,c('NUM_LIKES')])
names(agg26) = c('POST_ID','CREATED_DATE','NUM_USER', 'AVG_HASHTAG','NUM_LIKES')
dim(agg26)

# 3-2.
agg26$POPULAR = ifelse(agg26$NUM_USER>=31, 1, 0)

# 3-3.
agg26$WEEKDAY = strftime(strptime(agg26$CREATED_DATE, format='%Y-%m-%d'), '%w')
agg26$WEEKDAY = as.numeric(agg26$WEEKDAY)

# 3-4.
dataset6 = agg26
dataset6 = dataset6[, c('POPULAR', 'NUM_LIKES', 'AVG_HASHTAG', 'WEEKDAY')]

xtrain6 = as.matrix(dataset6[,-(1)])
ytrain6 = as.character(dataset6[,c('POPULAR')])

library('naivebayes')
naive = naivebayes::gaussian_naive_bayes(xtrain6, ytrain6)

# 3-5.
test6 = as.matrix(data.frame(NUM_LIKES=c(16), AVG_HASHTAG=c(2.42),WEEKDAY=c(5)))
test6
ans26 = predict(naive, test6, type='prob')[2]
ans26


########################################
## Answers Summary
#####################################
paste0('Q1: ', ans24)
paste0('Q2: ', ans25)
paste0('Q3: ', ans26)

round(ans24, 2)
round(ans25, 4)
round(ans26, 3)
