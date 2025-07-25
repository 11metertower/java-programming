
library(data.table)
library(dplyr)

CUSTOMER = fread("lv2-2404.csv")
dim(CUSTOMER)
str(CUSTOMER)
summary(CUSTOMER)

##### Question 1 ##############################################################
df210 = copy(CUSTOMER)

# 1-1, 1-2
df_rg1 <- df210 %>% filter(REGION == 1) %>% select(TENURE); dim(df_rg1)
df_rg2 <- df210 %>% filter(REGION == 2) %>% select(TENURE); dim(df_rg2)
df_rg3 <- df210 %>% filter(REGION == 3) %>% select(TENURE); dim(df_rg3)

num_mean <- mean(df_rg1$TENURE)
num_std <- sd(df_rg1$TENURE)
z_value <- 1.968178
a_low <- num_mean - z_value * num_std / sqrt(nrow(df_rg1))
a_up <- num_mean + z_value * num_std / sqrt(nrow(df_rg1))

num_mean <- mean(df_rg2$TENURE)
num_std <- sd(df_rg2$TENURE)
z_value <- 1.968038
b_low <- num_mean - z_value * num_std / sqrt(nrow(df_rg2))
b_up <- num_mean + z_value * num_std / sqrt(nrow(df_rg2))

num_mean <- mean(df_rg3$TENURE)
num_std <- sd(df_rg3$TENURE)
z_value <- 1.967596
c_low <- num_mean - z_value * num_std / sqrt(nrow(df_rg3))
c_up <- num_mean + z_value * num_std / sqrt(nrow(df_rg3))

A = max(c(a_up, b_up, c_up)) + min(c(a_low, b_low, c_low))
print(A, digits=16)

ans21 = round(A, 2)
print(ans21)


##### Question 2 ##############################################################
df220 = copy(CUSTOMER)

# 2-1
library(caret)
preProcModel <- preProcess(df220[, c("TENURE", "AGE", "INCOME", "EMPLOY")], method = c("center", "scale"))
transformedData <- predict(preProcModel, df220[, c("TENURE", "AGE", "INCOME", "EMPLOY")])
colnames(transformedData) <- c("TENURE_S", "AGE_S", "INCOME_S", "EMPLOY_S")
df220 <- df220[, -c("TENURE", "AGE", "INCOME", "EMPLOY")]
df221 <- cbind(df220, transformedData)

# 2-2
df_for_clustering <- df221 %>% select(TENURE_S, AGE_S, INCOME_S, EMPLOY_S)
set.seed(1234)
km00 <- kmeans(df_for_clustering, centers = 4)
df221$clus <- km00$cluster
df221 <- mutate(df221, A_YN = ifelse(df221$CUSTCAT == "Class A", 1, 0))

# 2-3
srs_cnt01 <- df221 %>% filter(clus == 1) %>% select(TENURE_S, AGE_S, INCOME_S, EMPLOY_S) %>% colMeans()
srs_cnt02 <- df221 %>% filter(clus == 2) %>% select(TENURE_S, AGE_S, INCOME_S, EMPLOY_S) %>% colMeans()
srs_cnt03 <- df221 %>% filter(clus == 3) %>% select(TENURE_S, AGE_S, INCOME_S, EMPLOY_S) %>% colMeans()
srs_cnt04 <- df221 %>% filter(clus == 4) %>% select(TENURE_S, AGE_S, INCOME_S, EMPLOY_S) %>% colMeans()

# 2-4
arr_std <- rep(0, 4)

for (i in 1:4) {
  df_tmp <- df221 %>% filter(clus == i) %>% select(TENURE_S, AGE_S, INCOME_S, EMPLOY_S)
  srs_cnt01 <- colMeans(df_tmp)
  srs_ucd <- (df_tmp$TENURE_S - srs_cnt01["TENURE_S"])^2 +
    (df_tmp$AGE_S - srs_cnt01["AGE_S"])^2 +
    (df_tmp$INCOME_S - srs_cnt01["INCOME_S"])^2 +
    (df_tmp$EMPLOY_S - srs_cnt01["EMPLOY_S"])^2
  arr_std[i] <- sd(sqrt(srs_ucd))
}

# 2-5
print(min(arr_std), digit=16)

ans22 = round(min(arr_std), 3)
print(ans22)


##### Question 3 ##############################################################
df230 = copy(CUSTOMER)

# 3-1
df231 <- df230 %>% filter(CUSTCAT %in% c("Class A", "Class D"))

# 3-2
df231 <- df231 %>%
  mutate(AGE_GROUP = case_when(
    AGE < 30 ~ "AGE_20",
    AGE < 40 ~ "AGE_30",
    AGE < 50 ~ "AGE_40",
    TRUE ~ "AGE_50"
  ))

# 3-3
library(fastDummies)
df233 <- df231
df234 <- dummy_cols(df233, select_columns = c('REGION','ED','AGE_GROUP'), remove_first_dummy = TRUE)

# 3-4
columns_to_convert <- c('CUSTCAT') # List the column names you want to convert
for (col in columns_to_convert) {
  df234[[col]] <- as.factor(df234[[col]])
}

train23 <- df234 %>% filter(CUS_ID %% 5 != 0); dim(train23)
test23 <- df234 %>% filter(CUS_ID %% 5 == 0); dim(test23)

# 3-5
library(rpart)
model_formula <- CUSTCAT ~ TENURE+INCOME+MARITAL+EMPLOY+RETIRE+GENDER+REGION_2+REGION_3+ED_2+ED_3+ED_4+ED_5+AGE_GROUP_AGE_30+AGE_GROUP_AGE_40+AGE_GROUP_AGE_50
set.seed(1234)
rf = rpart(model_formula, train23, control=rpart.control(cp=0, maxdepth=3))

# 3-6
arr_pred <- predict(rf, newdata = test23, 'class')
arr_act <- test23$CUSTCAT
precision <- sum((arr_pred == "Class A") & (arr_act == "Class A")) / sum(arr_pred == "Class A")
print(precision, digit=16)

ans23 = round(precision, 3)
ans23


####### Summary ################################################################
ans21; ans22; ans23
