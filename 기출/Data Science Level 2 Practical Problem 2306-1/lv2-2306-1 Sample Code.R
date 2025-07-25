
### Settings ###################################################################
library(data.table)
library(dplyr)

##### Set 1 Preprocess #########################################################
df_raw02 = fread("lv2-2306-1.csv")
dim(df_raw02)

##### Question 1 ##############################################################
df240 = copy(df_raw02)

# Assuming df240 is already a data.frame in R
list_num <- c('SCREEN_SIZE','REAR_CAMERA_MP','FRONT_CAMERA_MP','INTERNAL_MEMORY','BATTERY','WEIGHT')
list_rsq <- c()

for (col in list_num) {
  linear_model <- lm(NORMALIZED_USED_PRICE ~ ., data = df240 %>% select(col, 'NORMALIZED_USED_PRICE'))
  num_rsq <- summary(linear_model)$r.squared
  list_rsq <- append(list_rsq, num_rsq)
}

max_rsq <- max(list_rsq)

ans21 = round(max_rsq, 3)
ans21


##### Question 2 ##############################################################
df250 = copy(df_raw02)

# step2-1
df251 <- df250 %>% filter(RELEASE_YEAR %in% c(2019, 2020))

# 2-2
# Assuming df_raw02 is a data frame containing the data
srs_tmp <- df_raw02 %>%
  filter(RELEASE_YEAR %in% c(2020, 2019)) %>%
  count(DEVICE_BRAND) %>%
  filter(n >= 45)

list_brd <- srs_tmp$DEVICE_BRAND
print(list_brd)

df252 <- df251 %>%
  filter(DEVICE_BRAND %in% list_brd) %>%
  copy()

dim(df252)

# 2-3
list_tmp <- list()

for (brd in unique(df252$DEVICE_BRAND)) {
  df_tmp <- df252 %>%
    filter(DEVICE_BRAND == brd) %>%
    select(SCREEN_SIZE, REAR_CAMERA_MP, FRONT_CAMERA_MP, INTERNAL_MEMORY, BATTERY, WEIGHT, NORMALIZED_USED_PRICE)
  
  corr_matrix <- cor(df_tmp)
  max_corr <- max(abs(corr_matrix[1:6, "NORMALIZED_USED_PRICE"]))
  list_tmp[[length(list_tmp) + 1]] <- max_corr
}

mean(unlist(list_tmp))

ans22 = round(mean(unlist(list_tmp)), 3)
ans22


##### Question 3 ##############################################################
df260 = copy(df_raw02)

# step3-1
df260$PRD_AGE <- 2023 - df260$RELEASE_YEAR

# step3-2
library(fastDummies, quietly = T)

# Assuming your dataframe is named 'df260' in R
df_dum <- dummy_cols(df260[, c("DEVICE_BRAND", "OS", "NET_4G", "NET_5G")], remove_first_dummy = TRUE)
df_dum <- df_dum %>% select(-DEVICE_BRAND, -OS, -NET_4G, -NET_5G)
df261 = cbind(df260, df_dum)
df261 <- df261 %>% select(-DEVICE_BRAND, -OS, -NET_4G, -NET_5G)

# step3-3
train26 <- df261 %>% filter(PRD_ID %% 5 != 0)
test26 <- df261 %>% filter(PRD_ID %% 5 == 0)


# step 3-4
# Define the features
list_fts <- c('SCREEN_SIZE', 'REAR_CAMERA_MP', 'FRONT_CAMERA_MP', 'INTERNAL_MEMORY',
              'RAM', 'BATTERY', 'WEIGHT', 'DAYS_USED', 'NORMALIZED_NEW_PRICE', 
              'PRD_AGE', colnames(df_dum))
length(list_fts)


# Create a formula to fit the model
formula <- as.formula(paste("NORMALIZED_USED_PRICE ~", paste(list_fts, collapse="+")))

library(randomForest, quietly = T)
# Fit the Random Forest model
set.seed(2305)
RF00 <- randomForest(formula, data = train26,
                     ntree = 9,
                    nodesize = 10)

arr_pred <- as.numeric(predict(RF00, test26))
arr_act <- test26$NORMALIZED_USED_PRICE

mse <- mean((arr_pred - arr_act)^2)
rmse <- sqrt(mse)

rmse

ans23 = round(rmse, 3)
ans23


################################################################################
ans21; ans22; ans23

