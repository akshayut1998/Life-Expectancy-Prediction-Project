rm(list = ls())
library(dplyr)
library(Hmisc)
library(skimr)
library(devtools)
library(visdat)
library(tidyverse)
library(corrplot)
library(mltools)
library(caret)
library(tidyr)
library(glmnet)
library(WDI)
library(kknn)
library(randomForest)
library(rpart)
library(gbm)

file_path <- "life_expectancy.csv"
life <- read.csv(file_path,header = TRUE)
head(life)
dim(life) 
summary(life)
skim(life)

# Initializing the results tibble

results_tibble <- tibble(
  `Algorithm_Name` = character(),
  `Train_RMSE` = numeric(),
  `Validation_RMSE` = numeric(),
  `Test_RMSE` = numeric()
)

# Function to append values to tibble and return it
append_values_to_tibble <- function(tibble_data, algorithm_name, train_rmse, validation_rmse, test_rmse) {
  new_row <- tibble(
    `Algorithm_Name` = algorithm_name,
    `Train_RMSE` = train_rmse,
    `Validation_RMSE` = validation_rmse,
    `Test_RMSE` = test_rmse
  )
  
  updated_tibble <- bind_rows(tibble_data, new_row)
  return(updated_tibble)
}

# Get unique countries in the data frame
countries <- unique(life$Country)

# Iterate over each column except 'Country'
for (col_name in colnames(life)[-1]) {
  # Iterate over each country
  for (country in countries) {
    # Subset the data for the current country
    country_data <- life %>% filter(Country == country)
    
    # Calculate the median value for the current column within the country, ignoring NAs
    median_value <- median(country_data[[col_name]], na.rm = TRUE)
    
    # Replace NAs with the median value for the current column within the country
    life <- life %>% 
      mutate(!!col_name := ifelse(Country == country & is.na(!!sym(col_name)), median_value, !!sym(col_name)))
  }
}

summary(life)

# Main issues visible so far- GDP, and population columns are very unreliable and have a lot of missing values
# Solution - Import GDP and Population data from an external data source

GDP_DATA=WDI(country="all",indicator=c("NY.GDP.MKTP.CD","SP.POP.TOTL"),start=2000,end=2015)
GDP_DATA %>%  glimpse
GDP_DATA %>%  summary

data.frame(unique(life$Country)[!(unique(life$Country) %in% GDP_DATA$country)])
data.frame(unique(GDP_DATA$country))

replace_life=c("Bahamas","Bolivia (Plurinational State of)","Côte d'Ivoire","Congo","Democratic People's Republic of Korea","Democratic Republic of the Congo","Egypt","Gambia","Iran (Islamic Republic of)","Kyrgyzstan","Lao People's Democratic Republic","Micronesia (Federated States of)","Republic of Korea","Republic of Moldova","Saint Lucia","Saint Vincent and the Grenadines","Slovakia","Turkey","United Republic of Tanzania","United States of America","Venezuela (Bolivarian Republic of)","Viet Nam","Yemen")
replace_GDP=c("Bahamas, The","Bolivia","Cote d'Ivoire","Congo, Rep.","Korea, Dem. People's Rep.","Congo, Dem. Rep.","Egypt, Arab Rep.","Gambia, The","Iran, Islamic Rep.","Kyrgyz Republic","Lao PDR","Micronesia, Fed. Sts.","Korea, Rep.","Moldova","St. Lucia","St. Vincent and the Grenadines","Slovak Republic","Turkiye","Tanzania","United States","Venezuela, RB","Vietnam","Yemen, Rep.")

for (i in 1:length(replace_life))
{
  GDP_DATA$country[GDP_DATA$country==replace_GDP[i]] <-  replace_life[i]
}

joined_data=left_join(life,GDP_DATA,by=c("Country"="country","Year"="year"))
joined_data = subset(joined_data, select = -c(Population,GDP) )
life <-joined_data %>% rename("GDP"="NY.GDP.MKTP.CD","Population"="SP.POP.TOTL")
life$gdp_capita=life$GDP/life$Population
life %>%  summary

# Measles 

ggplot(
  data = life,
  mapping = aes(x = Measles , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Polio 

ggplot(
  data = life,
  mapping = aes(x = Polio , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Diphtheria 

ggplot(
  data = life,
  mapping = aes(x = Diphtheria , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# HIV.AIDS 

ggplot(
  data = life,
  mapping = aes(x = HIV.AIDS , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# infant.deaths 

ggplot(
  data = life,
  mapping = aes(x = infant.deaths , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# under.five.deaths 

ggplot(
  data = life,
  mapping = aes(x = under.five.deaths , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Total.expenditure 

ggplot(
  data = life,
  mapping = aes(x = Total.expenditure , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# BMI 

ggplot(
  data = life,
  mapping = aes(x = BMI , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# thinness..1.19.years 

ggplot(
  data = life,
  mapping = aes(x = thinness..1.19.years , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Alcohol 

ggplot(
  data = life,
  mapping = aes(x = Alcohol , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Schooling 

ggplot(
  data = life,
  mapping = aes(x = Schooling , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Population 

ggplot(
  data = life,
  mapping = aes(x = Population , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# gdp_capita 

ggplot(
  data = life,
  mapping = aes(x = gdp_capita , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Status 

ggplot(
  data = life,
  mapping = aes(x = Status , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Hepatitis.B 

ggplot(
  data = life,
  mapping = aes(x = Hepatitis.B , y = Life.expectancy)
) +
  geom_point() +
  geom_smooth(method = "lm")

# We decided to remove Hepatitis.B as a feature as there were a large number of na values and other variables could possibly provide the same information (disease state of a nation)

summary(life)
x = subset(life, select = -c(Hepatitis.B))
life = na.omit(x)

# Linear Regression


#feature engineering for linear regression
life$trend=life$Year-1999
temp=as.factor(life$Status)
life$Status=as.factor(life$Status)
life=life %>% mutate(value = 1)  %>% spread(Status, value,  fill = 0 )

y_variables=c("Life.expectancy")
x_variables=c("Measles","Polio","Diphtheria","HIV.AIDS","infant.deaths","under.five.deaths","Total.expenditure","BMI","thinness..1.19.years","Alcohol","Schooling","Population","gdp_capita","trend","Developed","Developing")
all_variables=c(x_variables,y_variables)
before_filter=life
before_filter$Status=temp
#filtering for required columns
life <- life %>% select(all_of(all_variables))

#data-split

set.seed(1)
life$uid <- 1:nrow(life)
train_set <- life %>% sample_frac(0.7)
val_set  <- anti_join(life, train_set, by = 'uid')
test_set <- val_set %>% sample_frac(0.5)
val_set  <- anti_join(val_set, test_set, by = 'uid')
train_set = subset(train_set, select = -c(uid) )
test_set = subset(test_set, select = -c(uid) )
val_set = subset(val_set, select = -c(uid) )
train_val_set=rbind(train_set,val_set)

#feature selection and training for train

null = lm(Life.expectancy~1, data=train_set)
full = lm(Life.expectancy~., data=train_set)

regForward = step(null, scope=formula(full), direction="forward", k=log(nrow(life)))
regBack = step(full, direction="backward", k=log(nrow(life)))
regBoth = step(null, scope=formula(full), direction="both", k=log(nrow(life)))

aic_values <- c(AIC(regForward), AIC(regBack), AIC(regBoth))
lowest_aic_index <- which.min(aic_values)
lowest_aic_formula <- switch(lowest_aic_index,
                             formula(regForward$call[[2]]),
                             formula(regBack$call),
                             formula(regBoth$call[[2]]))

linear_reg_model=lm(lowest_aic_formula, train_set)

prediction=predict(linear_reg_model,subset(train_set, select = -c(Life.expectancy) ))
MSE_linear_regression_train=sqrt(mean((train_set$Life.expectancy-prediction)^2))

prediction=predict(linear_reg_model,subset(val_set, select = -c(Life.expectancy) ))
MSE_linear_regression_val=sqrt(mean((val_set$Life.expectancy-prediction)^2))

#stepwise feature selection and training for train+val

null = lm(Life.expectancy~1, data=train_val_set)
full = lm(Life.expectancy~., data=train_val_set)

regForward = step(null, scope=formula(full), direction="forward", k=log(nrow(life)))
regBack = step(full, direction="backward", k=log(nrow(life)))
regBoth = step(null, scope=formula(full), direction="both", k=log(nrow(life)))

aic_values <- c(AIC(regForward), AIC(regBack), AIC(regBoth))

lowest_aic_index <- which.min(aic_values)
lowest_aic_formula <- switch(lowest_aic_index,
                             formula(regForward$call[[2]]),
                             formula(regBack$call),
                             formula(regBoth$call[[2]]))

linear_reg_model=lm(lowest_aic_formula, train_val_set)

prediction=predict(linear_reg_model,subset(test_set, select = -c(Life.expectancy) ))
MSE_linear_regression_test=sqrt(mean((test_set$Life.expectancy-prediction)^2))

coef(linear_reg_model)

results_tibble=append_values_to_tibble(results_tibble,"Linear Regression",MSE_linear_regression_train,MSE_linear_regression_val,MSE_linear_regression_test)

# Lasso Regression

lambda_values=seq(0.0001, 1, by=0.0001)
indexing=1
rmse_values <- data.frame(validation = numeric(length(lambda_values)), train = numeric(length(lambda_values)))
pp <- preProcess(train_set[x_variables], method = "scale")
train_scale_set <- predict(pp, train_set[x_variables])
val_scale_set <- predict(pp, val_set[x_variables])
train_scale_set$Life.expectancy=train_set$Life.expectancy
val_scale_set$Life.expectancy=val_set$Life.expectancy

for (i in lambda_values) {
  
  lasso_reg_model <- glmnet(x = as.matrix(train_scale_set[, x_variables]),y=train_scale_set[, y_variables],alpha =1, lambda=i)
  prediction_val=predict(lasso_reg_model,as.matrix(val_scale_set[, x_variables]))
  rmse_values[indexing, "validation"]=sqrt(mean((val_scale_set[, y_variables]-prediction_val)^2))
  prediction_train=predict(lasso_reg_model,as.matrix(train_scale_set[, x_variables]))
  rmse_values[indexing, "train"]=sqrt(mean((train_scale_set[, y_variables]-prediction_train)^2))
  print(i)
  indexing=indexing+1
}

ideal_lambda <- lambda_values[which.min(rmse_values$validation)]
row_with_least_validation_rmse <- rmse_values[which.min(rmse_values$validation), ]
ideal_validation_rmse <- row_with_least_validation_rmse$validation
ideal_train_rmse <- row_with_least_validation_rmse$train

plot(log(lambda_values), rmse_values$validation, type = "l", col = "blue", xlab = "log(Lambda)", ylab = "MSE",
     main = "Val MSE vs. log(Lambda) for LASSO Regression")
points(log(ideal_lambda), ideal_validation_rmse, col = "red", pch = 16)
text(log(ideal_lambda), ideal_validation_rmse, paste0(" Ideal (", round(log(ideal_lambda), 3), ", ", round(ideal_validation_rmse, 3), ")"),
     pos = 3)


pp <- preProcess(train_val_set[x_variables], method = "scale")
train_val_scale_set <- predict(pp, train_val_set[x_variables])
test_scale_set <- predict(pp, test_set[x_variables])
train_val_scale_set$Life.expectancy=train_val_set$Life.expectancy
test_scale_set$Life.expectancy=test_set$Life.expectancy

lasso_reg_model <- glmnet(x = as.matrix(train_val_scale_set[, x_variables]),y=train_val_scale_set[, y_variables],alpha =1, lambda=ideal_lambda)
prediction=predict(lasso_reg_model,as.matrix(test_scale_set[, x_variables]))
ideal_test_rmse=sqrt(mean((test_scale_set[, y_variables]-prediction)^2))

coef(lasso_reg_model)

results_tibble=append_values_to_tibble(results_tibble,"Lasso Regression",ideal_train_rmse,ideal_validation_rmse,ideal_test_rmse)

# KNN Regressor

k_values=seq(1, nrow(train_scale_set)-1, by=1)
rmse_values <- data.frame(validation = numeric(length(k_values)))
for (i in k_values) {
  knn_regressor=kknn(Life.expectancy ~ .,train=train_scale_set,test=val_scale_set,k=i,kernel = "rectangular")
  rmse_values[i, "validation"]=sqrt(mean((val_scale_set[, y_variables]-knn_regressor$fitted)^2))
  print(i)
}

ideal_k <- k_values[which.min(rmse_values$validation)]
row_with_least_validation_rmse <- rmse_values[which.min(rmse_values$validation), ]
ideal_validation_rmse <- row_with_least_validation_rmse

knn_regressor=kknn(Life.expectancy ~ .,train=train_scale_set,test=train_scale_set,k=ideal_k,kernel = "rectangular")
ideal_train_rmse=sqrt(mean((train_set[, y_variables]-knn_regressor$fitted)^2))

knn_regressor=kknn(Life.expectancy ~ .,train=train_val_scale_set,test=test_scale_set,k=ideal_k,kernel = "rectangular")
ideal_test_rmse=sqrt(mean((test_set[, y_variables]-knn_regressor$fitted)^2))

plot(k_values, rmse_values[, "validation"], type = "l", col = "blue", xlab = "K", ylab = "MSE",
     main = "Val MSE vs. K for KNN Regression")
points(ideal_k, ideal_validation_rmse, col = "red", pch = 16)
text(ideal_k, ideal_validation_rmse, paste0(" Ideal (", round((ideal_k), 3), ", ", round(ideal_validation_rmse, 3), ")"),
     pos = 3)

results_tibble=append_values_to_tibble(results_tibble,"KNN Regressor",ideal_train_rmse,ideal_validation_rmse,ideal_test_rmse)

results_tibble

# random forest

life=before_filter
y_variables=c("Life.expectancy")
x_variables=c("Measles","Polio","Diphtheria","HIV.AIDS","infant.deaths","under.five.deaths","Total.expenditure","BMI","thinness..1.19.years","Alcohol","Schooling","Population","gdp_capita","Status")
all_variables=c(x_variables,y_variables)

#filtering for required columns
life <- life %>% select(all_of(all_variables))

life$uid <- 1:nrow(life)
train_set <- life %>% sample_frac(0.7)
val_set  <- anti_join(life, train_set, by = 'uid')
test_set <- val_set %>% sample_frac(0.5)
val_set  <- anti_join(val_set, test_set, by = 'uid')
train_set = subset(train_set, select = -c(uid) )
test_set = subset(test_set, select = -c(uid) )
val_set = subset(val_set, select = -c(uid) )
train_val_set=rbind(train_set,val_set)

p=ncol(train_set)-1
mtryv = c(p,sqrt(p),6,10)
ntreev = c(100,500,50,1000)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(Life.expectancy~.,data=train_set,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=val_set)
  olrf[i] = sqrt(mean((val_set$Life.expectancy-ofit)^2))
  ilrf[i] = sqrt(mean((train_set$Life.expectancy-ifit)^2))
}
#----------------------------------------
#print losses

rf_results=cbind(parmrf,olrf,ilrf)
print(rf_results)
lowest_olrf_row <- subset(rf_results, olrf == min(olrf))
lowest_olrf_row
#--------------------------------------------------
#fit on train+val
finrf = randomForest(Life.expectancy~.,data=train_val_set,mtry=lowest_olrf_row$mtry,ntree=lowest_olrf_row$ntree)
finrfpred=predict(finrf,newdata=test_set)
#--------------------------------------------------
#plot y vs yhat for test data

finrfrmse = sqrt(sum((test_set$Life.expectancy-finrfpred)^2)/nrow(test_set))
cat('finrfrmse: ',finrfrmse,'\n')
plot(test_set$Life.expectancy,finrfpred,xlab='test Life.expectancy',ylab='rf pred')
abline(0,1,col='red',lwd=2)

#--------------------------------------------------
#plot variable importance

varImpPlot(finrf)

results_tibble=append_values_to_tibble(results_tibble,"Random Forest",lowest_olrf_row$ilrf,lowest_olrf_row$olrf,finrfrmse)
results_tibble

# Decision trees

cp_values <- seq(0, 0.1, 0.05)
nset=length(cp_values)

train_rmse_values <- numeric(length(cp_values))
test_rmse_values <- numeric(length(cp_values))

train_rmse = rep(0,nset)
val_rmse = rep(0,nset)

cp_values <- seq(0, 0.1, 0.005)
minsplit = c(5,10,50,100)
parmrf = expand.grid(cp_values,minsplit)
colnames(parmrf)=c('cp_values','minsplit')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
parmrf$cp_values[2]
  
for(i in 1:nset) {
  model <- rpart(Life.expectancy ~ ., data = train_set,  control=rpart.control(minsplit=parmrf$minsplit[i],cp=parmrf$cp_values[i]))
  ifit = predict(model)
  ofit=predict(model,newdata=val_set)
  olrf[i] = sqrt(mean((val_set$Life.expectancy-ofit)^2))
  ilrf[i] = sqrt(mean((train_set$Life.expectancy-ifit)^2))
}

dt_results=cbind(parmrf,olrf,ilrf)
print(dt_results)
lowest_olrf_row <- subset(dt_results, olrf == min(olrf))
lowest_olrf_row

model <- rpart(Life.expectancy ~ ., data = train_val_set,  control=rpart.control(minsplit=lowest_olrf_row$minsplit,cp=lowest_olrf_row$cp_values))
test_rmse=predict(model,newdata=test_set)
test_rmse = sqrt(mean((test_set$Life.expectancy-test_rmse)^2))

results_tibble=append_values_to_tibble(results_tibble,"Decision Trees",lowest_olrf_row$ilrf,lowest_olrf_row$olrf,test_rmse)
results_tibble

# Boosting Regression

# Hyperparameter grid
param_grid <- expand.grid(tdepth = c(4, 10), ntree = c(1000, 5000, 10000), lam = c(0.001, 0.2, 0.3))

#Function to fit and evaluate gbm model
fit_and_evaluate <- function(tdepth, ntree, lam, train_data, val_data) {
  model <- gbm(Life.expectancy ~ ., data = train_data, distribution = 'gaussian',
               interaction.depth = tdepth, n.trees = ntree, shrinkage = lam)
  
  train_pred <- predict(model, n.trees = ntree, train_data)
  val_pred <- predict(model, newdata = val_data, n.trees = ntree)
  
  train_rmse <- sqrt(mean((train_data$Life.expectancy - train_pred)^2))
  val_rmse <- sqrt(mean((val_data$Life.expectancy - val_pred)^2))
  
  return(list(model = model, train_rmse = train_rmse, val_rmse = val_rmse))
}

#Loop over the hyperparameter grid and fit models
results <- lapply(1:nrow(param_grid), function(i) {
  cat('doing boost ', i, ' out of ', nrow(param_grid), '\n')
  fit_and_evaluate(param_grid$tdepth[i], param_grid$ntree[i], param_grid$lam[i], train_set, val_set)
})

#Combine the results into a data frame
results_df <- cbind(param_grid, do.call(rbind, lapply(results, function(x) c(x$train_rmse, x$val_rmse))))
colnames(results_df) <- c('tdepth', 'ntree', 'lam', 'train_rmse', 'val_rmse')

#Lowest validation RMSE
lowest_olrf_row <- results_df[which.min(results_df$val_rmse), ]

#Train the final model on train+validation set
final_model <- gbm(Life.expectancy ~ ., data = rbind(train_set, val_set),
                   distribution = 'gaussian', interaction.depth = lowest_olrf_row$tdepth,
                   n.trees = lowest_olrf_row$ntree, shrinkage = lowest_olrf_row$lam)

#Evaluate the model on the test set
test_pred <- predict(final_model, newdata = test_set, n.trees = lowest_olrf_row$ntree)
test_rmse <- sqrt(mean((test_set$Life.expectancy - test_pred)^2))

#Plotting variable importance
vsum <- summary(final_model)
vsum_df <- as.data.frame(vsum$rel.inf)
names(vsum_df) <- 'rel.inf'

vsum_df$var <- rownames(vsum_df)
row.names(vsum)=NULL
plot(vsum$rel.inf, axes = FALSE, pch = 16, col = 'red', main = "Variable Importance")
axis(1,labels=vsum$var,at = 1:nrow(vsum_df), las = 2, cex.axis = 0.5)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')

results_tibble <- append_values_to_tibble(results_tibble, "Boosting Regression",
                                          lowest_olrf_row$train_rmse, lowest_olrf_row$val_rmse, test_rmse)

#Best algorithm based on test RMSE value
best_algorithm <- subset(results_tibble, `Test_RMSE` == min(test_rmse))

#Print the results_tibble and best_algorithm
results_tibble
best_algorithm

