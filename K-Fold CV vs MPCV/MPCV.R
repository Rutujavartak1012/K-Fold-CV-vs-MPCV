# MPCV (Multiple Predicting Cross-Validation). 

df = read.csv("C:\\Users\\talas\\Downloads\\Advertising.csv")
df

data = df

set.seed(1234)

# Since there are 200 observations (i.e n = 200) in the dataset and we need to make k = 5 folds.
# n_k = n/k = 200/5 = 40

k = 5
n = nrow(data)

# Number of samples in each fold 

n_k = n/k

# Splitting the dataset randomly into 5 folds into nearly equal sizes.

data_new = split(data, sample(rep(1:5, 40)))

Fold_1 = data_new$`1`

Fold_2 = data_new$`2`

Fold_3 = data_new$`3`

Fold_4 = data_new$`4`

Fold_5 = data_new$`5`





# Building a MLR model on Fold_1

Model_1 = lm(Sales ~ TV + Radio + Newspaper, Fold_1)
summary(Model_1)


# Validating the Model_1 on (K-1) Folds.

          
predict_2 = predict(Model_1,Fold_2)
predict_3 = predict(Model_1,Fold_3)
predict_4 = predict(Model_1,Fold_4)
predict_5 = predict(Model_1,Fold_5)



# Mean Squared Errors 

MSE_2 = mean((Fold_2$Sales-predict_2)^2)
MSE_3 = mean((Fold_3$Sales-predict_3)^2)
MSE_4 = mean((Fold_4$Sales-predict_4)^2)
MSE_5 = mean((Fold_5$Sales-predict_5)^2)

MSE1 = cbind(MSE_2,MSE_3,MSE_4,MSE_5)

A1 = mean(MSE1)


##################################



# Building a MLR model on Fold_2

Model_2 = lm(Sales ~ TV + Radio + Newspaper, Fold_2)
summary(Model_2)


# Validating the Model_2 on (K-1) Folds.

predict__1 = predict(Model_2,Fold_1)
predict__3 = predict(Model_2,Fold_3)
predict__4 = predict(Model_2,Fold_4)
predict__5 = predict(Model_2,Fold_5)



# Mean Squared Errors 

MSE__1 = mean((Fold_1$Sales-predict__1)^2)
MSE__3 = mean((Fold_3$Sales-predict__3)^2)
MSE__4 = mean((Fold_4$Sales-predict__4)^2)
MSE__5 = mean((Fold_5$Sales-predict__5)^2)

MSE2 = cbind(MSE__1,MSE__3,MSE__4,MSE__5)

A2 = mean(MSE2)


######################################



# Building a MLR model on Fold_3

Model_3 = lm(Sales ~ TV + Radio + Newspaper, Fold_3)
summary(Model_3)


# Validating the Model_2 on (K-1) Folds.

predict___1 = predict(Model_3,Fold_1)
predict___2 = predict(Model_3,Fold_2)
predict___4 = predict(Model_3,Fold_4)
predict___5 = predict(Model_3,Fold_5)



# Mean Squared Errors 

MSE___1 = mean((Fold_1$Sales-predict___1)^2)
MSE___2 = mean((Fold_2$Sales-predict___2)^2)
MSE___4 = mean((Fold_4$Sales-predict___4)^2)
MSE___5 = mean((Fold_5$Sales-predict___5)^2)

MSE3 = cbind(MSE___1,MSE___2,MSE___4,MSE___5)

A3 = mean(MSE3)


######################################



# Building a MLR model on Fold_4

Model_4 = lm(Sales ~ TV + Radio + Newspaper, Fold_4)
summary(Model_4)


# Validating the Model_2 on (K-1) Folds.

predict____1 = predict(Model_4,Fold_1)
predict____2 = predict(Model_4,Fold_2)
predict____3 = predict(Model_4,Fold_3)
predict____5 = predict(Model_4,Fold_5)



# Mean Squared Errors 

MSE____1 = mean((Fold_1$Sales-predict____1)^2)
MSE____2 = mean((Fold_2$Sales-predict____2)^2)
MSE____3 = mean((Fold_3$Sales-predict____3)^2)
MSE____5 = mean((Fold_5$Sales-predict____5)^2)

MSE4 = cbind(MSE____1,MSE____2,MSE____3,MSE____5)

A4 = mean(MSE4)


###############################################



# Building a MLR model on Fold_5

Model_5 = lm(Sales ~ TV + Radio + Newspaper, Fold_5)
summary(Model_5)


# Validating the Model_2 on (K-1) Folds.

predict_____1 = predict(Model_5,Fold_1)
predict_____2 = predict(Model_5,Fold_2)
predict_____3 = predict(Model_5,Fold_3)
predict_____4 = predict(Model_5,Fold_4)



# Mean Squared Errors 

MSE_____1 = mean((Fold_1$Sales-predict_____1)^2)
MSE_____2 = mean((Fold_2$Sales-predict_____2)^2)
MSE_____3 = mean((Fold_3$Sales-predict_____3)^2)
MSE_____4 = mean((Fold_4$Sales-predict_____4)^2)

MSE5 = cbind(MSE_____1,MSE_____2,MSE_____3,MSE_____4)

A5 = mean(MSE5)


cbind(A1,A2,A3,A4,A5)

min(cbind(A1,A2,A3,A4,A5))


