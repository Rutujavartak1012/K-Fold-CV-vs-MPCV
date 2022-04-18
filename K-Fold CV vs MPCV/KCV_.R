# K-Fold Cross Validation

df = read.csv("C:\\Users\\Rutuja Vartak\\Downloads\\Advertising.csv")
df
data = df

set.seed(1234)

# Splitting the dataset randomly into 5 folds into nearly equal sizes.

data_new = split(data, sample(rep(1:5, 40)))

Fold_1 = data_new$`1`

Fold_2 = data_new$`2`

Fold_3 = data_new$`3`

Fold_4 = data_new$`4`

Fold_5 = data_new$`5`


#Splitting the data into training and testing

train_1 = rbind(Fold_1,Fold_2,Fold_3,Fold_4)

test_1 = Fold_5

# Building a MLR model on Fold_1

Model_1 = lm(Sales ~ TV + Radio + Newspaper, train_1)
summary(Model_1)

# Validating Model_1 on test_1

predict_1 = predict(Model_1,Fold_5)

# Mean Squared Error 

MSE_1 = mean((Fold_5$Sales-predict_1)^2)
MSE_1

################################################################

#Splitting the data into training and testing

train_2 = rbind(Fold_1,Fold_2,Fold_3,Fold_5)

test_2 = Fold_4

# Building a MLR model on Fold_2

Model_2 = lm(Sales ~ TV + Radio + Newspaper, train_2)
summary(Model_2)

# Validating Model_2 on test_2

predict_2 = predict(Model_2,Fold_4)

# Mean Squared Error 

MSE_2 = mean((Fold_4$Sales-predict_2)^2)
MSE_2
###########################################################################

#Splitting the data into training and testing

train_3 = rbind(Fold_1,Fold_2,Fold_4,Fold_5)

test_3 = Fold_3

# Building a MLR model on Fold_3

Model_3 = lm(Sales ~ TV + Radio + Newspaper, train_3)
summary(Model_3)

# Validating Model_3 on test_3

predict_3 = predict(Model_3,Fold_3)

# Mean Squared Error 

MSE_3 = mean((Fold_3$Sales-predict_3)^2)
MSE_3
#############################################################################

#Splitting the data into training and testing

train_4 = rbind(Fold_1,Fold_3,Fold_4,Fold_5)

test_4 = Fold_2

# Building a MLR model on Fold_2

Model_4 = lm(Sales ~ TV + Radio + Newspaper, train_4)
summary(Model_3)

# Validating Model_4 on test_4

predict_4 = predict(Model_4,Fold_2)

# Mean Squared Error 

MSE_4 = mean((Fold_2$Sales-predict_4)^2)
MSE_4
########################################################################

#Splitting the data into training and testing

train_5 = rbind(Fold_2,Fold_3,Fold_4,Fold_5)

test_5 = Fold_1

# Building a MLR model on Fold_1

Model_5 = lm(Sales ~ TV + Radio + Newspaper, train_5)
summary(Model_2)

# Validating Model_5 on test_5

predict_5 = predict(Model_5,Fold_1)

# Mean Squared Error 

MSE_5 = mean((Fold_1$Sales-predict_5)^2)
MSE_5

cbind(MSE_1,MSE_2,MSE_3,MSE_4,MSE_5)

mean(cbind(MSE_1,MSE_2,MSE_3,MSE_4,MSE_5))





