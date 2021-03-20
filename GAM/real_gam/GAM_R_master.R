library(mgcv)

getPerformance = function(pred, val) {
  error = pred - val
  MAE = mean(abs(error), na.rm = TRUE)
  MAE_std = sd(abs(error))
  MAPE = mean(100*abs(error)/val)
  MAPE_std = sd(100*abs(error)/val)
  MSE = sum(error^2)/length(val)
  MSE_std = sd((error)**2)
  RMSE = sqrt(MSE)
  perf = data.frame(MAE,MAE_std,MAPE, MAPE_std, MSE, MSE_std, RMSE)
}

setwd("/home/vacek/Downloads/GAM/real_gam")

X_y_train= read.csv('X_y_train.csv')
X_y_test = read.csv('X_y_test.csv')

trainData = as.data.frame(X_y_train[2:14])
testData = as.data.frame(X_y_test[2:14])

y_train = as.data.frame(X_y_train[14])
y_test = as.data.frame(X_y_test[14])

k=10
# Entrenamos el modelo con los datos de TRAIN
gam_train = gam(y ~ s(x_0, bs='ps', k=k) + s(x_1, bs='ps', k=k) + s(x_2, bs='ps', k=k) +
                    s(x_3, bs='ps', k=k) + s(x_4, bs='ps', k=k) + s(x_5, bs='ps', k=k) +
                    s(x_6, bs='ps', k=k) + s(x_7, bs='ps', k=k) + s(x_8, bs='ps', k=k) + 
                    s(x_9, bs='ps', k=k) + te(x_10, x_11, bs='ps', k=k),
                  family = Gamma(link = "identity"), data=trainData, gamma = 2)

# Resumen del modelo
summary(gam_train)

# Lambdas
gam_train$sp

# Análisis de residuos
gam.check(gam_train)

#Predicciones sobre Train
y_pred = predict(gam_train, trainData, type="response")

err_train <- getPerformance(y_pred,trainData$y)
err_train


#Predicciones sobre Test
y_pred_test = predict(gam_train, testData, type="response")

err_test <- getPerformance(y_pred_test,testData$y)
err_test


