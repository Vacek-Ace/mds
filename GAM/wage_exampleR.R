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


setwd("/home/vacek/Downloads/GAM")

X = read.csv('X_wage.csv', header = FALSE, sep = ",")
y = read.csv('y_wage.csv',  header = FALSE, sep = ",")

names(X) = c('year', 'age', 'education')
names(y) = 'wage'

X_y=c(X, y)

k=10
# Poisson Model with P-splines
gam = gam(wage ~ s(year, bs='ps', k=k) + s(age, bs='ps', k=k) + s(education, bs='fs', k=5),
                family = poisson(link = "identity"), data=X_y, gamma = 2)


# Resumen del modelo
summary(gam)
# Lambdas
gam$sp

# Partial Dependance plots
plot(gam)


k2=2
# Poisson Model with thin plate splines
gam2 = gam(wage ~ s(year, bs='tp', k=k2) + s(age, bs='tp', k=k2) + s(education, bs='fs', k=5),
          family = poisson(link = "identity"), data=X_y, gamma = 2)

# Resumen del modelo
summary(gam2)
# Lambdas
gam2$sp

# Partial Dependance plots
plot(gam2)

# Residuals
gam.check(gam2)




