#Machine Learning Algorithms
#Oswaldo Arturo Díaz Arca
#TFM - Universidad Internacional de la Rioja

rm(list=ls())
setwd("D:/ruta_personal/TFM_UNIR-main")
library(dplyr)
library(caret)
library(party)
library(gbm)
library(glmnet)
library(xgboost)
library(e1071)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(car)
# Trainning and testing datasets
ml_dataset = read.csv("Data/ml_dataset.csv")
inTrain <- createDataPartition(y=ml_dataset$Rdto_kg.ha, p=0.60, list=F)
trainning <- ml_dataset[inTrain,]
testing <- ml_dataset[-inTrain,]

# -------MULTIPLE LINEAR REGRESSION (MLR)---------
t <- proc.time()
mlr_model <- lm(Rdto_kg.ha~., data=ml_dataset)
proc.time()-t
summary(mlr_model) # El modelo es significativo
# Selection of best predictors (Akaike(AIC))
step(object=mlr_model, direction="both", trace=1)
# New model
formula <- Rdto_kg.ha ~ Tmin_Nov + Tmin_Feb + Tmax_Oct + Tmax_Dec + 
  Tmax_Jan + Tmax_Feb + Tmax_Mar + Prec_Oct + Prec_Nov + Prec_Dec + 
  Prec_Jan + Prec_Feb + Prec_Mar + GDD_Nov + GDD_Dec + GDD_Jan + 
  GDD_Feb + GDD_Mar + TR_Feb + Rs_Oct + Rs_Nov + Rs_Dec + Rs_Jan + 
  Rs_Feb + Rs_Mar
mlr_model <- lm(formula=formula, data=ml_dataset)
summary(mlr_model)
# Confidence intervals
confint(lm(formula=formula, data=ml_dataset))
# Validation of lineal relation between predictors and "Rdto"
plot1 <- ggplot(data = ml_dataset, aes(Tmin_Nov, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = ml_dataset, aes(Tmin_Feb, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = ml_dataset, aes(Tmax_Oct, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = ml_dataset, aes(Tmax_Dec, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = ml_dataset, aes(Tmax_Jan, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot6 <- ggplot(data = ml_dataset, aes(Tmax_Feb, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot7 <- ggplot(data = ml_dataset, aes(Tmax_Mar, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot8 <- ggplot(data = ml_dataset, aes(Prec_Oct, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot9 <- ggplot(data = ml_dataset, aes(Prec_Nov, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot10 <- ggplot(data = ml_dataset, aes(Prec_Dec, mlr_model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9)
# Validation of residuals normal distribution
qqnorm(mlr_model$residuals)
qqline(mlr_model$residuals)
shapiro.test(mlr_model$residuals[1:5000]) #*No hay normalidad de residuos
ggplot(data=ml_dataset, aes(mlr_model$fitted.values, mlr_model$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()
bptest(mlr_model) #*No hay Homocedasticidad
# Matrix of correlation between predictors
corrplot(cor(ml_dataset[,-37]),
         method = "number", tl.col = "black")
# Analisys of Variance Inflation Factor (VIF):
vif(mlr_model)
# Autocorrelation:
dwt(mlr_model, alternative = "two.sided")
summary(mlr_model)
# Metrics
mlr_pred <- predict(mlr_model, newdata=ml_dataset[,-37])
postResample(mlr_pred, ml_dataset$Rdto_kg.ha)

#-------LASSO REGRESSION--------------------
# Hyperparameters
lambdas <- 10^seq(2, -15, by = -.1)
lasso_reg <- cv.glmnet(as.matrix(trainning[,-37]), trainning[,37], alpha = 0.5, lambda = lambdas, standardize = TRUE, nfolds = 10)
lambda_best <- lasso_reg$lambda.min 
# Model
t <- proc.time()
lasso_model <- glmnet(as.matrix(trainning[,-37]), trainning[,37], alpha = 1, lambda = lambda_best, standardize = TRUE)
proc.time()-t
postResample(predict(lasso_model, newx=as.matrix(trainning[,-37])), 
             trainning$Rdto_kg.ha)
# Model performance
lasso_pred  <- predict(lasso_model, newx = as.matrix(testing[,-37]))
postResample(lasso_pred, testing$Rdto_kg.ha)

# ------ELASTIC NET REGRESSION----------
# Hyperparameters
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,repeats = 5,
                           search = "random",verboseIter = TRUE)
# Model
t <- proc.time()
elast_model <- train(Rdto_kg.ha ~ .,data = trainning,
                     method = "glmnet",preProcess = c("center", "scale"),
                     tuneLength=10,trControl = train_cont)
proc.time()-t
elast_model$bestTune
postResample(predict(elast_model, newdata=trainning), trainning$Rdto_kg.ha)
# Model performance
elast_pred  <- predict(elast_model, newdata=testing)
round(postResample(elast_pred, testing$Rdto_kg.ha),3)

# ------MULTIVARIATE ADAPTIVE REGRESSION SPLINES (MARS)-------
# Hyperparameters
marsGrid <- expand.grid(degree=c(4), nprune=c(75))
ctrl <- trainControl(method="repeatedcv", number=10)
# Model
t <- proc.time()
set.seed(111)
mars_model <- caret::train(Rdto_kg.ha~ ., data=trainning, method="earth", tuneGrid=marsGrid, trControl=ctrl)
proc.time()-t
# Model performance
mars_pred  <- predict(mars_model, newdata = testing)
postResample(mars_pred, testing$Rdto_kg.ha)

# -------K Nearest Neighbors (KNN)------------------
# Hyperparameters
ctrl = trainControl(method="repeatedcv", number=10)
knnGrid = expand.grid(k=c(4))
# Model
t <- proc.time()
set.seed(222)
knn_model = train(Rdto_kg.ha~ ., data=trainning,method="knn",preProcess=c("center","scale"), tuneGrid=knnGrid,trControl=ctrl)
proc.time()-t
# Model performance
knn_pred = predict(knn_model,newdata=testing)
postResample(knn_pred,testing$Rdto_kg.ha)

# ------GRADIENT BOOSTING MACHINE (GBM)-------------------------
# Hyperparameters
gbmGrid = expand.grid(n.trees = c(1000),interaction.depth = c(16),
                      shrinkage = c(0.2),n.minobsinnode = c(9))
ctrl = trainControl(method="repeatedcv", number=10)
# Model
t <- proc.time()
set.seed(333)
gbm_model = train(Rdto_kg.ha~., data=trainning, method="gbm", tuneGrid=gbmGrid, trControl=ctrl)
proc.time()-t
# Model performance
gbm_pred <- predict(gbm_model, testing)
postResample(gbm_pred, testing$Rdto_kg.ha)

# ----------------XGBOOST-----------------
# Hyperparameters
params = list(set.seed=444, eval_metric="rmse",
              objetive="reg:squarederror")
# Model
t <- proc.time()
xgboost_model = xgboost(data=as.matrix(trainning[,-37]),
                        label= trainning[,37],params=params,
                        nrounds=3000,verbose=1,eta=.18,
                        early_stopping_rounds = 3,max_depth=6)
proc.time()-t
postResample(predict(xgboost_model, newdata = as.matrix(trainning[,-37])), 
             trainning$Rdto_kg.ha)
# Model performance
xgboost_pred  <- predict(xgboost_model, newdata = as.matrix(testing[,-37]))
postResample(xgboost_pred, testing$Rdto_kg.ha)

#--------RANDOM FOREST---------
# Hyperparameters
rf_grid <- data.frame(mtry=c(35))
ctrl = trainControl(method="repeatedcv", number=10)
# Model
t <- proc.time()
set.seed(555)
rf_model <- caret::train(Rdto_kg.ha~., data=trainning, method="rf", tuneGrid=rf_grid, trControl=ctrl,ncores=6, ntree=300)
proc.time()-t
# Model performance
rf_pred <- predict(rf_model, testing)
postResample(rf_pred, testing$Rdto_kg.ha)

# ------CONDITIONAL INFERENCE TREE (CIT)----------------------
# Model
t <- proc.time()
set.seed(666)
cit_model = ctree(Rdto_kg.ha~.,data=trainning)
proc.time()-t
postResample(predict(cit_model), trainning$Rdto_kg.ha)
# Model performance
cit_pred  <- predict(cit_model, newdata = testing)
postResample(cit_pred, testing$Rdto_kg.ha)

# ------CONDITIONAL INFERENCE FOREST (CIF)--------------------
# Model
t <- proc.time()
set.seed(777)
cif_model <- cforest(Rdto_kg.ha ~ ., data = trainning, control = cforest_unbiased(mtry = 25, ntree = 500))
proc.time()-t
postResample(predict(cif_model), trainning$Rdto_kg.ha)
# Model performance
cif_pred  <- predict(cif_model, newdata = testing)
postResample(cif_pred, testing$Rdto_kg.ha)

# -------SVR --------------
# Model
t <- proc.time()
set.seed(888)
svr_model <- svm(formula=Rdto_kg.ha~., 
                 data=trainning, type="eps-regression",
                 epsilon = 0.1, 
                 cost = 450)
proc.time()-t
postResample(predict(svr_model), trainning$Rdto_kg.ha)
# Model performance
svr_pred <- predict(svr_model, testing)
postResample(svr_pred, testing$Rdto_kg.ha)

# -------------DEEP NEURAL NETWORK (DNN)---------------
#install.packages(c("keras","tensorflow"))
#install.packages("devtools")
#devtools::install_github("rstudio/keras",dependencies=TRUE)
#devtools::install_github("rstudio/tensorflow",dependencies = TRUE)
#install_keras()
#install_tensorflow()
library(keras)
library(tensorflow)
library(devtools)
library(magrittr)
library(neuralnet)

# Model Structure Example
n <- neuralnet(Rdto_kg.ha~.,data=ml_dataset,hidden=c(15,5),
               linear.output=F,lifesign='full',rep=1)
plot(n, col.hidden='darkgreen', col.hidden.synapse='darkgreen', 
     show.weights=F, information=F, fill='lightblue')
# Matrix
data <- as.matrix(ml_dataset)
dimnames(data) <- NULL
# Partition
set.seed(999)
ind <- sample(2,nrow(data), replace=T, prob=c(.7, .3))
training <- data[ind==1, c(1:36,38:43)]
test <- data[ind==2, c(1:36,38:43)]
training_target <- data[ind==1, 37]
test_target <- data[ind==2, 37]
# Normalize
m <- colMeans(training)
s <- apply(training,2,sd)
training <- scale(training, center=m, scale=s)
test <- scale(test, center=m, scale=s)
# Model
model <- keras_model_sequential()
model %>% 
  layer_dense(unit=400, activation='gelu', input_shape=c(42)) %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=350, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=300, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=250, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=200, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=130, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=50, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(unit=20, activation='gelu') %>%
  layer_dropout(rate=0.1) %>%
  layer_batch_normalization() %>%
  layer_dense(units=1)
summary(model)
# Compilation
set.seed(998)
model %>% compile(loss='mse',
                  optimizer='rmsprop',
                  metrics='mae')
# Early Stopping
early_stopping = callback_early_stopping(min_delta = 0.001, 
  patience = 20, restore_best_weights = TRUE)
# Trainning model
t <- proc.time()
set.seed(996)
dnn_model <- model %>%
  fit(training, training_target, epochs=200, batch_size=32,
      callbacks=list(early_stopping), validation_split=0.2)
proc.time()-t
# Model performance
model %>% evaluate(test, test_target)
pred <- model %>% predict(test)
rmse = sqrt(mean((test_target-pred)^2))
r2 = cor(test_target,pred)^2

#---------------------MODELS PLOTS----------------
# MLR
plot(mlr_pred, ml_dataset$Rdto_kg.ha,col="darkblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Multiple Linear Regression")
lines(4000:16000,4000:16000,col="darkorange")
# LASSO
plot(lasso_pred,testing$Rdto_kg.ha, col="darkblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="LASSO Regression")
lines(4000:16000,4000:16000,col="darkorange")
# ENR
plot(elast_pred,testing$Rdto_kg.ha, col="darkblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Elastic Net Regression")
lines(4000:16000,4000:16000,col="darkorange")
# MARS
plot(mars_pred, testing$Rdto_kg.ha,col="chartreuse3",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Multivariate Adaptive Regression Splines (MARS)")
lines(4000:16000,4000:16000,col="darkorange")
# KNN
plot(knn_pred,testing$Rdto_kg.ha, col="chartreuse3",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="K Nearest Neighbors")
lines(4000:16000,4000:16000,col="darkorange")
# GBM
plot(gbm_pred, testing$Rdto_kg.ha,col="blueviolet",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Gradient Boosting Machine")
lines(4000:16000,4000:16000,col="darkorange")
# XGBoost
plot(xgboost_pred, testing$Rdto_kg.ha,col="blueviolet",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Extreme Gradient Boosting")
lines(4000:16000,4000:16000,col="darkorange")
# RF
plot(rf_pred, testing$Rdto_kg.ha,col="cornflowerblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Random Forest")
lines(4000:16000,4000:16000,col="darkorange")
# CIF
plot(cif_pred, testing$Rdto_kg.ha,col="cornflowerblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Conditional Inference Forest")
lines(4000:16000,4000:16000,col="darkorange")
# SVR
plot(svr_pred, testing$Rdto_kg.ha,col="cornflowerblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Support Vector Regression")
lines(4000:16000,4000:16000,col="darkorange")
# DNN
plot(test_target,pred,col="cornflowerblue",
     xlab="Rdto predicho",ylab="Rdto observado",
     main="Deep Neural Network")
lines(4000:16000,4000:16000,col="darkorange")

#-------------VARIABLE IMPORTANCE-------------
library("vip")
library("Ckmeans.1d.dp")
 
mlr = vi(mlr_model)
lasso = vi(lasso_model)
elast = vi(elast_model)
mars = vi(mars_model)
knn = vi(knn_model)
gbm = vi(gbm_model)
xgboost = vi(xgboost_model)
rf = vi(rf_model)
cif = vi(cif_model)

# Plots
p9 <- ggplot(data=cif, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("CIF") + xlab("")
p8 <- ggplot(data=rf, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("RF") + xlab("")
p7 <- ggplot(data=xgboost, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("XGBoost") + xlab("")
p6 <- ggplot(data=gbm, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("GBM") + xlab("")
p5 <- ggplot(data=knn, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("KNN") + xlab("")
p4 <- ggplot(data=mars, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("MARS") + xlab("")
p3 <- ggplot(data=elast, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("Elastic Net") + xlab("")
p2 <- ggplot(data=lasso, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("LASSO") + xlab("")
p1 <- ggplot(data=mlr, aes(x=reorder(Variable, Importance),y=Importance)) +
  geom_bar(position="dodge",stat="identity",fill="blueviolet") + 
  coord_flip() + ggtitle("MLR") + xlab("")

grid.arrange(p1,p2,p3, ncol = 3)
grid.arrange(p9,p4,p5,p7,p6,p8, ncol = 6)

# Bibliography:
# MARS: http://uc-r.github.io/mars
# CIF: https://cran.rstudio.com/web/packages/varImp/varImp.pdf
# CIF: https://www.rdocumentation.org/packages/partykit/versions/1.2-15/topics/cforest
# CIF: https://community.rstudio.com/t/understanding-conditional-inference-forests-variable-importance/57428/5
# CIF: https://www.youtube.com/watch?v=3Jy3f0J21QI
# CIT: https://cran.r-project.org/web/packages/partykit/vignettes/ctree.pdf
# KNN: https://www.youtube.com/watch?v=zwc7doFXpFs&list=PLzMw_44KVqI99Nuu_JNVt79Cx3jFwy0lh
# GBM: https://www.youtube.com/watch?v=EMUkl2IDq_Y
# GBM: https://www.rdocumentation.org/packages/gbm/versions/2.1.8/topics/gbm
# DNN: https://www.tensorflow.org/api_docs/python/tf/keras/activations
# DNN: https://keras.rstudio.com/reference/callback_early_stopping.html
# DNN: https://www.youtube.com/watch?v=cIUg11mAmK4
# DNN: https://www.kaggle.com/learn/intro-to-deep-learning
# LASSO/ELAST: https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r
# SVR: https://rpubs.com/richkt/280840
# MLR: https://rpubs.com/Joaquin_AR/226291#:~:text=Versi%C3%B3n%20PDF%3A%20Github-,Introducci%C3%B3n,2%2C%20X3%E2%80%A6).