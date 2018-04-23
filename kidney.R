#Logistic Regression 

#kidney <- read.csv("C:\\Users\\Lenova\\Desktop\\chronic_kidney_disease.csv")
kidney <- read.csv("C:\\Users\\Lenova\\Desktop\\ch_kidney_disease.csv", na.strings = "?")

kidney$age <- as.numeric(kidney$age)
kidney$bp <- as.numeric(kidney$bp)
kidney$bgr <- as.numeric(kidney$bgr)
kidney$bu <- as.numeric(kidney$bu)
kidney$sc <- as.numeric(kidney$sc)
kidney$sod <- as.numeric(kidney$sod)
kidney$pot <- as.numeric(kidney$pot)
kidney$hemo <- as.numeric(kidney$hemo)
kidney$pcv <- as.numeric(kidney$pcv)
kidney$wbcc <- as.numeric(kidney$wbcc)
kidney$rbcc <- as.numeric(kidney$rbcc)


head(kidney)

set.seed(144)

require(caTools)

split <- sample.split(kidney$class, SplitRatio=1/5)  
training<-subset(kidney, split == T)
testing<-subset(kidney, split == F)

# training$bgr <- (training$bgr)*1.0
 
# logit'ten sonra , control = list(maxit = 100) Gerek kalmadı..
 
model <- glm(class~ age + bp + bgr + bu + sc + sod + pot + hemo + pcv + wbcc + rbcc, data = training, family = binomial("logit"))

#chi-sq confmatlara uygulanmalı..

summary(model)

plot(model)

prediction <- predict(model, newdata = testing, type = "response")

confMat <- table(testing$class, prediction > 0.5)

confMat  

anova(model)

# anovadaki önemli değişkenlere göre:

model2 <- glm(class~ age + bp + bgr + bu, data = training, family = binomial("logit"))

summary(model2)

plot(model2)

prediction2 <- predict(model2, newdata = testing, type = "response")

confMat2 <- table(testing$class, prediction > 0.5)

confMat2

# model2'deki yıldızlı değişkenler alınarak:

model3 <- glm(class~ bp + bu, data = training, family = binomial("logit"))

summary(model3)

cf <- table(testing$class, prediction > 0.5)

cf

#confMat, confMat2 ve cf aynı sonucu verdi?

# SVM

library(e1071)

model_svm <- svm(class~ age + bp + bgr + bu + sc + sod + pot + hemo + pcv + wbcc + rbcc, data = training) 

summary(model_svm)

pred <- predict(model_svm, testing)

tablo <- table(Hesaplanan = pred, Gercek = testing$class)

# tablo same length hatası veriyor

tablo  

# Karar Ağacı

library(rpart)

model_dt <- rpart(class~ bp + bu, data = training)

summary(model_dt)

plot(model_dt)

# plot(model_dt, type="simple") karar ağacını çizdirmedi..

pred2 <- predict(model_dt, testing)

tablo2 <- table(Hesaplanan = pred2, Gercek = testing$class)

# tablo2 de same length hatası veriyor..

tablo2

# anova yı modele uygulamak lazım. önemli olan sütunlara göre karar ağacında onları kullan.
# karar ağacı için daha az değişken gerekli.