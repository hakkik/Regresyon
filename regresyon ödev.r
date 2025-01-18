library(car)
library(fBasics)
library(lmtest)
library(MASS)
library(nortest)
library(olsrr)
library(stats)
library(zoo)
library(fastDummies)
library(DAAG)

# pulling our data
data=read.table("c:/data1.txt", header = T)
names(data)
names(data) = c("verim","isik","sicaklik","su","mineraller")
attach(data)
toprak_turu=as.factor(mineraller)
basicStats(data)

# we're looking to see if it's normal, it's not.
qqnorm(verim)
qqline(verim)
shapiro.test(verim)

# Kolmogorov-Svirnov tests
ks.test(verim, "pnorm", mean = mean(verim), sd = sd(verim))

# We check again by doing log transformation, it does not work again
lnverim=log(verim)
qqnorm(lnverim)
qqline(lnverim)
shapiro.test(lnverim)

# We are looking for square root transformation, again no 
verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)

# From boxplot we now find the values
boxplot(verim)
which(verim %in% boxplot(verim) $out)

# now we extract our values and create our new data
data_aykirilar_silindi=data[-c(17 ,33, 46, 64 ,69 ,72), ]
write.table(data_aykirilar_silindi,"data2.txt",row.names = FALSE)

# Cleaning up our environment
rm(list = ls())

# pulling our new data
data2=read.table("c:/data2.txt",header = T)
names(data2) = c("verim","isik","sicaklik","su","mineraller")
attach(data2)
mineraller=as.factor(mineraller)
basicStats(data2)

# we look at the normality distribution of our new data and it still does not provide
qqnorm(verim)
qqline(verim)
shapiro.test(verim)

# log transformation
lnverim=log(verim)
qqnorm(lnverim)
qqline(lnverim)
shapiro.test(lnverim)

# we are doing square root transformation to our new data
verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)

# Linearity
data_birlestirme=cbind(verim,isik,sicaklik,su,mineraller)
pairs(data_birlestirme)

# Model
model=lm(sqrt_verim~isik+sicaklik+su+mineraller)
model
summary(model)

# finding a confidence interval
confint(model ,level = 0.99)

# Residual Review
inf=ls.diag(model)
inf
influence.measures(model)
n=74
k=5

# code to find k
toplam_katsayi <- length(coef(model))
k <- toplam_katsayi - 1
print(k)

## Residual Review ##

# line distance
h1 = 2*(k+1)/n
h1
which(inf$hat> h1)
hat=inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/n , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="red")

# standart articles
which(inf$std.res>2)
which(inf$std.res<(-2))
std=inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text( x = 1:length(std), y = std,labels = ifelse(std < -2 | std > 2, 1:length(std), ""),col = "red", pos = 3 )

# student articles
which(inf$stud.res>3)
which(inf$stud.res<(-3))
stud=inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x = 1:length(stud),y= stud,labels = ifelse(stud < -3 | stud > 3, 1:length(stud), ""), col = "red", pos = 3)

# cook distance
h2 = 4/n
h2
which(inf$cooks>h2)
cooksd = cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="red")

# Removing Outliers and creating new data
data_aykırılar_silindi2=data2[-c(8,17,52, 20,21,22,23,24,19), ]
write.table(data_aykırılar_silindi2,"data3.txt",row.names=FALSE)

### Rebuilding the Model ###           

#CLEAN WORKSPACE BEFORE STARTING
rm(list=ls())  

sonveri=read.table("c:/data3.txt",header = T)
names(sonveri)=c("verim","isik","sicaklik","su","mineraller")
names(sonveri)
attach(sonveri)
mineraller=as.factor(mineraller)
basicStats(sonveri)

qqnorm(verim)
qqline(verim)
shapiro.test(verim)

verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)


data_birleşik=cbind(verim,isik,sicaklik,su,mineraller)
pairs(data_birleşik)

yenimodel=lm(sqrt_verim~isik+sicaklik+su+mineraller) # new model
yenimodel
summary(yenimodel) # test statistics

confint(yenimodel, level=0.99) # confidence intervals


### Assumption Distortions ###
# Variable Variance 
inf=ls.diag(yenimodel)
plot(predict(yenimodel),inf$stud.res,ylab="Student Tip Artıklar",xlab="Tahmin Değerleri") # grafik
bptest(yenimodel) # Breusch-Pagan Testi
 summary(lm(abs(residuals(yenimodel))~fitted(yenimodel)))

# Otokorelasyon 
dwtest(yenimodel)

# Multiple Connectivity 
inf=ls.diag(yenimodel)
inf
detach("package:car", unload=TRUE)
vif(yenimodel) #1st way to vif
library(olsrr) 
ols_vif_tol(yenimodel) #2st way to vif
library(olsrr)
ols_eigen_cindex(yenimodel)


### Variable Selection ###
library(stats)
boşmodel=lm(Üretim ~ 1)
ileriye=step(boşmodel,Üretim~Kod+Test+Dizayn+Platform,  direction = "forward") #the choice forward
ileriye
summary(ileriye)

geriye=step(yenimodel, direction="backward") #backward selection
summary(geriye)

library(MASS)
adımsal=stepAIC(yenimodel, direction = "both", trace = FALSE) #stepwise selection
summary(adımsal)


### Ridge Regression ###
library(MASS)
ridge=lm.ridge(Üretim~Kod+Test+Dizayn+Platform ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 0]
