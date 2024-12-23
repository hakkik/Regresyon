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

#datamızı çekiyoruz
data=read.table("c:/file 42_2220381067_HakkiKondak.txt", header = T)
names(data)
names(data) = c("verim","isik","sicaklik","su","mineraller")
attach(data)
toprak_turu=as.factor(mineraller)
basicStats(data)

#noramllik sağlıyor mu diye bakıyoruz sağlamıyor
qqnorm(verim)
qqline(verim)
shapiro.test(verim)

#Kolmogorov-swirnov testi
ks.test(verim, "pnorm", mean = mean(verim), sd = sd(verim))

#log dönüşümü yaparak tekrar kontrol ediyoruz yine sağlamıyor
lnverim=log(verim)
qqnorm(lnverim)
qqline(lnverim)
shapiro.test(lnverim)

#karekök dönüşümü bakıyoruz yine yok 
verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)

#boxplottan artık değerlerini buluyoruz
boxplot(verim)
which(verim %in% boxplot(verim) $out)

#artık değerlerimizi çıkarıp yeni verimizi oluşturuyoruz
data_aykirilar_silindi=data[-c(17 ,33, 46, 64 ,69 ,72, 55,18), ]
write.table(data_aykirilar_silindi,"sonveri3.txt",row.names = FALSE)

#Environmentimizi temizliyoruz
rm(list = ls())

#yeni verimizi çekiyoruz
data2=read.table("c:/sonveri.txt",header = T)
names(data2) = c("verim","isik","sicaklik","su","mineraller")
attach(data2)
mineraller=as.factor(mineraller)
basicStats(data2)

#yeni verimizin normallik dağılımına bakıyoruz yine sağlamıyor
qqnorm(verim)
qqline(verim)
shapiro.test(verim)

#log dönüşümü
lnverim=log(verim)
qqnorm(lnverim)
qqline(lnverim)
shapiro.test(lnverim)

#yeni verimize karekök dönüşümü yapıyoruz sağlıyor artık
verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)

# Doğrusallık
data_birlestirme=cbind(sqrt_verim,isik,sicaklik,su,mineraller)
pairs(data_birlestirme)

# Model
model=lm(sqrt_verim~isik+sicaklik+su+mineraller)
model
summary(model)

#guven araligi bulma
confint(model ,level = 0.99)

# Artık İncelemesi
inf=ls.diag(model)
inf
influence.measures(model)
n=74
k=5

#k yı bulma kodu
toplam_katsayi = length(coef(model))
k = toplam_katsayi - 1
print(k)

## Artık İncelemesi ##

#hat uzaklığı
h1 = 2*(k+1)/n
h1
which(inf$hat> h1)
hat=inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/n , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="red")

#standart artiklar
which(inf$std.res>2)
which(inf$std.res<(-2))
std=inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text( x = 1:length(std), y = std,labels = ifelse(std < -2 | std > 2, 1:length(std), ""),col = "red", pos = 3 )

#student artiklar
which(inf$stud.res>3)
which(inf$stud.res<(-3))
stud=inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x = 1:length(stud),y= stud,labels = ifelse(stud < -3 | stud > 3, 1:length(stud), ""), col = "red", pos = 3)

#cook uzakligi
h2 = 4/n
h2
which(inf$cooks>h2)
cooksd = cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="red")

# Aykırı Değerlerin Çıkartılması ve yeni verinin oluşturulması
data_aykırılar_silindi2=data2[-c(17, 20, 21, 22, 23, 24, 19, 28, 38, 45, 51, 54, 62, 64, 84 ,52), ]
write.table(data_aykırılar_silindi2,"sonveri3.txt",row.names=FALSE)

### Modelin Yeniden Kurulması ###           

#BAŞLAMADAN ÖNCE WORKSPACE'i TEMİZLEYİN
rm(list=ls())  

sonveri=read.table("c:/sonveri3.txt",header = T)
names(sonveri)=c("verim","isik","sicaklik","su","mineraller")
names(sonveri)
attach(sonveri)
mineraller=as.factor(mineraller)
basicStats(sonveri)

qqnorm(verim)
qqline(verim)
shapiro.test(verim)

#log dönüşümü
lnverim=log(verim)
qqnorm(lnverim) 
qqline(lnverim)
shapiro.test(lnverim)

#karekök dönüşümü
verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)

# doğrusallık
data_birleşik=cbind(sqrt_verim,isik,sicaklik,su,mineraller)
pairs(data_birleşik)

# yeni model
yenimodel=lm(sqrt_verim~isik+sicaklik+su+mineraller) # yeni model
yenimodel
summary(yenimodel) # test istatistikleri

# güven aralıkları
confint(yenimodel, level=0.99) 

# Artık İncelemesi
inf=ls.diag(yenimodel)
inf
influence.measures(yenimodel)
n=59
k=5

#k yı bulma kodu
toplam_katsayi = length(coef(yenimodel))
k = toplam_katsayi - 1
print(k)

## Artık İncelemesi ##

h1 = 2*(k+1)/n
h1
which(inf$hat> h1)
hat=inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/n , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="red")

#standart artiklar
which(inf$std.res>2)
which(inf$std.res<(-2))
std=inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text( x = 1:length(std), y = std,labels = ifelse(std < -2 | std > 2, 1:length(std), ""),col = "red", pos = 3 )

#student artiklar
which(inf$stud.res>3)
which(inf$stud.res<(-3))
stud=inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x = 1:length(stud),y= stud,labels = ifelse(stud < -3 | stud > 3, 1:length(stud), ""), col = "red", pos = 3)

#cook uzakligi
h2 = 4/n
h2
which(inf$cooks>h2)
cooksd = cooks.distance(yenimodel)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="red")

### Varsayım Bozulumları ###
inf=ls.diag(yenimodel)
plot(predict(yenimodel),inf$stud.res,ylab="Student Tip Artıklar",xlab="Tahmin Değerleri") # grafik
library(lmtest)
bptest(yenimodel) # Breusch-Pagan Testi
 summary(lm(abs(residuals(yenimodel))~fitted(yenimodel)))

# Otokorelasyon 
dwtest(yenimodel)

# Çoklu Bağlantı 
inf=ls.diag(yenimodel)
inf
library(DAAG)
detach("package:car", unload=TRUE)
vif(yenimodel) #vif için 1.yol
ols_vif_tol(yenimodel) #vif için 2.yol
ols_eigen_cindex(yenimodel)

dummy=dummy_cols(mineraller)
mineraller1=dummy$.data_1
mineraller2=dummy$.data_2
mineraller3=dummy$.data_3

ort1=mean(isik)
kt1=sum((isik-ort1)^2)
skx1=(isik-ort1)/(kt1^0.5)
ort2=mean(sicaklik)
kt2=sum((sicaklik-ort2)^2)
skx2=(sicaklik-ort2)/(kt2^0.5)
ort3=mean(su)
kt3=sum((su-ort3)^2)
skx3=(su-ort3)/(kt3^0.5)
ort42=mean(mineraller2)
kt42=sum((mineraller2-ort42)^2)
skx42=(mineraller2-ort42)/(kt42^0.5)
ort43=mean(mineraller3)
kt43=sum((mineraller3-ort43)^2)
skx43=(mineraller3-ort43)/(kt43^0.5)
x=cbind(skx1,skx2,skx3,skx42,skx43)
sm=eigen(t(x)%*%x)
signif(sm$values,3)
signif(sm$vectors,3)

#uyum kestirimi veri2 ilk degeri 
uyum = lm(sqrt_verim~isik+sicaklik+su+mineraller)
new = data.frame(isik=c(4.10),sicaklik=c(1.18),su=c(7.91),mineraller=factor(c(1)))
predict(uyum, newdata = new)
guven=predict(uyum, newdata=new,interval="confidence",level=0.95) #guven aral??g?? bulma
print(guven)

#on kestirim
onkes = lm(sqrt_verim~isik+sicaklik+su+mineraller)
new2 = data.frame(isik=c(5.86),sicaklik=c(2.71),su=c(7.49),mineraller=factor(c(1)))
predict(onkes, newdata = new2)
guven2=predict(onkes, newdata=new2 ,interval="confidence",level=0.95)
print(guven2)

#ileriye dogru secilim
lm.null = lm(sqrt_verim ~ 1)
forward = step(lm.null,sqrt_verim~isik+sicaklik+su+mineraller,  direction = "forward")
forward
summary(forward)

#geriye dogru secilim
backward=step(yenimodel,direction="backward")
summary(backward)

#adimsal secilim
step.model = stepAIC(yenimodel, direction = "both", trace = FALSE)
summary(step.model)

#ridge
ridge = lm.ridge(sqrt_verim~isik+sicaklik+su+mineraller ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),ylab=expression(h=0,lwd=2))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 0]
