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
toprak_turu=as.factor(toprak_turu)
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
data_aykirilar_silindi=data[-c(17 ,33, 46, 64 ,69 ,72), ]
write.table(data_aykirilar_silindi,"sonveri.txt",row.names = FALSE)

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
data_birlestirme=cbind(verim,isik,sicaklik,su,mineraller)
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
data_aykırılar_silindi2=data2[-c(17, 19, 20, 21,22,23,24,8,52), ]
write.table(data_aykırılar_silindi2,"sonveri2.txt",row.names=FALSE)

### Modelin Yeniden Kurulması ###           

#BAŞLAMADAN ÖNCE WORKSPACE'i TEMİZLEYİN
rm(list=ls())  

sonveri=read.table("c:/sonveri2.txt",header = T)
names(sonveri)=c("verim","isik","sicaklik","su","mineraller")
names(sonveri)
attach(sonveri)
mineraller=as.factor(mineraller)
basicStats(sonveri)

qqnorm(verim)
qqline(verim)
shapiro.test(verim)

lnverim=log(verim)
qqnorm(lnverim)
qqline(lnverim)
shapiro.test(lnverim)

verim_kok = verim -min(verim) +1
sqrt_verim=sqrt(verim_kok)
qqnorm(sqrt_verim)
qqline(sqrt_verim)
shapiro.test(sqrt_verim)

# doğrusallık
data_birleşik=cbind(verim,isik,sicaklik,su,mineraller)
pairs(data_birleşik)

# yeni model
yenimodel=lm(sqrt_verim~isik+sicaklik+su+mineraller) # yeni model
yenimodel
summary(yenimodel) # test istatistikleri

# güven aralıkları
confint(yenimodel, level=0.99) 


### Varsayım Bozulumları ###
# Değişen Varyanslılık 
inf=ls.diag(yenimodel)
plot(predict(yenimodel),inf$stud.res,ylab="Student Tip Artıklar",xlab="Tahmin Değerleri") # grafik
library(lmtest)
bptest(yenimodel) # Breusch-Pagan Testi
summary(lm(abs(residuals(yenimodel))~fitted(yenimodel)))

# Otokorelasyon 
library(lmtest)
dwtest(yenimodel)

# Çoklu Bağlantı 
inf=ls.diag(yenimodel)
inf
library(DAAG)
detach("package:car", unload=TRUE)
vif(yenimodel) #vif için 1.yol
library(olsrr) 
ols_vif_tol(yenimodel) #vif için 2.yol
library(olsrr)
ols_eigen_cindex(yenimodel)


### Değişken Seçimi ###
lm.null=lm(verim~1)
forward=step(lm.null ,verim~isik+sicaklik+su+mineraller, direction = "forward")
forward
summary(forward)

library(stats)
boşmodel=lm(sqrt_verim ~ 1)
ileriye=step(sqrt_verim~isik+sicaklik+su+mineraller,  direction = "forward") #ileriye doğru seçim
ileriye
summary(ileriye)
geriye=step(yenimodel, direction="backward") #geriye doğru seçim
summary(geriye)

library(MASS)
adımsal=stepAIC(yenimodel, direction = "both", trace = FALSE) #adımsal seçim
summary(adımsal)


### Ridge Regresyon ###
library(MASS)
ridge=lm.ridge(Üretim~Kod+Test+Dizayn+Platform ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 0.4]