# Regresyon Analizi Projesi

Bu proje, tarımsal verim üzerinde çeşitli faktörlerin etkisini analiz eden kapsamlı bir regresyon analizi çalışmasıdır. Işık, sıcaklık, su ve mineral seviyelerinin bitki verimi üzerindeki etkilerini incelemektedir.

## İçerik

- Veri normallik analizi
- Aykırı değer tespiti ve temizleme
- Çoklu regresyon modeli oluşturma
- Model varsayımlarının kontrolü
- Ridge regresyon analizi
- İleriye/geriye dönük değişken seçimi

## Kullanılan Kütüphaneler

```R
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
```

## Analiz Adımları

1. **Veri Ön İşleme**
   - Veri normallik kontrolü
   - Log ve karekök dönüşümleri
   - Aykırı değerlerin tespiti ve çıkarılması

2. **Model Oluşturma**
   - Çoklu regresyon modeli kurulumu
   - Model varsayımlarının kontrolü
   - Güven aralıklarının hesaplanması

3. **Model Doğrulama**
   - Artık analizi
   - Cook's distance hesaplaması
   - VIF değerlerinin kontrolü
   - Otokorelasyon testi

4. **Değişken Seçimi**
   - İleriye doğru seçim
   - Geriye doğru seçim
   - Adımsal seçim
   - Ridge regresyon

## Kullanım

1. Veri setinizi projenin ana dizinine yerleştirin
2. Dosya yolunu kendi sisteminize göre güncelleyin:
```R
data=read.table("your_path/your_file.txt", header = T)
```
3. Kodu çalıştırın ve sonuçları analiz edin

## Gereksinimler

- R 4.0.0 veya üzeri
- Yukarıda listelenen R paketleri

## Önemli Notlar

- Veri setinde aykırı değerler temizlenmiştir
- Normallik varsayımı için karekök dönüşümü kullanılmıştır
- Model seçimi için AIC kriteri kullanılmıştır

## İletişim

İsim: Hakkı Kondak
Öğrenci Numarası: 2220381067

## Lisans

Bu proje MIT lisansı altında lisanslanmıştır.
