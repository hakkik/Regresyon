# Regresyon: Multiple Regression Analysis on Agricultural Yield

This project is a multiple linear regression study in R examining how light, temperature, water, and mineral levels affect agricultural yield. It covers the full workflow: data cleaning, assumption checks, outlier analysis, model building, and variable selection.

## Table of Contents

- [About the Project](#about-the-project)
- [File Structure](#file-structure)
- [Requirements](#requirements)
- [Setup](#setup)
- [Analysis Workflow](#analysis-workflow)
- [Usage](#usage)
- [Results](#results)
- [Notes](#notes)

## About the Project

Using `verim` (yield, the dependent variable) along with `isik` (light), `sicaklik` (temperature), `su` (water), and `mineraller` (minerals) as independent variables, a multiple regression model is built. Model assumptions (normality, linearity, homoscedasticity, autocorrelation, multicollinearity) are tested, and outliers are removed in two rounds to improve the model.

## File Structure

```
├── regresyon ödev.r    # Main analysis script
├── data1.txt           # Raw/original dataset
├── data2.txt           # Dataset after 1st round of outlier removal
├── data3.txt           # Dataset after 2nd round of outlier removal (final)
└── README.md           # Project documentation
```

## Requirements

- R 4.0.0 or higher
- The following R packages:

```r
install.packages(c("car", "fBasics", "lmtest", "MASS", "nortest",
                    "olsrr", "zoo", "fastDummies", "DAAG"))
```

The `stats` package ships with R by default and requires no separate installation.

## Setup

```bash
git clone https://github.com/hakkik/Regresyon.git
cd Regresyon
```

The script reads data files from hardcoded paths (e.g. `c:/data1.txt`). Before running it on your own system, update the file paths inside the script to match your data's location:

```r
data <- read.table("your_path/data1.txt", header = TRUE)
```

## Analysis Workflow

The script follows these steps in order:

1. **Descriptive statistics and normality testing**
   - Summary statistics via `basicStats()`
   - Q-Q plot, Shapiro-Wilk, and Kolmogorov-Smirnov tests
   - Log and square-root transformations are tried to achieve normality

2. **Outlier removal (round 1)**
   - Outliers are detected using boxplots
   - Outliers are removed, producing `data2.txt`

3. **Second normality check and model building**
   - Normality is retested on `data2.txt`
   - Linearity between variables is examined with `pairs()`
   - The first regression model is built via `lm()`, with confidence intervals computed

4. **Residual analysis**
   - Leverage values, standardized and studentized residuals
   - Cook's distance to identify influential observations
   - Outliers found at this stage are removed, producing `data3.txt`

5. **Final model (using `data3.txt`)**
   - The model is rebuilt and summarized
   - **Heteroscedasticity:** Breusch-Pagan test
   - **Autocorrelation:** Durbin-Watson test
   - **Multicollinearity:** VIF and eigenvalue/condition index analysis

6. **Variable selection and Ridge regression**
   - Forward, backward, and stepwise selection methods
   - Ridge regression showing how coefficients change with lambda

## Usage

1. Place `data1.txt`, `data2.txt`, and `data3.txt` in the project folder (or update the paths in the script to match your data).
2. Open `regresyon ödev.r` in R or RStudio.
3. Run the script block by block (step by step), reviewing the plots and test outputs produced at each stage.

> Note: Some sections of the script (variable selection and Ridge regression) reference variable names such as `Üretim`, `Kod`, `Test`, `Dizayn`, and `Platform`, which are not defined in the dataset used elsewhere. You'll need to update these lines with the actual column names from your dataset before running them.

## Python Version

The R workflow has also been ported to Python in `regression_analysis.py`.

Install the Python dependencies:

```bash
pip install -r requirements.txt
```

Run the analysis:

```bash
python regression_analysis.py
```

The Python script reads `data1.txt` from the project folder, reproduces the two outlier-removal rounds, fits the OLS models, runs assumption checks, performs AIC-based variable selection, and runs Ridge regression. It writes:

```text
data2_python.txt
data3_python.txt
plots/
```

`data2_python.txt` and `data3_python.txt` contain the same numeric rows as the R-generated `data2.txt` and `data3.txt`; the only formatting difference is that R writes quoted column names while the Python script writes plain column names.

The `plots/` folder contains Python equivalents of the R visualizations: Q-Q plots for raw/log/square-root yield transformations, the yield boxplot with outlier labels, pair plots, leverage, standardized residual, studentized residual, Cook's distance, fitted-vs-studentized-residual plots, and Ridge coefficient paths.

### Main R vs Python Differences

| Step | R | Python |
| --- | --- | --- |
| Data loading | `read.table()` | `pandas.read_csv(sep=r"\s+")` |
| Descriptive statistics | `fBasics::basicStats()` | `pandas.DataFrame.describe()` |
| Normality tests | `shapiro.test()`, `ks.test()` | `scipy.stats.shapiro()`, `scipy.stats.kstest()` |
| OLS regression | `lm()` | `statsmodels.formula.api.ols()` |
| Categorical variable | `as.factor(mineraller)` | `C(mineraller)` in the formula |
| Influence diagnostics | `ls.diag()`, `influence.measures()` | `model.get_influence().summary_frame()` |
| Breusch-Pagan | `lmtest::bptest()` | `statsmodels.stats.diagnostic.het_breuschpagan()` |
| Durbin-Watson | `lmtest::dwtest()` | `statsmodels.stats.stattools.durbin_watson()` |
| VIF | `car::vif()` / `olsrr` | `statsmodels.stats.outliers_influence.variance_inflation_factor()` |
| Ridge | `MASS::lm.ridge()` | `sklearn.linear_model.Ridge` |

The Python version also fixes two practical issues in the original R script: it uses relative file paths instead of hardcoded `c:/...` paths, and it rewrites the variable-selection/Ridge sections with the actual dataset columns (`verim`, `isik`, `sicaklik`, `su`, `mineraller`) instead of undefined names such as `Üretim`, `Kod`, `Test`, `Dizayn`, and `Platform`.

## Results

Key findings from the analysis:

- The normality assumption was not satisfied on the raw data; a square-root transformation was used to improve it.
- After two rounds of outlier removal, the model's assumptions improved significantly.
- Variable selection methods (forward/backward/stepwise) and Ridge regression are compared to identify the best-fitting model.

## Notes

- This project was prepared as a regression analysis project.
- The code is intended for educational/academic purposes and is not recommended for direct production use.
- File paths are Windows-specific (`c:/...`), so macOS/Linux users will need to update these lines.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Regresyon: Tarımsal Verim Üzerine Çoklu Regresyon Analizi

Bu proje, ışık, sıcaklık, su ve mineral seviyelerinin tarımsal verim üzerindeki etkisini incelemek için R dilinde yapılmış bir çoklu doğrusal regresyon çalışmasıdır. Proje kapsamında; veri temizleme, varsayım kontrolleri, aykırı değer analizi, model kurma ve değişken seçimi adımlarını uçtan uca içerir.

## İçindekiler

- [Proje Hakkında](#proje-hakkında)
- [Dosya Yapısı](#dosya-yapısı)
- [Gereksinimler](#gereksinimler)
- [Kurulum](#kurulum)
- [Analiz Akışı](#analiz-akışı)
- [Kullanım](#kullanım)
- [Sonuçlar](#sonuçlar)
- [Notlar](#notlar)

## Proje Hakkında

Veri setinde yer alan `verim` (bağımlı değişken), `isik`, `sicaklik`, `su` ve `mineraller` (bağımsız değişkenler) kullanılarak bir çoklu regresyon modeli kurulmuş; model varsayımları (normallik, doğrusallık, sabit varyans, otokorelasyon, çoklu bağlantı) test edilmiş ve aykırı gözlemler iki aşamada temizlenerek model iyileştirilmiştir.

## Dosya Yapısı

```
├── regresyon ödev.r    # Ana analiz betiği
├── data1.txt           # Ham/orijinal veri seti
├── data2.txt           # 1. aykırı değer temizliği sonrası veri
├── data3.txt           # 2. aykırı değer temizliği sonrası nihai veri
└── README.md           # Proje dokümantasyonu
```

## Gereksinimler

- R 4.0.0 veya üzeri
- Aşağıdaki R paketleri:

```r
install.packages(c("car", "fBasics", "lmtest", "MASS", "nortest",
                    "olsrr", "zoo", "fastDummies", "DAAG"))
```

`stats` paketi R ile birlikte gelir, ayrıca kurulum gerektirmez.

## Kurulum

```bash
git clone https://github.com/hakkik/Regresyon.git
cd Regresyon
```

Betik veri dosyalarını sabit bir yoldan (`c:/data1.txt` vb.) okuyacak şekilde yazılmıştır. Kendi sisteminizde çalıştırmadan önce betik içindeki dosya yollarını, verilerin bulunduğu klasöre göre güncelleyin:

```r
data <- read.table("your_path/data1.txt", header = TRUE)
```

## Analiz Akışı

Betik şu adımları sırasıyla uygular:

1. **Betimsel istatistikler ve normallik testi**
   - `basicStats()` ile özet istatistikler
   - Q-Q grafiği, Shapiro-Wilk ve Kolmogorov-Smirnov testleri
   - Normalliği sağlamak için log ve karekök dönüşümleri denenir

2. **Aykırı değer temizliği (1. tur)**
   - Boxplot ile aykırı gözlemler tespit edilir
   - Aykırı gözlemler çıkarılarak `data2.txt` oluşturulur

3. **İkinci normallik kontrolü ve model kurulumu**
   - `data2.txt` üzerinde normallik yeniden test edilir
   - Değişkenler arası doğrusallık `pairs()` ile incelenir
   - `lm()` ile ilk regresyon modeli kurulur, güven aralıkları hesaplanır

4. **Artık (residual) analizi**
   - Hat (leverage) değerleri, standardize ve studentize artıklar
   - Cook uzaklığı ile etkili gözlemlerin tespiti
   - Bu adımda bulunan aykırı gözlemler çıkarılarak `data3.txt` oluşturulur

5. **Nihai modelin kurulması (`data3.txt` ile)**
   - Model yeniden kurulur ve özetlenir
   - **Değişen varyans:** Breusch-Pagan testi
   - **Otokorelasyon:** Durbin-Watson testi
   - **Çoklu bağlantı:** VIF ve özdeğer/koşul indeksi analizi

6. **Değişken seçimi ve Ridge regresyon**
   - İleriye, geriye ve adımsal (stepwise) seçim yöntemleri
   - Ridge regresyon ile katsayıların lambda değerine göre değişimi

## Kullanım

1. `data1.txt`, `data2.txt`, `data3.txt` dosyalarını proje klasörüne yerleştirin (veya betikteki yolları kendi verinize göre güncelleyin).
2. `regresyon ödev.r` dosyasını R veya RStudio'da açın.
3. Betiği bloklar halinde (adım adım) çalıştırın; her aşamada üretilen grafikleri ve test çıktılarını inceleyin.

> Not: Betik bazı bölümlerde `Üretim`, `Kod`, `Test`, `Dizayn`, `Platform` gibi veri setinde tanımlı olmayan değişken adları içerir (değişken seçimi ve Ridge regresyon bölümleri). Bu satırları çalıştırmadan önce kendi veri setinizdeki sütun adlarıyla güncellemeniz gerekir.

## Python Sürümü

R iş akışı `regression_analysis.py` dosyasıyla Python'a da taşınmıştır.

Python bağımlılıklarını kurmak için:

```bash
pip install -r requirements.txt
```

Analizi çalıştırmak için:

```bash
python regression_analysis.py
```

Python betiği proje klasöründeki `data1.txt` dosyasını okur; iki aşamalı aykırı değer temizliğini, OLS modellemeyi, varsayım testlerini, AIC tabanlı değişken seçimini ve Ridge regresyonu uygular. Çalışınca şu çıktıları üretir:

```text
data2_python.txt
data3_python.txt
plots/
```

`data2_python.txt` ve `data3_python.txt`, R ile üretilen `data2.txt` ve `data3.txt` dosyalarıyla aynı sayısal satırlara sahiptir. Tek fark yazım biçimindedir: R sütun adlarını tırnaklı, Python ise tırnaksız yazar.

`plots/` klasörü R'deki görselleştirmelerin Python karşılıklarını içerir: ham/log/karekök dönüşümlü verim Q-Q grafikleri, aykırı değer etiketli verim boxplot grafiği, değişkenler arası pair plot, leverage, standardize artık, studentize artık, Cook uzaklığı, tahmin-studentize artık grafiği ve Ridge katsayı yolları.

### Temel R ve Python Farkları

| Adım | R | Python |
| --- | --- | --- |
| Veri okuma | `read.table()` | `pandas.read_csv(sep=r"\s+")` |
| Betimsel istatistikler | `fBasics::basicStats()` | `pandas.DataFrame.describe()` |
| Normallik testleri | `shapiro.test()`, `ks.test()` | `scipy.stats.shapiro()`, `scipy.stats.kstest()` |
| OLS regresyon | `lm()` | `statsmodels.formula.api.ols()` |
| Kategorik değişken | `as.factor(mineraller)` | Formülde `C(mineraller)` |
| Etki/aykırı gözlem tanıları | `ls.diag()`, `influence.measures()` | `model.get_influence().summary_frame()` |
| Breusch-Pagan | `lmtest::bptest()` | `statsmodels.stats.diagnostic.het_breuschpagan()` |
| Durbin-Watson | `lmtest::dwtest()` | `statsmodels.stats.stattools.durbin_watson()` |
| VIF | `car::vif()` / `olsrr` | `statsmodels.stats.outliers_influence.variance_inflation_factor()` |
| Ridge | `MASS::lm.ridge()` | `sklearn.linear_model.Ridge` |

Python sürümü ayrıca özgün R betiğindeki iki pratik sorunu giderir: `c:/...` gibi sabit yollar yerine proje içi göreli yolları kullanır ve değişken seçimi/Ridge bölümlerini veri setindeki gerçek sütun adlarıyla (`verim`, `isik`, `sicaklik`, `su`, `mineraller`) çalışacak şekilde düzenler.

## Sonuçlar

Analiz sonucunda:

- Ham veride normallik varsayımı sağlanamamış, karekök dönüşümü ile iyileştirme yapılmıştır.
- İki aşamalı aykırı değer temizliği sonrasında model varsayımları önemli ölçüde iyileşmiştir.
- Değişken seçimi yöntemleri (ileri/geri/adımsal) ve Ridge regresyon ile en uygun model karşılaştırmalı olarak sunulmuştur.

## Notlar

- Bu proje bir ders ödevi (regresyon ödevi) kapsamında hazırlanmıştır.
- Kod, öğretici/akademik amaçlıdır; üretim ortamı için doğrudan kullanılması önerilmez.
- Dosya yolları Windows'a özgü (`c:/...`) yazıldığından macOS/Linux kullanıcılarının bu satırları güncellemesi gerekir.
