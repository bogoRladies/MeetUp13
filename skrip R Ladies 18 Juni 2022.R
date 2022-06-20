library("dplyr")
library("ggpubr")
library("gridExtra")
library("dslabs")
library(psych)
library(caret)
library(car)
library(tidyverse)

#memanggil data 'temp_carbon'
#simpan sebagai 'db.data'

db.data <- temp_carbon

#cek data
View(db.data)

#buat data terdistribusi normal, simpan sebagai 'banding'
banding<- rnorm(10000)

#Menggunakan plot densitas (density plot)
p1<-ggdensity(db.data$temp_anomaly, 
              main = "Plot densitas suhu anomali",
              xlab = "Suhu anomali")
p2<-ggdensity(banding,
              main="Data pembanding (normal dist)",
              xlab="Data pembanding")

grid.arrange(p1,p2, ncol=1, nrow=2)

#Uji normal dengan Shapiro-Wilk normality test (pada data)

shapiro.test(db.data$temp_anomaly)


#Uji normal dengan Shapiro-Wilk normality test (pada model)
#lakukan regresi linear
m1<-lm(temp_anomaly~year, data=db.data)

#periksa distribusi residual model m1
shapiro.test(resid(m1))
hist(resid(m1))

#distribusi normal, maka sudah tepat menggunakan regresi linear biasa (Gaussian)

# 2. Heterogeneity of variances     
# Cara menguji pelanggaran homogeneity
# Kita coba me-run persamaan kita menggunakan simple linear regression, 
# lihat distribusi residualnya

m2<-lm(temp_anomaly~year + carbon_emissions, data=db.data) 
#cek summary model
summary(m2)
#ekstrak residual dari model kita
res.m2<-resid(m2)
#pengamatan visual untuk melihat apakah tidak ada pola pada residual data kita
plot(res.m2)

#hitung AIC (Akaike Criterion)
#biasanya untuk dibandingkan dengan model lain
AIC(m2)

# Uji multicollinearity (visual)
#kita ingin tahu apakah emisi karbon dipengaruhi oleh
#temperatur anomali, anomali temparatur darat, atau anomali temperatur laut
#di sini sebetulnya kita sudah tahu bahwa nilai temparatur anomali
#merupakan penjumlahan dari anomali temperatur darat dan anomali temperatur laut
#prediksi kita, korelasi antar ketiga variabel ini akan tinggi

#pertama kita membuat subset data
db.data.col<-db.data[c(2:4)] #pilih variabel 2-4 dari dataset

# Uji multicollinearity (visual)

pairs.panels(db.data.col, method = "pearson", # correlation method
             hist.col = "#00AFBB", density = TRUE,  # show density plots
             ellipses = TRUE# show correlation ellipses
)

# Uji multicollinearity (VIF)

lm3 <- lm(carbon_emissions~temp_anomaly+land_anomaly+
            ocean_anomaly+year, 
          data = db.data)
vif(lm3)

#Kita bandingkan

lm4 <- lm(carbon_emissions~temp_anomaly+year, 
          data = db.data)
vif(lm4)

# Tugas & kunci jawaban

#Bacalah dataset "us_contagious_diseases" dari package "dslabs". 
#Dataset ini berisi data daftar penyakit menular di Amerika Serikat. 
#Subset penyakit polio saja. Lalu cari tahu:  
  
#Apakah jumlah penyakit polio (count) bisa dijelaskan oleh tahun (year) dan jumlah populasi (population) kota?
  
#Eksplorasi data tsb seperti contoh data temperatur di atas!   
#1. cek distribusi normal residual (plot histogram, shapiro test, uji visual)  
#2. cek homogeneity of variances dari residual (visual)  
#3. cek korelasi antar variabel penjelas (visual dan VIF)  
#4. tulis kesimpulan & buat visualisasi

#Kunci jawaban A untuk membaca data dan subset data polio
#simpan di variabel baru bernama 'all.dis'
all.dis<-us_contagious_diseases
#subset hanya polio dengan tidyverse
pol.dis<-all.dis %>% 
  filter(disease=="Polio")

#Kunci jawaban B
#Usahakan coba ini secara mandiri sebelum mengecek kunci jawaban!

#buat model yg ingin diuji
#jumlah penyakit = y
#tahun dan jumlah populasi = x1 dan x2

#cek nama variabel
names(pol.dis)

#buat model
lm1<-lm(count~year+population,
        data=pol.dis)
summary(lm1)
#visual
hist(resid(lm1))
#uji shapiro wilk
shapiro.test(resid(lm1))
#uji visual
plot(lm1)
#homogeneity of variances
plot(resid(lm1))
#korelasi antar variabel penjelas
#buat subset year dan population dulu
pol.sub<-pol.dis[c(3,6)] 
pairs.panels(pol.sub, method = "pearson", # correlation method
             hist.col = "#00AFBB", density = TRUE,  # show density plots
             ellipses = TRUE# show correlation ellipses
)
#vif
vif(lm1)
#plot
ggplot(data = pol.dis, aes(x = year, y = count)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE)
ggplot(data = pol.dis, aes(x = year, y = population)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE)
