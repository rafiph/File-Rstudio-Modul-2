#1A
# memasukkan nilai X
X <- c(78, 75, 67, 77, 70, 72, 78, 70, 77) 

#memasukkan nilai Y
Y <- c(100, 95, 70, 90, 90, 90, 89, 100, 100)

# mencari nilai selisih dari X dan Y
D <- Y - X 
#

#mencari nilai standar deviasi menggunakan selisih
StandarDeviasi <- sd(D)
paste("Standar Deviasi = ", StandarDeviasi)

#1B
#mencari nilai t(p-value)
#t.test (Y, X, paired = TRUE, var.equal = FALSE)
t_test = t.test(Y, X, paired = TRUE, alternative = "two.sided")
t_test 
#1C Kesimpulan
alpha = 0.05
if(t_test$p.value < alpha){
  cat("tolak H0: Tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴")
} else{
  cat("terima H0: Ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴")
};

#NOMOR 2
install.packages("BSDA")
library(BSDA)

# Data yang diberikan
n <- 100
x.mean <- 23500
x.sd <- 3000
pop_mean_claim <- 25000
alpha <- 0.05

#Hipotesis H0 H1
#H0 :Rata - rata mobil dikemudikan <= 20.000 kilometer per tahun.
#H1 :Rata - rata mobil dikemudikan > 20.000 kilometer per tahun.

#2A apakah setuju dengan pernyataan di soal? kenapa?
zsum.test(mean.x = 23500, sigma.x = 3000, n.x = 100, alternative = "greater",
          mu = 20000, conf.level = 0.95)

#NOMOR 3
library(BSDA)  #untuk uji t dua sampel
install.packages("vctrs")
install.packages("mosaic")
library(mosaic)

#data yang diberikan
n_bandung <- 20
n_bali <- 27
mean_bandung <- 3.64
mean_bali <- 2.79
sd_bandung <- 1.67
sd_bali <- 1.5
alpha <- 0.05
confident_level <- 0.95

#3A
#H0: μ_bandung = μ_bali
#H1: μ_bandung ≠ μ_bali

#3B
tsum.test(mean.x=mean_bandung, s.x = sd_bandung, n.x = n_bandung, 
          mean.y =mean_bali, s.y = sd_bali, n.y = n_bali, 
          alternative = "two.side", var.equal = TRUE,
          conf.level = confident_level)

#3C
#Hitung pooled variance dan pooled SD
sp2 <- ((n_bandung - 1) * sd_bandung^2 + (n_bali - 1) * sd_bali^2) / (n_bandung + n_bali - 2)
sp <- sqrt(sp2)

#Hitung t hitung
t_hitung <- (mean_bandung - mean_bali) / (sp * sqrt(1/n_bandung + 1/n_bali))

#Output nilai t hitung
t_hitung

#3D Nilai Kritikal
df <- 2
t_kritis <- qt(1 - alpha/2, df)
t_kritis

#3E Keputusan
if (abs(t_hitung) > t_kritis) {
  keputusan <- "Tolak H0 (ada perbedaan signifikan rata-rata jumlah saham Bandung dan Bali)"
} else {
  keputusan <- "Terima H0 (tidak ada perbedaan signifikan rata-rata jumlah saham Bandung dan Bali)"
}
keputusan

#NOMOR 4
#Data
Glass <- c(
  rep("A", 3), rep("B", 3), rep("C", 3),
  rep("A", 3), rep("B", 3), rep("C", 3),
  rep("A", 3), rep("B", 3), rep("C", 3)
)

Temp <- c(
  rep(100, 9), rep(125, 9), rep(150, 9)
)

Light <- c(
  # 100 derajat
  580, 568, 570,    # A
  550, 530, 579,    # B
  546, 575, 599,    # C
  # 125 derajat
  1090, 1087, 1085, # A
  1070, 1035, 1000, # B
  1045, 1053, 1066, # C
  # 150 derajat
  1392, 1380, 1386, # A
  1328, 1312, 1299, # B
  867, 904, 889     # C
)

data <- data.frame(Glass, Temp, Light)

#4A Plot sederhana Visualisasi data
library(ggplot2)
ggplot(data, aes(x = factor(Temp), y = Light, fill = Glass)) +
  geom_boxplot() +
  labs(title = "Visualisasi Keluaran Cahaya",
       x = "Suhu Operasi (°C)", y = "Keluaran Cahaya") +
  theme_minimal()

#4B ANOVA dua arah
anova_model <- aov(Light ~ factor(Temp) * Glass, data = data)
summary(anova_model)

#4C tabel mean dan standar deviasi keluaran cahaya untuk setiap perlakuan
library(dplyr)
hasil <- data %>%
  group_by(Glass, Temp) %>%
  summarise(
    Mean = mean(Light),
    SD = sd(Light),
    .groups = 'drop'
  )

hasil
