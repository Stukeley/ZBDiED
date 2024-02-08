# ZBDiED, Lab 6, Rafał Klinowski
library(class)
library(gmodels)

setwd("/Users/stukeleyak/Desktop/Studia/Studia magisterskie sem 2/ZBDIED/Lab6")

# Algorytm kNN dla danych dotyczących raka
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)

# Usunięcie pierwszej kolumny
wbcd <- wbcd[-1]

table(wbcd$diagnosis)

# Zmiana oznaczeń klas
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("łagodny", "złośliwy"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# Wartości pierwszych trzech zmiennych
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# Utworzenie funkcji do normalizacji
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

# Przetestowanie funkcji
normalize(c(1,2,3,4,5,6))

# Normalizacja zbioru
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])

# Zbudowanie zbioru treningowego i testowego
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_diag <- wbcd[1:469, 1]
wbcd_test_diag <- wbcd[470:569, 1]

# Utworzenie klasyfikatora kNN
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_diag, k = 21)

CrossTable(x = wbcd_test_diag, y = wbcd_test_pred,prop.chisq=FALSE)

# Powtórzenie powyższych kroków po standaryzacji
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z)

wbcd_train_z <- wbcd_z[1:469, ]
wbcd_test_z <- wbcd_z[470:569, ]

wbcd_test_pred_z <- knn(train = wbcd_train_z, test = wbcd_test_z,cl = wbcd_train_diag, k = 3)
CrossTable(x = wbcd_test_diag, y = wbcd_test_pred_z,prop.chisq=FALSE)

# Powtórzenie dla k=15, normalizacja
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_diag, k = 15)
CrossTable(x = wbcd_test_diag, y = wbcd_test_pred,prop.chisq=FALSE)

# Powtórzenie dla k=15, standaryzacja
wbcd_train_z <- wbcd_z[1:469, ]
wbcd_test_z <- wbcd_z[470:569, ]
wbcd_test_pred_z <- knn(train = wbcd_train_z, test = wbcd_test_z,cl = wbcd_train_diag, k = 15)
CrossTable(x = wbcd_test_diag, y = wbcd_test_pred_z,prop.chisq=FALSE)

# Powtórzenie dla k=5
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_diag, k = 5)
CrossTable(x = wbcd_test_diag, y = wbcd_test_pred,prop.chisq=FALSE)

# Powtórzenie dla k=3
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_diag, k = 3)
CrossTable(x = wbcd_test_diag, y = wbcd_test_pred,prop.chisq=FALSE)

# Algorytm k-Means dla danych dotyczących profili społecznościowych
ds<- read.table(header = TRUE,"snsdata.csv",sep = ",")
str(ds)

# Eksploracja danych
# Histogram dla zmiennej gender
genders <- table(ds$gender)
barplot(genders)

# Uzupełnienie brakujących wartości
# Gender
ds$gender<-ifelse(is.na(ds$gender), 3, ds$gender)

# Age
summary(ds$age)

ds$age <- ifelse(ds$age >= 13 & ds$age < 20, ds$age, NA)
summary(ds$age)

# Zamiana brakujących danych na średnie
aggregate(data = ds, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(ds$age, ds$gradyear, FUN =function(x) mean(x, na.rm = TRUE))
tail(ave_age, n=200)

ds$age <- ifelse(is.na(ds$age), ave_age, ds$age)
summary(ds$age)

# Usunięcie brakujących danych dotyczących zainteresowań
ds <- na.omit(ds)

# Normalizacja danych
interests <- ds[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

summary(interests_z)

# k-Means dla k=5
teen_clusters <- kmeans(interests_z, 5)
teen_clusters$size
teen_clusters$centers

# k-Means dla k=4
teen_clusters <- kmeans(interests_z, 4)
teen_clusters$size
teen_clusters$centers

# k-Means dla k=12
teen_clusters <- kmeans(interests_z, 12)
teen_clusters$size
teen_clusters$centers

# k-Means dla k=2
teen_clusters <- kmeans(interests_z, 2)
teen_clusters$size
teen_clusters$centers

# Analiza wartości k
library(ggplot2)
library(stats)
ssd <- numeric(20)  # Zakładamy, że sprawdzamy od 1 do 20 klastrów

for (k in 1:20) {
  kmeans_model <- kmeans(interests_z, centers = k)
  ssd[k] <- kmeans_model$tot.withinss
}

elbow_plot <- ggplot() +
  geom_line(aes(x = 1:20, y = ssd), color = "blue") +
  geom_point(aes(x = which.min(ssd), y = min(ssd)), color = "red", size = 3) +
  labs(title = "Elbow Method",
       x = "Number of Clusters (k)",
       y = "Sum of Squared Distances (SSD)") +
  theme_minimal()

print(elbow_plot)
