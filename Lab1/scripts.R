# Ćw. 28. (praca domowa)

# 1.
liczby <- sample(c(1,2,3,4), 100, replace=TRUE)
czynniki <- factor(liczby, levels=1:4)
levels(czynniki) <- c("czerwony", "zielony", "niebieski", "żółty")
print(czynniki)

summary(czynniki)

# 2.
data(iris)
podzialy <- c(4.3, 5.02, 5.74, 6.46, 7.18, 7.9)
dlugosci <- cut(iris$Sepal.Length, breaks=podzialy, labels=c("(4.3,5.02]", "(5.02, 5.74]", "(5.74,6.46]", "(6.46,7.18]", "(7.18,7.9]"), include.lowest=TRUE)
print(dlugosci)

ilosci <- table(dlugosci)
print(ilosci)

# 3.
data(iris)

dlugosc_ponizej_5 <- iris$Sepal.Length < 5
wyniki <- table(dlugosc_ponizej_5, iris$Species)
rownames(wyniki) <- c("FALSE", "TRUE")

print(wyniki)

# 4.
x <- c(1,3,4,7,11,18,29)
x2 <- list("x*2"=x*2, "x/2"=x/2, "sqrt(x)"=sqrt(x))
print(x2)

element <- x2[["sqrt(x)"]][3:5]
print(element)

# 5.
ramka <- data.frame(
  'wiek' = c(25, 31, 23, 52, 76, 49, 26),
  'wzrost' = c(177, 163, 190, 179, 163, 183, 164),
  'waga' = c(57, 69, 83, 75, 70, 83, 53),
  'płeć' = c('K', 'K', 'M', 'M', 'K', 'M', 'K'),
  row.names = c("Kasia", "Ania", "Tomek", "Piotr", "Maria", "Karol", "Sylwia")
)
print(ramka)

ramka$płeć = c("K", "M", "K", "K", "K", "K", "M")
print(ramka)

# 6.
data(swiss)

dane <- swiss[c(1, 2, 3, 10, 11, 12, 13), c("Examination", "Education", "Infant.Mortality")]

print(dane)
