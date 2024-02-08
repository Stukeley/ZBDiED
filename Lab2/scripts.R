# Ćw. 1.
data(airquality)

str(airquality)
airquality[100:105, 5:6]

vars <- c("Wind", "Temp")
airquality[100:105, vars]

# Ćw. 2.
fn <- system.file("csv", "weather.csv", package="rattle")
file.show(fn)

data <- read.csv("https://rattle.togaware.com/weather.csv")

# Ćw. 3.
# setwd(...)
t1 <- read.table("titanic.csv", header=TRUE, sep=",")
t2 <- read.csv("titanic.csv", header=TRUE, sep=",", quote="\"", dec=".",
               fill=TRUE, comment.char="")
t3 <- read.csv("titanic.csv")

# Ćw. 4.
schowek <- read.table(file("clipboard"), header=TRUE, sep="\t")

# Ćw. 5.
Veg <- read.table(file="Vegetation2.txt", header=TRUE)
names(Veg)
str(Veg)

# Ćw. 6.
m <- mean(Veg$R)

m1 <- mean(Veg$R[Veg$Transect == 1])
m2 <- mean(Veg$R[Veg$Transect == 2])
m3 <- mean(Veg$R[Veg$Transect == 3])
m4 <- mean(Veg$R[Veg$Transect == 4])
m5 <- mean(Veg$R[Veg$Transect == 5])
m6 <- mean(Veg$R[Veg$Transect == 6])
m7 <- mean(Veg$R[Veg$Transect == 7])
m8 <- mean(Veg$R[Veg$Transect == 8])

ms <- c(m1, m2, m3, m4, m5, m6, m7, m8)

tapply(Veg$R, Veg$Transect, mean)

tapply(X = Veg$R, INDEX = Veg$Transect, FUN = mean)

Me <- tapply(Veg$R, Veg$Transect, mean)
Sd <- tapply(Veg$R, Veg$Transect, sd)
Le <- tapply(Veg$R, Veg$Transect, length)

params <- c(Me, Sd, Le)
params

# Ćw. 7.
sapply(Veg[, 5:9], FUN=mean)
lapply(Veg[, 5:9], FUN=mean)

# Ćw. 8.
# 1.
temp <- lapply(c(5,5,5,5,5,5,5), FUN=sample)
names(temp) <- c("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota",
                 "Niedziela")

mins <- sapply(temp, FUN=min)
maxs <- sapply(temp, FUN=max)

# 2.
data(USArrests)
results <- sapply(USArrests, FUN=sum)
results
