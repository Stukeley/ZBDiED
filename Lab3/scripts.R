# ZBDIED, Lab 3
# Rafał Klinowski

setwd("/Users/stukeleyak/Desktop/Studia/Studia magisterskie sem 2/ZBDIED/Lab3")

# Ćw 1
Veg <- read.table(file="Vegetation2.txt", header=TRUE)
plot(x = Veg$BARESOIL, y = Veg$R)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness",
     main = "Scatter plot",
     xlim = c(0,45),
     ylim = c(4,19),
     pch=8, # Symbole
     col=2) # Kolor

# Zaawansowany wykres
Veg$Time2 <- Veg$Time
Veg$Time2[Veg$Time <= 1974] <- 15
Veg$Time2[Veg$Time > 1974] <- 16
Veg$Col2 <- Veg$Time
Veg$Col2[Veg$Time <= 1974] <- 1
Veg$Col2[Veg$Time > 1974] <- 2

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness",
     main = "Scatter plot",
     xlim = c(0,45),
     ylim = c(4,19),
     pch=Veg$Time2,
     col=Veg$Col2,
     cex=1.5) # Rozmiar symbolu

# Ćw 3
Owls <- read.table(file="Owls.txt", header=TRUE)
names(Owls)
str(Owls)
AllNests <- unique(Owls$Nest)

Owls.ATV <- Owls[Owls$Nest == "AutavauxTV",]

plot(x = Owls.ATV$ArrivalTime,
     y = Owls.ATV$NegPerChick,
     xlab = "Arrival Time", main = "AutavauxTV",
     ylab = "Negotiation behaviour")

Owls.Bot <- Owls[Owls$Nest == "Bochet", ]
plot(x = Owls.Bot$ArrivalTime,
     y = Owls.Bot$NegPerChick,
     xlab = "Arrival Time",
     ylab = "Negotiation behaviour", main = "Bochet")

# Automatyzacja

for (i in 1:27){
  Nest.i <- AllNests[i]
  Owls.i <- Owls[Owls$Nest == Nest.i, ]
  plik <- paste("Wykresy/", Nest.i, ".jpg", sep = "")
  # Zapis do pliku zamiast na ekran
  jpeg(file = plik)
  plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
       xlab = "Arrival Time",
       ylab = "Negotiation behaviour", main = Nest.i)
  dev.off()
}

# Ćw 4
library(package="lattice")
install.packages("mlmRev")

# Getting started with Lattice
data(Chem97, package="mlmRev")
head(Chem97)

histogram(~ gcsescore, data = Chem97)

histogram(~ gcsescore | factor(score), data = Chem97)

densityplot(~ gcsescore | factor(score), Chem97, groups = gender,
            plot.points = FALSE, auto.key = TRUE)

# Plotting Lattice
library(datasets)
xyplot(Ozone ~ Wind, data=airquality)

xyplot(Ozone ~ Wind | Month, data=airquality, layout=c(5,1))

p <- xyplot(Ozone ~ Wind, data=airquality)
print(p)

# Ćw 6
# 1
bwplot(factor(score) ~ gcsescore | gender, Chem97)

bwplot(gcsescore ~ gender | factor(score), Chem97, layout = c(6, 1))

# Ćw 6
# 2
Veg <- read.table(file="Vegetation2.txt", header=TRUE)

Veg$Year <- rep(1)
Veg$Year[Veg$Time == 2002] <- 2

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness",
     main = "Wykres Ćw 6",
     xlim = c(0,45),
     ylim = c(4,19),
     cex=Veg$Year)
