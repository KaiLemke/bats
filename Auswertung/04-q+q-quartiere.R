# Kastenzahlen zu x.koa und Levels von x.kz zugeordnet
df.kz <- subset(df, select=c("x.koa", "x.kz"))
df.kz$freq <- rep(0, length(df.kz$x.koa)) # leerer Vektor der richtigen Länge

# ermittelt die Anzahl der kontrollierten Kästen für die Kombination aus x.koa und x.kz
f.temp <- function(i) {
	x <- df.kz$x.koa[i]
	y <- df.kz$x.kz[i]
	temp <- subset(df.kz, x.koa == x & x.kz == y)
	temp <- length(temp$freq)
	temp
} 

# Schreibt die Werte in den Vektor
for(i in seq_along(df.kz$freq)) {
	df.kz$freq[i] <- f.temp(i)
}


# kontrollierte Kästen pro Jahr
tab.koa <- table(df$x.koa) 
v.koa <- as.numeric(tab.koa)

# kontrollierte Schläge pro Jahr
v.schlag <- rep(0, length(levels(df$x.koa))) # leerer Vektor der Länge der Kontrollen
f.temp <- function(x){
	temp <- subset(df, x.koa == x, select="x.schlag")
	temp <- as.numeric(table(temp))
	temp <- subset(temp, temp != 0)
	temp <- length(temp)
	temp
} # ermittelt die Anzahl der kontrollierten Schläge (Level != 0)
for(i in 1:6){
	x <- c(10:15)[i]
	v.schlag[i] <- f.temp(x)
} # schreibt die Werte in den Vektor


par(mfrow=c(2,1))

# leerer Plot
plot(
      freq ~ x.koa, 
      data = df.kz,
      ylim = c(0, max(m.schlag[, -7]) + 20),
      type = "n",
      axes = 0,
      main = "kontrollierte Kästen pro Jahr und Zustand",
      xlab = "Kontrolljahr",
      ylab = "kontrollierte Kästen"
      )
axis(1)
axis(2, at=seq(0, max(m.schlag[, -7]) + 20, 20))
box()

# Punkte für die Kastenzustände
for(i in seq_along(levels(df.kz$x.kz))) {
	points(
	     freq ~ x.koa,
	     data = subset(
			   df.kz,
			   x.kz == levels(df.kz$x.kz)[i]
			   ),
	     pch = i
	     )
}

# Summe der kontrollierten Kästen pro Jahr
lines(as.numeric(m.schlag["Summe",-7]) ~ c(10:15), lty=3)

legend(
       14, 170, 
       c(levels(df.kz$x.kz), "Summe"),
       pch=c(1:4, NA),
       lty=c(rep(0,4),3)
       )

plot(
     as.numeric(m.schlag["Summe", -7]) / v.schlag ~ c(10:15),
     type="l", 
     lty=5,
     main="kontrollierte Kästen pro Jahr und Schlag",
     xlab = "Kontrolljahr",
     ylab = "kontrollierte Kästen"
     )


# Werte von v.schlag in data frame mit den beiden freq's
df.kz.schlag <- data.frame(
			   x.koa = df$x.koa,
			   x.kz = df$x.kz,
			   x.schlag = df$x.schlag,
			   counts.kz = df.kz$freq,
			   num.schlag = rep(0, length(df$x.koa))
			   )
f.temp <- function(x) {
	temp <- c(10:15)
	x <- which(temp == x)
	v.schlag[x]
}
for(i in seq_along(df.kz.schlag$x.koa)) {
	x <- df.kz.schlag$x.koa[i]
	df.kz.schlag$num.schlag[i] <- f.temp(x)
}

# Anzahl der kontrollierten Schläge als Korrektur 
# für die Anzahl der kontrollierten Kästen
df.kz.schlag$freq <- df.kz.schlag$counts.kz / df.kz.schlag$num.schlag


# Regression

Model2 <- aov(freq ~ x.koa, data=df.kz.schlag)
summary(Model2)

par(mfrow=c(2,2))
plot(Model2)
# Modell passt überhaupt nicht!

# PSEUDOREPLIKATION: 523 DF!!!


v.freq <- rep(0,6)

for(i in 10:15) {
	temp <- c(10:15)
	x <- which(temp == i)
	v.freq[x] <- with(
			  subset(df.kz.schlag, x.koa == i),
			  mean(freq)
			  )
}

Model3 <- aov(v.freq ~ temp)

summary(Model3)

par(mfrow=c(2,2))
plot(Model3)


# Kastenzustände

for(i in 10:15) {
	temp <- c(10:15)
	x <- which(temp == i)
	v.freq[x] <- with(
			  subset(df, x.koa == i),
			  mean(y.kz)
			  )
}

Model4 <- aov(v.freq ~ temp)

summary(Model4)

par(mfrow=c(1,1))

plot(freq ~ x.kz, data=df.kz)
