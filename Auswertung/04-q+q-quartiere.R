# Kastenzustände und Jahre sortiert
fac.kz <- factor(
		 df$kz,
		 levels=c("i", "lb", "sb", "d")
		 )
df.kz <- data.frame(koa=df$koa, kz=fac.kz)

# Kastenzahl pro Zustand und Jahr
tab.kz <- with(
	       df.kz,
	       table(kz, koa)
	       )

# kontrollierte Kästen pro Jahr
tab.koa <- table(df$koa) 
v.koa <- as.numeric(tab.koa)

# kontrollierte Schläge pro Jahr
v.schlag <- rep(0, length(levels(df$koa))) # leerer Vektor der Länge der Kontrollen
f.temp <- function(x){
	temp <- subset(df, koa == x, select="schlag")
	temp <- as.numeric(table(temp))
	temp <- subset(temp, temp != 0)
	temp <- length(temp)
	temp
} # ermittelt die Anzahl der kontrollierten Schläge (Level != 0)
for(
    i in seq_along(
		   levels(df$koa)
		   )
    ){
	x <- levels(df$koa)[i]
	v.schlag[i] <- f.temp(x)
} # schreibt die Werte in den Vektor


# Plot

# kontrollierte Kästen pro Zustand und Jahr
bar.kz <- barplot(
	tab.kz,
	horiz=FALSE,
	col=gray.colors(4),
	legend=c(
		 "intakt", 
		 "leicht beschädigt", 
		 "stark beschädigt", 
		 "defekt"
		 ),
	xlab="Jahr",
	ylab="kontrollierte Kästen",
	ylim=c(0,max(v.koa)+10),
	axes=F
	)
axis(2, at=seq(0, max(v.koa) + 10, 10))

# kontrollierte Schläge pro Jahr
lines(
      bar.kz,
      v.schlag,
      type="b",
      lty=3
      )

axis(4, at=seq(0, max(v.schlag) + 5, 5))
mtext("kontrollierte Schläge", side=4)

# kontrollierte Kästen pro Schlag und Jahr
lines(
      bar.kz,
      colSums(tab.kz) / v.schlag
      )

mtext(
      "o.. : kontrollierte Schläge \n
	_ : kontrollierte Kästen / kontrollierte Schläge",
      side=3,
      line=-5
      )

title(
      main="Entwicklung der Kastenzahl und des Kastenzustandes",
      sub="Kontrollierte Kästen nach Zustand, kontrollierte Schläge, Entwicklung Kastenzahl"
      )


# ANOVA

# Kastenzahlen zu Levels von koa und kz zugeordnet
df.kz$freq <- rep(0, length(df.kz$koa)) # leerer Vektor der richtigen Länge

# ermittelt die Anzahl der kontrollierten Kästen für die Kombination aus koa und kz
f.temp <- function(i) {
	x <- df.kz$koa[i]
	y <- df.kz$kz[i]
	temp <- subset(df.kz, koa == x & kz == y)
	temp <- length(temp$freq)
	temp
} 

# Schreibt die Werte in den Vektor
for(i in seq_along(df.kz$freq)) {
	df.kz$freq[i] <- f.temp(i)
}

par(mfrow=c(2,1))
plot(df.kz$freq ~ df.kz$koa * df.kz$kz)

# Modell
Model2 <- aov(freq ~ koa * kz, data=df.kz)
summary(Model2)

par(mfrow=c(2,2))
plot(Model2)

summary.lm(Model2)

temp1 <- rep(
	     seq(10,30,1), 
	     length(levels(df.kz$kz))
	     )
temp1 <- factor(temp1)
f.temp <- function(i) {
	temp <- levels(df.kz$kz)[i]
	temp <- rep(temp, length(seq(10,30,1)))
	temp
}
temp2 <- c(
	    f.temp(1), 
	    f.temp(2), 
	    f.temp(3),
	    f.temp(4)
	    )
pre.Model2 <- data.frame(koa=temp1, kz=temp2)
pre.Model2
pre.Model2$freq <- predict(Model2, pre.Model2)
