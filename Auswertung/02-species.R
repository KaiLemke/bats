levels(df$x.flart)

# Quartiere pro Art
tab.art <- with(df, table(x.flart, x.koa))
m.art <- as.data.frame.matrix(tab.art)
m.art <- m.art[-1,]
m.art <- rbind(m.art, Summe=colSums(m.art))
m.art <- cbind(m.art, gesamt=rowSums(m.art))

Table2 <- pander(
		 m.art,
		 caption="Quartiere pro Art",
		 digits=0,
		 split.table=Inf
		 )
Table2

# Individuen pro Art
df.ind.art <- ddply(df, .(x.koa, x.flart), summarise, Individuen = sum(y.flges))
df.ind.art <- subset(df.ind.art, x.flart != 0) 

Table3 <- pander(
		 df.ind.art,
		 caption="Summe der Individuen pro Art",
		 digits=0
		 )
Table3

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

# Summe der Individuen pro Jahr
df.ind.koa <- ddply(df, .(x.koa), summarise, ArtSumme = sum(y.flges))
df.ind.koa$counts.schlag <- v.schlag
df.ind.koa$ArtSummeProSchlag <- df.ind.koa$ArtSumme / v.schlag


# Plot

.parsave <- par(no.readonly=T)
par(xpd=T, 
    mar=.parsave$mar + c(0,0,0,10),
    mfrow=c(2,1)
    )

plot(
     log(Individuen) ~ x.koa,
     data = df.ind.art,
     ylim = c(0, log(max(df.ind.koa$ArtSumme) + 2)),
     type = "n",
     main = "Summe der Individuen pro Jahr und Art",
     xlab = "Kontrolljahr",
     ylab = "Logarithmus der Individuen"
     )

# Punkte für die Arten
for(i in seq_along(levels(df.ind.art$x.flart))) {
	points(
	       log(Individuen) ~ x.koa,
	       data = subset(
			     df.ind.art,
			     x.flart == levels(df.ind.art$x.flart)[i]
			     ),
	       pch = i
	       )
}

lines(log(ArtSumme) ~ x.koa, data=df.ind.koa, lty=3)

legend(
       15.3, 5,
       c(levels(df.ind.art$x.flart)[-1], "Summe"),
       pch=c(seq_along(levels(df.ind.art$x.flart)[-1]) + 1, NA), # die Null braucht keiner
       lty=c(rep(0, length(levels(df.ind.art$x.flart)[-1])), 3)
       )

plot(
     ArtSummeProSchlag ~ x.koa,
     data = df.ind.koa,
     type = "l",
     lty = 3,
     main = "Summe der Individuen pro Jahr und Schlag",
     xlab = "Kontrolljahr",
     ylab = "Summe der Individuen / Summe der Schläge"
     )

par(.parsave)
