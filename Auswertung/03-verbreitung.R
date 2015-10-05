# Quartiere pro Schlag
tab.q.schlag <- with(
		   subset(
			  df,
			  x.qtyp != 0
			  ),
		   table(x.schlag, x.koa))
m.q.schlag <- as.data.frame.matrix(tab.q.schlag)
m.q.schlag <- rbind(m.q.schlag, Summe=colSums(m.q.schlag))
m.q.schlag <- cbind(m.q.schlag, gesamt=rowSums(m.q.schlag))

Table4 <- pander(
		 m.q.schlag,
		 caption="Quartiere pro Schlag",
		 digits=0
		 )
Table4

# Individuen pro Schlag
df.art.schlag <- ddply(
		       df, 
		       .(x.koa, x.schlag), 
		       summarise, Individuen = sum(y.flges)
		       )

# Kästen pro Schlag
v.kasten.schlag <- rep(0, length(df.art.schlag$Individuen))
for(
    i in seq_along(v.kasten.schlag)
    ){
	v.kasten.schlag[i] <- m.schlag[
				       paste0(df.art.schlag$x.schlag[i]), 
				       paste0(df.art.schlag$x.koa[i])
				       ]
}
df.kasten.art.schlag <- df.art.schlag
df.kasten.art.schlag$Kastenzahl <- v.kasten.schlag
df.kasten.art.schlag$IndProKasten <- with(df.kasten.art.schlag,
					  Individuen / Kastenzahl
					  )


# Plot

par(xpd=T, 
    mar=.parsave$mar + c(0,0,0,10),
    mfrow=c(2,1)
    )

plot(
     log(Individuen) ~ x.koa,
     data = df.art.schlag,
     ylim = c(0, log(max(df.ind.koa$ArtSumme) + 2)),
     type = "n",
     main = "Summe der Individuen pro Jahr und Schlag",
     xlab = "Kontrolljahr",
     ylab = "Logarithmus der Individuen"
     )

# Punkte für die Schläge
for(i in seq_along(levels(df.art.schlag$x.schlag))) {
	points(
	       log(Individuen) ~ x.koa,
	       data = subset(
			     df.art.schlag,
			     x.schlag == levels(df.art.schlag$x.schlag)[i]
			     ),
	       col = i,
	       pch = i
	       )
}

lines(log(ArtSumme) ~ x.koa, data=df.ind.koa, lty=3)

legend(
       15.3, 5,
       c(levels(df.art.schlag$x.schlag), "Summe"),
       col=c(seq_along(levels(df.art.schlag$x.schlag)), NA),
       pch=c(seq_along(levels(df.art.schlag$x.schlag)), NA),
       lty=c(rep(0, length(levels(df.art.schlag$x.schlag))), 3)
       )

plot(
     Individuen ~ x.schlag,
     data=df.kasten.art.schlag,
     at=seq_along(levels(df.kasten.art.schlag$x.schlag)) - 0.2,
     col="white",
     main="Individuen und Kästen pro Schlag",
     xlab="Schlag",
     ylab=""
     )

plot(
     Kastenzahl ~ x.schlag,
     data=df.kasten.art.schlag,
     axes=0,
     xlab=NULL,
     ylab=NULL,
     col="lightgray",
     add=T
     )

legend(
       16.3, 80,
       c("Individuen", "Kästen"),
       c("white", "lightgray")
       )

par(.parsave)

par(mfrow=c(2,1))

plot(
     IndProKasten ~ x.schlag,
     data=df.kasten.art.schlag,
     main="Individuen pro Kasten und Schlag",
     xlab="Schlag",
     ylab="Individuen"
     )

plot(Individuen ~ Kastenzahl,
     data=df.kasten.art.schlag,
     main="Individuen in Abhängigkeit von der Kastendichte",
     xlab="Kastenzahl pro Schlag",
     ylab="Individuen"
     )
abline(lm(Individuen ~ Kastenzahl, data=df.kasten.art.schlag))

par(.parsave)





# ANOVA: Individuen ~ Kastenzahl + schlag
Model1 <- aov(Individuen ~ Kastenzahl + x.schlag, data=df.kasten.art.schlag)

Table5 <- pander(
		 Model1,
		 caption="ANOVA: Individuen ~ Kastenzahl + Schlag"
		 )
Table5

par(mfrow=c(2,2))
plot(Model1)
