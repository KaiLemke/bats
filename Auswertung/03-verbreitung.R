# Quartiere pro Schlag
tab.q.schlag <- with(
		   subset(
			  df,
			  qtyp != 0
			  ),
		   table(schlag, koa))
m.q.schlag <- as.data.frame.matrix(tab.q.schlag)
m.q.schlag <- rbind(m.q.schlag, Summe=colSums(m.q.schlag))
m.q.schlag <- cbind(m.q.schlag, gesamt=rowSums(m.q.schlag))

Table3 <- pander(
		 m.q.schlag,
		 caption="Quartiere pro Schlag",
		 digits=0
		 )
Table3

# Individuen pro Schlag
df.art.schlag <- ddply(df, .(koa, schlag), summarise, Individuen = sum(flges))

par(mfrow=c(7,1))
for(i in 10:15){
	with(
	     subset(df.art.schlag, koa == i),
	     plot(Individuen ~ schlag, 
		  main=paste0("20", i),
		  ylim=c(0,100)
		  )
	     )
}
with(df.art.schlag,
     plot(Individuen ~ schlag,
	  main="gesamt",
	  ylim=c(0,100)
	  )
     )

# Kästen pro Schlag
v.kasten.schlag <- rep(0, length(df.art.schlag$Individuen))
for(
    i in seq_along(v.kasten.schlag)
    ){
	v.kasten.schlag[i] <- m.schlag[
				       paste0(df.art.schlag$schlag[i]), 
				       paste0(df.art.schlag$koa[i])
				       ]
}
df.kasten.art.schlag <- df.art.schlag
df.kasten.art.schlag$Kastenzahl <- v.kasten.schlag
df.kasten.art.schlag$IndProKasten <- with(df.kasten.art.schlag,
					  Individuen / Kastenzahl
					  )

with(df.kasten.art.schlag,
	     plot(Kastenzahl ~ schlag,
		  main="gesamt"
		  )
     )

# Indivdiuen pro Kasten und Schlag
with(df.kasten.art.schlag,
     plot(IndProKasten ~ schlag,
	  main="gesamt"
	  )
     )

# Individuen pro Kasten
with(df.kasten.art.schlag,
     {
	     plot(Individuen ~ Kastenzahl,
		  main="gesamt"
		  )
	     abline(lm(Individuen ~ Kastenzahl))
     }
)

# ANOVA: Individuen ~ Kastenzahl + schlag
Model1 <- aov(Individuen ~ Kastenzahl + schlag, data=df.kasten.art.schlag)

Table4 <- pander(
		 Model1,
		 caption="ANOVA: Individuen ~ Kastenzahl + Schlag"
		 )
Table4

par(mfrow=c(2,2))
plot(Model1)
