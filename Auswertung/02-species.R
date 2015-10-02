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

par(mfrow=c(7,1))
for(i in 10:15){
	with(
	     subset(df.ind.art, x.koa == i),
	     plot(Individuen ~ x.flart, 
		  main=paste0("20", i),
		  ylim=c(0,140)
		  )
	     )
}
with(df.ind.art,
     plot(Individuen ~ x.flart,
	  main="gesamt",
	  ylim=c(0,140)
	  )
     )
