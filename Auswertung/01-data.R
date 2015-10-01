source("Auswertung/read.R")

# Kontrollen ~ Jahr
tab.schlag <- with(df, table(schlag, koa))
m.schlag <- as.data.frame.matrix(tab.schlag)
m.schlag <- rbind(m.schlag, Summe=colSums(m.schlag))
m.schlag <- cbind(m.schlag, gesamt=rowSums(m.schlag))

Table1 <- pander(
		 m.schlag,
		 caption="Datensätze pro Schlag",

		 digits=0
		 )
Table1
