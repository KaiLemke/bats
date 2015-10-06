source("00-libs.R")

# Lade die Datensätze
load("df"); str(df)
load("df10")
load("df11")
load("df12")
load("df13")
load("df14")
load("df15")
load("l")

# Überblick über die Datenstruktur
str(df)

# Kontrollen ~ Jahr
tab.schlag <- with(df, table(x.schlag, x.koa))
m.schlag <- as.data.frame.matrix(tab.schlag)
m.schlag <- rbind(m.schlag, Summe=colSums(m.schlag))
m.schlag <- cbind(m.schlag, gesamt=rowSums(m.schlag))

Table1 <- pander(
		 m.schlag,
		 caption="Datensätze pro Schlag",

		 digits=0
		 )
Table1
