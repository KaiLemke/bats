# Lese kompletten Datensatz ein
setwd("./..")
gesamttabelle  <- read.csv("gesamttabelle.csv", header=T)
head(gesamttabelle)

# korrigiere Datentypen
gesamttabelle$gpsr  <- as.double(gesamttabelle$gpsr)
gesamttabelle$gpsh  <- as.double(gesamttabelle$koa)
gesamttabelle$koa  <- as.factor(gesamttabelle$koa)
gesamttabelle$kod  <- as.factor(gesamttabelle$kod)
gesamttabelle$wt  <- as.numeric(gesamttabelle$wt)
gesamttabelle$vn  <- as.factor(gesamttabelle$vn)
gesamttabelle$aves  <- as.factor(gesamttabelle$aves)
gesamttabelle$mamm  <- as.factor(gesamttabelle$mamm)
gesamttabelle$invert  <- as.factor(gesamttabelle$invert)
gesamttabelle$flkot  <- as.factor(gesamttabelle$flkot)
gesamttabelle$qtyp  <- as.factor(gesamttabelle$qtyp)
summary(gesamttabelle)

# Subsets für 2010 und 2011 bis 2015
inittabelle  <- subset(gesamttabelle, koa == 10)
summary(inittabelle)
monitoringtabelle  <- subset(gesamttabelle, koa != 10)
summary(monitoringtabelle)
