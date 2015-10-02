# Einlesen der Gesamttabelle
df <- read.csv("../gesamttabelle.csv", header=T)

# Korrigieren der Datenstruktur gemäß werte.csv

str(df)

# nicht verwertbare Variablen entfernen
with(
     df,
     rm(kid),
     rm(knr),
     rm(gpsr),
     rm(gpsh),
     rm(sonstiges)
     )
str(df)

# die unabhängigen Variablen
x.schlag <- df$schlag
x.ktyp <- df$ktyp
x.koa <- df$koa
x.kod <- df$kod
x.wt <- df$wt
x.wb <- factor(df$wb, levels=c("r", "w", "b", "s"))
str(x.wb)
head(x.wb); head(df$wb)
x.kz <- factor(df$kz,levels=c("d", "sb", "lb", "i"))
str(x.kz)
head(x.kz); head(df$kz)
x.vn <- factor(df$vn)
x.aves <- factor(df$aves)
x.mamm <- factor(df$mamm)
x.invert <- factor(df$invert)
x.flkot <- factor(df$flkot)
x.qtyp <- factor(df$qtyp)
x.flart <- df$flart

# die abhängigen Variablen
y.kz <- as.integer(x.kz)
str(y.kz)
head(x.kz); head(y.kz)
y.vn <- df$vn
head(y.vn); head(x.vn)
y.flkot <- df$flkot
y.flges <- df$flges
y.flm <- df$flm
y.flw <- df$flw
y.flad <- df$flad
y.fljuv <- df$fljuv

# df mit neuen Variablen
df <- data.frame(
		 x.schlag,
		 x.ktyp,
		 x.koa,
		 x.kod,
		 x.wt,
		 x.wb,
		 x.kz,
		 x.vn,
		 x.aves,
		 x.mamm,
		 x.invert,
		 x.flkot,
		 x.qtyp,
		 x.flart,
		 y.kz,
		 y.vn,
		 y.flkot,
		 y.flges,
		 y.flm,
		 y.flw,
		 y.flad,
		 y.fljuv
		 )
str(df)

# Subsets für 2010 und 2011 bis 2015
invisible(
	  lapply(
		 split(
		       df, 
		       df$x.koa
		       ), 
		 function(x) {
			 assign(
				paste0(
				       "df", 
				       x$x.koa[1]
				       ), 
				x, 
				pos=.GlobalEnv
				) 
		 }
		 )
	  )

# alle Datensätze in einer Liste
l <- list(
	  df=df, 
	  df10=df10, 
	  df11=df11, 
	  df12=df12, 
	  df13=df13, 
	  df14=df14, 
	  df15=df15
	  )

ls()

# schreibe die Datensätze in Dateien
save(df, file="df")
save(df10, file="df10")
save(df11, file="df11")
save(df12, file="df12")
save(df13, file="df13")
save(df14, file="df14")
save(df15, file="df15")
save(l, file="l")
