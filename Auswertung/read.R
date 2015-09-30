# Zuerst müssen wir die Gesamttabelle einlesen und die Datentypen gemäß werte.csv korrigieren.
df <- read.csv("gesamttabelle.csv", header=T)

# korrigiere Datentypen
df$gpsr  <- as.double(df$gpsr)
df$gpsh  <- as.double(df$koa)
df$koa  <- as.factor(df$koa)
df$kod  <- as.factor(df$kod)
df$wt  <- as.numeric(df$wt)
df$vn  <- as.factor(df$vn)
df$aves  <- as.factor(df$aves)
df$mamm  <- as.factor(df$mamm)
df$invert  <- as.factor(df$invert)
df$flkot  <- as.factor(df$flkot)
df$qtyp  <- as.factor(df$qtyp)

# Subsets für 2010 und 2011 bis 2015
invisible(
	  lapply(
		 split(
		       df, 
		       df$koa
		       ), 
		 function(x) {
			 assign(
				paste0(
				       "df", 
				       x$koa[1]
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

