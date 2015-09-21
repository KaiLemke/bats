-   Datensätze: gesamt, init und monitoring
-   Welche Arten haben ihre Sommerquartiere im Untersuchungsgebiet?
    -   gezählte Individuen pro Art
    -   Prozente der gezählten Individuen pro Art

Datensätze: gesamt, init und monitoring
=======================================

Zuerst müssen wir die Gesamttabelle einlesen und die Datentypen gemäß werte.csv korrigieren.

``` r
gesamt <- read.csv("gesamttabelle.csv", header=T)

# korrigiere Datentypen
gesamt$gpsr  <- as.double(gesamt$gpsr)
gesamt$gpsh  <- as.double(gesamt$koa)
gesamt$koa  <- as.factor(gesamt$koa)
gesamt$kod  <- as.factor(gesamt$kod)
gesamt$wt  <- as.numeric(gesamt$wt)
gesamt$vn  <- as.factor(gesamt$vn)
gesamt$aves  <- as.factor(gesamt$aves)
gesamt$mamm  <- as.factor(gesamt$mamm)
gesamt$invert  <- as.factor(gesamt$invert)
gesamt$flkot  <- as.factor(gesamt$flkot)
gesamt$qtyp  <- as.factor(gesamt$qtyp)

# Subsets für 2010 und 2011 bis 2015
init  <- subset(gesamt, koa == 10)
monitoring  <- subset(gesamt, koa != 10)
```

Welche Arten haben ihre Sommerquartiere im Untersuchungsgebiet?
===============================================================

Hierfür brauchen wir Datensätze, die nur die Zeilen enthalten, in denen eine Art bestimmt wurde.

``` r
gesamt.flart <- subset(gesamt, flart != 0)
init.flart <- subset(init, flart != 0)
monitoring.flart <- subset(monitoring, flart != 0)
```

gezählte Individuen pro Art
---------------------------

``` r
print("2010 bis 2015"); summary(gesamt.flart$flart)
```

    ## [1] "2010 bis 2015"

    ##                     0         M.daubentonii              M.myotis 
    ##                     0                    16                     5 
    ## M.mystacinus/brandtii           M.nattereri           Nyc.noctula 
    ##                     1                    74                     1 
    ##                   Pip          Pip.nathusii          Pip.pygmaeus 
    ##                     1                     2                     2 
    ##          Plec.auritus 
    ##                    13

``` r
print("2010"); summary(init.flart$flart)
```

    ## [1] "2010"

    ##                     0         M.daubentonii              M.myotis 
    ##                     0                     1                     2 
    ## M.mystacinus/brandtii           M.nattereri           Nyc.noctula 
    ##                     1                    15                     1 
    ##                   Pip          Pip.nathusii          Pip.pygmaeus 
    ##                     0                     2                     0 
    ##          Plec.auritus 
    ##                     8

``` r
print("2011 bis 2015"); summary(monitoring.flart$flart)
```

    ## [1] "2011 bis 2015"

    ##                     0         M.daubentonii              M.myotis 
    ##                     0                    15                     3 
    ## M.mystacinus/brandtii           M.nattereri           Nyc.noctula 
    ##                     0                    59                     0 
    ##                   Pip          Pip.nathusii          Pip.pygmaeus 
    ##                     1                     0                     2 
    ##          Plec.auritus 
    ##                     5

Prozente der gezählten Individuen pro Art
-----------------------------------------

``` r
print("2010 bis 2015"); summary(gesamt.flart$flart) / sum( summary(gesamt.flart$flart) ) * 100
```

    ## [1] "2010 bis 2015"

    ##                     0         M.daubentonii              M.myotis 
    ##             0.0000000            13.9130435             4.3478261 
    ## M.mystacinus/brandtii           M.nattereri           Nyc.noctula 
    ##             0.8695652            64.3478261             0.8695652 
    ##                   Pip          Pip.nathusii          Pip.pygmaeus 
    ##             0.8695652             1.7391304             1.7391304 
    ##          Plec.auritus 
    ##            11.3043478

``` r
print("2010"); summary(init.flart$flart) / sum( summary(init.flart$flart) ) * 100
```

    ## [1] "2010"

    ##                     0         M.daubentonii              M.myotis 
    ##              0.000000              3.333333              6.666667 
    ## M.mystacinus/brandtii           M.nattereri           Nyc.noctula 
    ##              3.333333             50.000000              3.333333 
    ##                   Pip          Pip.nathusii          Pip.pygmaeus 
    ##              0.000000              6.666667              0.000000 
    ##          Plec.auritus 
    ##             26.666667

``` r
print("2011 bis 2015"); summary(monitoring.flart$flart) / sum( summary(monitoring.flart$flart) ) * 100
```

    ## [1] "2011 bis 2015"

    ##                     0         M.daubentonii              M.myotis 
    ##              0.000000             17.647059              3.529412 
    ## M.mystacinus/brandtii           M.nattereri           Nyc.noctula 
    ##              0.000000             69.411765              0.000000 
    ##                   Pip          Pip.nathusii          Pip.pygmaeus 
    ##              1.176471              0.000000              2.352941 
    ##          Plec.auritus 
    ##              5.882353

Die deutliche Zunahme der Wasserfledermuas von 3 auf 18 % dürfte eher Zufall sein, da 2010 nur ein Einzeltier gefunden wurde, ähnliches gilt wohl für Abendsegler, Rauhhaut-- und umgekehrt Mückenfledermaus. Anders sieht es wohl beim Braunen Langohr aus, das von 27 auf 6 % gefallen ist.
