source("input.R")

# wähle nur Datensätze mit flart != 0
gesamttabelle  <- subset(gesamttabelle, flart != 0)
inittabelle  <- subset(inittabelle, flart != 0)
monitoringtabelle  <- subset(monitoringtabelle, flart != 0)

# counts
print("gezählte Individuen pro Art")
print("2010 bis 2015"); summary(gesamttabelle$flart)
print("2010"); summary(inittabelle$flart)
print("2011 bis 2015"); summary(monitoringtabelle$flart)

# Prozent
print("Prozent der gesamt gezählten Individuen")
print("2010 bis 2015"); summary(gesamttabelle$flart) / sum( summary(gesamttabelle$flart) ) * 100
print("2010"); summary(inittabelle$flart) / sum( summary(inittabelle$flart) ) * 100
print("2011 bis 2015"); summary(monitoringtabelle$flart) / sum( summary(monitoringtabelle$flart) ) * 100
