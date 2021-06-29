# Sensitivitätsanalyse

# Verringern der Inputdaten um 5 %

library(readxl)
library(optimLanduse)


dat <- read_xlsx(path = "inst/extdata/simDat-3-3-3.xlsx", col_names = T)

x <- 0.6

uValue <- 1
optimisticRule <- "expectation"
fixDistance <- NULL

digitsPrecision <- 4
lowerBound <- 0
upperBound <- 1


# Erzeugen eines leeren data.frames, in dem die Ergebnisse der Schleifendurchgänge abgespeichert werden
decrease <- data.frame(iteration = "",
                       matrix(ncol = 2 * length(unique(dat$landUse))), stringsAsFactors = FALSE)
names(decrease)[-1] <- paste0(unique(dat$landUse), "Value")
names(decrease)[-1:-(1+length(unique(dat$landUse)))] <- paste0(unique(dat$landUse), "Uncertainty")
decrease <- decrease[-1, ]


# Erhöhen der einzelnen Inputwerte
for (i in unique(dat$landUse)) {
  for (j in unique(dat$indicator)) {

    tempdat <- dat
    tempdat$indicatorValue[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorValue[tempdat$landUse == i & tempdat$indicator == j] * (1 - x)
    tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
    tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

    tempdat2 <- dat
    tempdat2$indicatorUncertainty[tempdat2$landUse == i & tempdat2$indicator == j] <- tempdat2$indicatorUncertainty[tempdat2$landUse == i & tempdat2$indicator == j] * (1 - x)
    tempinit2 <- initScenario(tempdat2, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
    tempresult2 <- solveScenario(x = tempinit2, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

    decrease[nrow(decrease) + 1, ] <- c(paste0(i, j), tempresult$landUse, tempresult2$landUse)
  }
}

# Runden der Ergebnisse
decrease[ , -1] <- round(decrease[ , -1], digits = 3)


# Ausgangsergebnis generieren
init <- initScenario(dat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
result <- solveScenario(x = init, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

# Überprüfen ob das Ergebnis der Interation dem Ausgangsergebnis entspricht
decrease$sameResultValue <- apply(decrease[ ,endsWith(names(decrease), "Value")], 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})
decrease$sameResultUncertainty <- apply(decrease[ ,endsWith(names(decrease), "Uncertainty")], 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})


# Anordnen der Spalten
decrease <- cbind(decrease$iteration, decrease[ ,endsWith(names(decrease), "Value")], decrease[ ,endsWith(names(decrease), "Uncertainty")])


sensitivity <- list(decrease = decrease)
