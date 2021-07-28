# Allowable Increase Uncertainty

library(readxl)
library(optimLanduse)

# Einlesen des Beispieldatensatzes
dat <- read_xlsx(path = "inst/extdata/simDat-3-3-3.xlsx", col_names = T)

uValue <- 1
optimisticRule <- "expectation"
fixDistance <- NULL

digitsPrecision <- 4
lowerBound <- 0
upperBound <- 1

# Erzeugen eines leeren data.frames
AllowableIncreaseUncertainty <- data.frame(Iteration = "",
                                     Allowable_Increase = "",
                                     Allowable_Increase = "", stringsAsFactors = FALSE)
AllowableIncreaseUncertainty <- AllowableIncreaseUncertainty[-1, ]
names(AllowableIncreaseUncertainty)[-1] <- c("Allowable Increase [%]", "Allowable Increase [absolute]")

# Lösen des Optimierungsproblems als Vergleich für die Bedingung der while-Schleife
init <- initScenario(dat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
result <- solveScenario(x = init, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)


for (i in unique(dat$landUse)) {
  for (j in unique(dat$indicator)) {

    z <- 0

    tempdat <- dat
    tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
    tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

    # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
    while(apply(round(tempresult$landUse, digits = 3), 1, FUN = function(x){all(x == round(result$landUse, digits = 3))}) & z <= 50){

      z <- z + 10

      tempdat <- dat
      tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
      tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
      tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

    }
    if (z >= 50) {
      AllowableIncreaseUncertainty[nrow(AllowableIncreaseUncertainty) + 1, ] <- c(paste0(i," | ", j), 5000, round((dat$indicatorUncertainty[dat$landUse == i & dat$indicator == j]*50), digits = 2))
      next # überspringt die aktuelle Iteration
    } else {

      z <- z - 10

      tempdat <- dat
      tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
      tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      while(apply(round(tempresult$landUse, digits = 3), 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})){

        z <- z + 1

        tempdat <- dat
        tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
        tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
        tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      }
      z <- z - 1

      tempdat <- dat
      tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
      tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      while(apply(round(tempresult$landUse, digits = 3), 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})){

        z <- z + 0.1

        tempdat <- dat
        tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
        tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
        tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      }
      z <- z - 0.1

      tempdat <- dat
      tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
      tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      while(apply(round(tempresult$landUse, digits = 3), 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})){

        z <- z + 0.01

        tempdat <- dat
        tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
        tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
        tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      }
      z <- z - 0.01

      tempdat <- dat
      tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
      tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      while(apply(round(tempresult$landUse, digits = 3), 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})){

        z <- z + 0.001

        tempdat <- dat
        tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorUncertainty[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
        tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
        tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

      }
      AllowableIncreaseUncertainty[nrow(AllowableIncreaseUncertainty) + 1, ] <- c(paste0(i," | ", j), (z - 0.001)*100, round((dat$indicatorUncertainty[dat$landUse == i & dat$indicator == j]*(z-0.001)), digits = 2))}
  }}

if (any(AllowableIncreaseUncertainty$`Allowable Increase [%]` == 5000)) {
  print(paste0("Warning: Maximum increase of 5000 % is reached ", dim(AllowableIncreaseUncertainty[AllowableIncreaseUncertainty$`Allowable Increase [%]` == 5000,])[1], " times."))
}

