# Sensitivitätsanalyse

# Erhöhen der Inputdaten um 5 %

library(readxl)
library(optimLanduse)


#dat <- read_xlsx(path = "C:/Users/felap/ownCloud/optimLanduse/Anwendungsbeispiele/1 simulateDataSource/simDat-3-3-3.xlsx", col_names = T)
dat <- read_xlsx(path = "inst/simDat-3-3-3.xlsx", col_names = T)
x <- 0.05

uValue <- 2
optimisticRule <- "expectation"
fixDistance <- NULL

digitsPrecision <- 4
lowerBound <- 0
upperBound <- 1


# Erzeugen eines leeren data.frames, in dem die Ergebnisse der Schleifendurchgänge abgespeichert werden
increase <- data.frame(iteration = "",
                        matrix(ncol = length(unique(dat$landUse))), stringsAsFactors = FALSE)
names(increase)[-1] <- unique(dat$landUse)
increase <- increase[-1, ]


# Erhöhen der einzelnen Inputwerte
for (i in unique(dat$indicator)) {
  for (j in unique(dat$landUse)) {

    tempdat <- dat

    tempdat$indicatorValue[tempdat$indicator == i & tempdat$landUse == j] <- tempdat$indicatorValue[tempdat$indicator == i & tempdat$landUse == j] * (1 + x)

    tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)

    tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

    increase[nrow(increase) + 1, ] <- c(paste0(i, j), tempresult$landUse)
  }
}



sensitivity <- list(increase = increase)


