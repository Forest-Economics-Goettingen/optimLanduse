# Allowable Increase

library(readxl)
library(optimLanduse)


dat <- read_xlsx(path = "inst/extdata/simDat-3-3-3.xlsx", col_names = T)

uValue <- 1
optimisticRule <- "expectation"
fixDistance <- NULL

digitsPrecision <- 4
lowerBound <- 0
upperBound <- 1

increase <- data.frame(iteration = "",
                       Allowable_Increase = "", stringsAsFactors = FALSE)
increase <- increase[-1, ]

init <- initScenario(dat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
result <- solveScenario(x = init, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)


for (i in unique(dat$landUse)) {
  for (j in unique(dat$indicator)) {

    z <- 0

    tempdat <- dat
    tempdat$indicatorValue[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorValue[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
    tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
    tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)


    while(apply(round(tempresult$landUse, digits = 3), 1, FUN = function(x){all(x == round(result$landUse, digits = 3))}) == TRUE){

      z <- z + 0.01

      tempdat <- dat
      tempdat$indicatorValue[tempdat$landUse == i & tempdat$indicator == j] <- tempdat$indicatorValue[tempdat$landUse == i & tempdat$indicator == j] * (1 + z)
      tempinit <- initScenario(tempdat, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
      tempresult <- solveScenario(x = tempinit, digitsPrecision = digitsPrecision, lowerBound = lowerBound, upperBound = upperBound)

    }

    increase[nrow(increase) + 1, ] <- c(paste0(i, j), z)
  }
}
