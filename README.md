optimLanduse
-----------

<h3>
<a name="menu"></a>
</h3>
<ul>
<li>
<a href="#1. Einleitung">Introduction</a>
</li>
<li>
<a href="#3. Input und Output">Package structure</a>
</li>
<li>
<a href="#6. Beispielhafte Anwendung">Exemplary application</a>
</li>
<li>
<a href="#7. Literatur">Literature</a>
</li>
</ul>


<h3>
<a name="1. Einleitung">Introduction</a>
</h3>

The R package **optimLanduse** provides tools for easy and systematic applications of the robust multiobjective land-cover composition optimization approach of Knoke et al. (2016). It includes tools to determine the land-cover composition that best balances the multiple functions a landscape can provide, and tools for understanding and visualizing how these compromises are reasoned. The **README.md** below guides users through the application and highlights possible use-cases on the basis of a published data set.
Illustrating the consequences of alternative ecosystem functions on the theoretically optimal landscape composition provides easily interpretable information for landscape modeling and decision making.
The method is already established in land-use optimization and has been applied in a couple of studies. More details about the theory, the definition of the formal optimization problem and also significant examples are listed in the <a href="#7. Literatur">literature</a> section

The package opens the approach of Knoke et al. (2016) to the community of landscape and planners and provides opportunities for straightforward systematic or batch applications.
To further enhance this, we have designed a graphical shiny application for the package to get a quick idea of the functionalities of the package, see http://134.76.17.50/optimlanduse_shiny/.


<h3>
<a name="3. Input und Output">Package structure</a>
</h3>

This chapter provides brief overview over the package functions. Please consider their respective help pages for more information. The function lpSolveAPI comes from the **lpSolveAPI** package. https://cran.r-project.org/package=lpSolveAPI

<p align="center">
  <img width="781.6" height="452" src="./man/figures/flowchart.png">
</p>

#### Initialization and Input

The *initScenario()* function integrates the user settings into the data and returns an *optimLanduse*-object ready for solving. The following input data are required: 

- *Coefficients table*: The package is **only capable** of processing a long-oriented type of data structure. All indicators have to vertically listed with their average expectations and uncertainties for the different land-cover alternatives. The columns of the table **must contain** a predefined heading. You can take this exact format from the given example table **exampleGosling.xlsx** or follow the below excerpt of this table:

<p align="center">
  <img width="673.4" height="298.2" src="./man/figures/exampleGraphic.png">
</p>

|            See the help files of the **exampleData** and **initScenario** functions for more details. An empty template (for predefined headings) 
|            is additionally given in the package and can be called via exampleData("exampleEmpty.xlsx").
<br>

- *uValue*: The argument for the uncertainty level. A higher uValue reflects a higher risk aversion of the decision maker. See the help file of the **initScenario** function for more details.

- *optimisticRule*: Specifies whether the optimistic contributions of each indicator should be defined either directly by their average, or by their average plus their uncertainty (if more is better) or minus its uncertainty (if less is better). The former option is most frequently used in recent literature and therefore builds the default.

- *fixDistance*: TBD (Need to create a uniform understandable description for readme and helper functions package). Problem is, that the definition in the paper is quite linked with the equation in the paper.

#### Solver and list with results

*solveScenario()* requires the initialized *optimLanduse* object and only a few optional solver-specific arguments. As the solving process has no stochastic element, the calculation times depend almost entirely on the number of digits calculated. 

- *digitsPrecision*: Provides the only possibility for the user to influence the calculation time. As the solving process has no stochastic element, the calculation times depend almost entirely on the number of digits calculated.

- *lowerBound* & *upperBound*: Optional bounds for the land-use options. Choosing 0 and 1 (the defaults) as boundaries for all decision variables, means that no land-cover alternative is forced into the farm portfolio and that no land-cover alternative is assigned a maximum share.

The resulting *list with results* contains the information of the optimization model. It contains the:

- land use allocation in the optimum,
- the detailed table with all possible indicator combinations (the scenario table), and the
- maximum distance **&beta;**.


#### Post-processing

*calcPerfomance()*: Attaches the portfolio performances of all scenarios and creates a frame for the visualization. The performance is defined as the distance to the maximum achievable level for each indicator and uncertainty scenario.



<h3>
<a name="6. Beispielhafte Anwendung">Exemplary application</a>
</h3>

Install package
``` r
install.packages("optimLanduse")
```

Simple example
``` r
require(readxl)
library(optimLanduse)

path <- exampleData("exampleGosling.xlsx")
dat <- read_excel(path)

init <- initScenario(dat,
                     uValue = 2,
                     optimisticRule = "expectation",
                     fixDistance = 3)
result <- solveScenario(x = init)
performance <- calcPerformance(result)


result$landUse
result$scenarioTable
result$scenarioSettings
result$status
result$beta
```

Exemplary batch application for distinct uncertainty values u
``` r
require(readxl)
library(optimLanduse)

path <- exampleData("exampleGosling.xlsx")
dat <- read_excel(path)

# define sequence of uncertainties
u <- seq(1, 5, 1)

# prepare empty data frame for the results

## alternative 1: loop, simply implemented ##

loopDf <- data.frame(u = u, matrix(NA, nrow = length(u), ncol = 1 + length(unique(dat$landUse))))
names(loopDf) <- c("u", "beta", unique(dat$landUse))

for(i in u) {
  init <- initScenario(dat, uValue = i, optimisticRule = "expectation", fixDistance = 3)
  result <- solveScenario(x = init)
  loopDf[loopDf$u == i,] <- c(i, result$beta, as.matrix(result$landUse))
}

# alternative 2: apply, faster
applyDf <- data.frame(u = u)

applyFun <- function(x) {
  init <- initScenario(dat, uValue = x, optimisticRule = "expectation", fixDistance = 3)
  result <- solveScenario(x = init)
  return(c(result$beta, as.matrix(result$landUse)))
}

applyDf <- cbind(applyDf,
                 t(apply(applyDf, 1, applyFun)))
                 
names(applyDf) <- c("u", "beta", names(result$landUse))
```

Plot the land-use allocations with increasing uncertainty
``` r
# Typical result visualization
require(ggplot2)
require(dplyr)
require(tidyr)

applyDf %>% gather(key = "land-use option", value = "land-use share", -u, -beta) %>%
  ggplot(aes(y = `land-use share`, x = u, fill = `land-use option`)) +
  geom_area(alpha = .8, color = "white") + theme_minimal()
```


Batch example - parallel

``` r
library(optimLanduse)
require(readxl)
require(foreach)
require(doParallel)

path <- exampleData("exampleGosling.xlsx")
dat <- read_excel(path)

registerDoParallel(8)

u <- seq(1, 5, 1)


loopDf1 <- foreach(i = u, .combine = rbind, .packages = "optimLanduse") %dopar% {
  init <- initScenario(dat, uValue = i, optimisticRule = "expectation", fixDistance = 3)
  result <- solveScenario(x = init)
  c(i, result$beta, as.matrix(result$landUse))
}

stopImplicitCluster()
```

<h3>
<a name="7. Literatur">Literature</a>
</h3>

Gosling, E., Reith, E., Knoke T., Gerique, A., Paul, C. (2020): Exploring farmer perceptions of agroforestry via multi-objective optimisation: a test application in Eastern Panama. <em>Agroforestry Systems</em> **94(5)**. https://doi.org/10.1007/s10457-020-00519-0

Knoke, T., Paul, C., Hildebrandt, P. et al. (2016): Compositional diversity of rehabilitated tropical lands supports multiple ecosystem services and buffers uncertainties. <em>Nat Commun</EM> **7**, 11877. https://doi.org/10.1038/ncomms11877

Paul, C., Weber, M., Knoke, T. (2017): Agroforestry versus farm mosaic systems – Comparing land-use efficiency, economic returns and risks under climate change effects. <em>Sci. Total Environ.</em> **587-588**. https://doi.org/10.1016/j.scitotenv.2017.02.037.

Knoke, T., Paul, C., et al. (2020). Accounting for multiple ecosystem services in a simulation of land‐use decisions: Does it reduce tropical deforestation?. <em>Global change biology</em> **26(4)**. https://doi.org/10.1111/gcb.15003
