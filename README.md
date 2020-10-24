optimLanduse
-----------

*optimLanduse* ist ein R Paket für die multikriterielle robuste Landschaftsoptimierung. Ziel des Pakets ist es, die von der Arbeitsgruppe Knoke entwickelte bzw. für die Landschaftsoptimierung erstmalig vewendete multikriterielle robuste Landschaftsoptimierung komfortabel und skalierbar anwenden zu können. Durch das Eibetten des Optimierungsverfahrens in die R Umgebung werden insbesondere Wiederholungsanwendungen (bspw. Sensitivitätsanalysen oder Distanzanalysen) vereinfach und schnell möglich. Der modulare Aufbau des Pakets soll eine Basis schaffen zu der zukünftige Erweiterungen schnell und einfach hinzugefügt werden können. Damit soll die Zusammenarbeit zwischen interessierten Arbeitsgruppen der robusten Landschaftsoptimierung vereinfacht werden. Für einfache Anwendungen existiert auch eine graphische Oberfläche für dieses Paket (LINK zu Volkers Shiny)


<h3>
<a name="menu">Inhaltsverzeichnis</a>
</h3>
<ul>
<li>
<a href="#1. Einleitung">1. Einleitung</a>
</li>
<li>
<a href="#2. Grundlegende Theorie">2. Grundlegende Theorie</a>
</li>
<li>
<a href="#3. Input und Output">3. Input und Output</a>
</li>
<li>
<a href="#4. Aufbau des Modells">4. Aufbau des Modells</a>
</li>
<li>
<a href="#5. Hinweise zur Anwendung">5. Hinweise zur Anwendung</a>
</li>
<li>
<a href="#6. Beispielhafte Anwendung">6. Beispielhafte Anwendung</a>
</li>
<li>
<a href="#7. Literatur">7. Literatur</a>
</li>
<li>
<a href="#7. Dateien">8. Dateien</a>
</li>
</ul>


<h3>
<a name="1. Einleitung">1. Einleitung</a>
</h3>
Die robuste multikriterielle Optimierung, in der Form wie Sie bspw. in LIT beschrieben ist 


<h3>
<a name="2. Grundlegende Theorie">2. Grundlegende Theorie</a>
</h3>



<h3>
<a name="3. Input und Output">3. Input und Output</a>
</h3>

#### Input (mehr Details siehe Pakethilfe)
- Parameterdatei mit Erwartungen und Unsicherheiten. Formatiert wie in der Bespieldatei **dataset.xlsx**.
- Unsicherheit `u`.
- Form des optimistischen Szenarios. Erwartung oder Erwartung + Unsicherheit?

#### Output (mehr Details siehe Pakethilfe)
- Landnutzungsanteile
- Szenarientabelle mit Abständen der Szenarien
- Mindestabstand `&beta;`

<h3>
<a name="4. Aufbau des Modells">4. Aufbau des Modells</a>
</h3>

<h3>
<a name="5. Hinweise zur Anwendung">5. Hinweise zur Anwendung</a>
</h3>
Um die aktuellste stabile Version zu installieren, führen Sie den folgenden Code aus.

``` r

## Benötigte Pakete

packages <- c("devtools", "lpSolveAPI",
             "dplyr", "tidyr", "janitor",
             "remotes")

## Herunterladen und installieren oder aktivieren

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

install_gitlab("forest_economics_goettingen/optimlanduse", host = "gitlab.gwdg.de")

```

<h3>
<a name="6. Beispielhafte Anwendung">6. Beispielhafte Anwendung</a>
</h3>

Einfache Anwendung
``` r
# Pakete laden
library(optimLanduse) 
library(readxl)
library(lpSolveAPI)  
library(dplyr)  
library(tidyr) 

# Daten einlesen
dat <- read_xlsx("database (shrinked).xlsx", col_names = FALSE)
dat <- dataPreparation(dat = dat, uncertainty = "SE")

# Optimierung initialisieren
init <- initScenario(dat, uValue = 2, optimisticRule = "expectation")

# Optimierung durchführen
result <- solveScenario(x = init)

# Resultate ansehen
result$landUse # Landnutzungsanteile
result$scenarioTable # Szenariotabelle
result$scenarioSettings # Einstellungen anzeigen
result$status # Erfolgreich optimiert oder abgebrochen?
result$beta # Beta
result$landUse # Landnutzungsanteile
```

Batch Anwendung für mehrere Unsicherheiten u
``` r
# Pakete laden
library(optimLanduse)
library(readxl)

# Daten einlesen
dat <- read_xlsx("database (shrinked).xlsx", col_names = FALSE)
dat <- dataPreparation(dat = dat, uncertainty = "SE")

# Sequenz definieren
u <- seq(1, 5, 1)

# Batch vorbereiten
loopDf <- data.frame(u = u, matrix(NA, nrow = length(u), ncol = 1 + length(unique(dat$landUse))))
names(loopDf) <- c("u", "beta", unique(dat$landUse))

# Optimierungen initialisieren und durchführen
# Beispielhaft werden nur die Landnutzungsanteile gespeichert.

# Alternative 1: Schleife, einfach zu programmieren

loopDf <- data.frame(u = u, matrix(NA, nrow = length(u), ncol = 1 + length(unique(dat$landUse))))
names(loopDf) <- c("u", "beta", unique(dat$landUse))

for(i in u) {
  init <- initScenario(dat, uValue = i, optimisticRule = "expectation")
  result <- solveScenario(x = init)
  loopDf[loopDf$u == i,] <- c(i, result$beta, as.matrix(result$landUse))
}

# Alternative 2: apply, schneller
applyDf <- data.frame(u = u)

applyFun <- function(x) {
  init <- initScenario(dat, uValue = x, optimisticRule = "expectation")
  result <- solveScenario(x = init)
  return(c(result$beta, as.matrix(result$landUse)))
}

applyDf <- cbind(applyDf,
                 t(apply(applyDf, 1, applyFun)))
```

Batch Anwendung - parallel

``` r
# Pakete laden
library(optimLanduse) 
library(readxl)
library(doParallel)

# Daten einlesen
dat <- read_xlsx("database (shrinked).xlsx", col_names = FALSE)
dat <- dataPreparation(dat = dat, uncertainty = "SE")

# Kerne initialisieren, bspw. 8 Kerne
registerDoParallel(8)

# Sequenz definieren
u <- seq(1, 5, 1)

# Batch initialisieren und durchführen
loopDf1 <- foreach(i = u, .combine = rbind) %dopar% {
  init <- initScenario(dat, uValue = i, optimisticRule = "expectation")
  result <- solveScenario(x = init)
  c(i, result$beta, as.matrix(result$landUse))
}
# Falls die Kerne wieder freigegeben werden sollen
stopImplicitCluster()
```
