optimLanduse
-----------

*optimLanduse* ist ein R Paket für die multikriterielle robuste Landschaftsoptimierung. Ziel des Pakets ist es, die von der Arbeitsgruppe Knoke entwickelte bzw. für die Landschaftsoptimierung erstmalig vewendete multikriterielle robuste Landschaftsoptimierung komfortabel und skalierbar anwenden zu können. Durch das Eibetten des Optimierungsverfahrens in die R Umgebung werden insbesondere Wiederholungsanwendungen (bspw. Sensitivitätsanalysen oder Distanzanalysen) vereinfach und schnell möglich. Der modulare Aufbau des Pakets soll eine Basis schaffen zu der zukünftige Erweiterungen schnell und einfach hinzugefügt werden können. Damit soll die Zusammenarbeit zwischen interessierten Arbeitsgruppen der robusten Landschaftsoptimierung vereinfacht werden.


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


<h3>
<a name="4. Aufbau des Modells">4. Aufbau des Modells</a>
</h3>

<h3>
<a name="5. Hinweise zur Anwendung">5. Hinweise zur Anwendung</a>
</h3>
Um die aktuellste stabile Version zu installieren, führen Sie den folgenden Code aus.  
<pre>
<code class="language-perl">
library(devtools)  
library(lpSolveAPI)  
library(dplyr)  
library(tidyr)  
install_git("https://gitlab.gwdg.de/forest_economics_goettingen/optimlanduse/optimLanduse.git")  
</code>
</pre>
<h3>
<a name="6. Beispielhafte Anwendung">6. Beispielhafte Anwendung</a>
</h3>

