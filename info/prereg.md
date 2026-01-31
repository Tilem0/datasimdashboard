# prereg

# Ungewöhnlich = Unvergesslich ?

## Der Einfluss der Informationsökologie und der Beziehungsorientierung auf die Source Memory von Datingprofilen

## \*\*1) Data Collection:\*\* Have any data been collected for this study already?

Es wurden noch keine Daten erhoben.

## \*\*2) Hypothesis:\*\* What's the main question being asked or hypothesis being tested in this study?

Forschungsfrage:  
Lässt sich der Cheater-Memory-Effekt (bessere Source-Memory-Leistung für Profile mit betrügerischem Merkmal) in Abhängigkeit der Informationsökologie (Häufigkeit von vertrauenswürdigen vs. betrügerischen Profilen) in einem Dating-Kontext umkehren? Beeinflusst die Beziehungsorientierung (Kurz- vs. Langzeitorientierung) diesen Effekt?

Hypothesen:  
\*\*H1 (Replikation des Cheater-Memory-Effekts):\*\* In der Bedingung "Realistische Ökologie" (vertrauenswürdige Profile häufig, betrügerische selten) ist die Source-Memory-Leistung für betrügerische Profile signifikant besser als für vertrauenswürdige Profile.

H2 (Einfluss der Informationsökologie / Reversal): In der Bedingung "Invertierte Ökologie" (betrügerische Profile häufig, vertrauenswürdige selten) kehrt sich der Effekt um: Die Source-Memory-Leistung für vertrauenswürdige Profile ist signifikant besser als für betrügerische Profile.  
\*(Statistisch erwarten wir hier eine disordinale Interaktion zwischen den Faktoren Informationsökologie und Valenz)\*

H3 (Interaktion durch Beziehungsorientierung): Die unter H1 und H2 beschriebenen Effekte werden durch die Beziehungsorientierung moderiert. Bei Personen mit stärkerer Langzeitorientierung (vs. Kurzzeitorientierung) ist die Sensitivität für die seltene Information (in der realistischen Bedingung: der Cheater) stärker ausgeprägt.  
\*(Konkret: Der Source-Memory-Vorteil für Cheater in der realistischen Bedingung korreliert positiv mit der Langzeitorientierung).\*

## \*\*3) Variables:\*\* Describe the key dependent variable(s) specifying how they will be measured.

Die zentrale abhängige Variable ist die Source Memory Leistung.

Die Messung erfolgt in einem zweistufigen Wiedererkennungstest (Recognition Test):

1. Item-Gedächtnis (Old/New): Vpn entscheiden für jedes Gesicht (alte und neue Gesichter), ob sie es zuvor gesehen haben ("Ja" / "Nein").
2. Quellengedächtnis (Source Judgment): Für als "alt" erkannte Gesichter müssen die Vpn die ursprüngliche Eigenschaft zuordnen. Um Rateeffekte zu minimieren, gibt es drei Antwortoptionen:  
   \* "Vertrauenswürdig" (Trustworthy)  
   \* "Betrügerisch" (Cheater)  
   \* "Weiß nicht / Neutral"

Die Operationalisierung erfolgt über das Conditional Source Identification Measure (CSIM). Dies ist der Anteil der korrekten Quellenzuordnungen (Correct Source) relativ zur Anzahl der korrekt wiedererkannten Gesichter (Hits).  
\*(= Anzahl korrekter Quellenzuordnungen / Anzahl korrekter Wiedererkennungen).\*

Zusätzlich erheben wir als kontinuierliche Variable die Beziehungsorientierung (BZO) (Langzeit- vs. Kurzzeitorientierung) mittels Fragebogen (Schwarz & Hassebrauck, 2007). Der Fragebogen umfasst 20 Items, die sich auf zwei Subskalen mit 11 Items zur Erfassung der Langzeitorientierung (LZO) und 9 Items zur Erfassung der Kurzzeitorientierung (KZO) aufteilen. Teilnehmende bewerten die Items auf einer 7-stufigen Likert-Skala mit den jeweiligen Endpolen 1 (“stimme überhaupt nicht zu”) und 7 (“stimme völlig zu”). Berechnet wird ein Mittelwert für die LZO und KZO der individuellen Versuchsperson.

## \*\*Conditions:\*\* How many and which conditions will participants be assigned to?

Design:  
Das Studiendesign ist ein 2 (Informationsökologie) x 2 (Valenz) Mixed Design mit einer zusätzlichen kontinuierlichen Variable.

Faktor 1: Informationsökologie (Between-Subjects) Die Versuchspersonen werden randomisiert einer von zwei Bedingungen zugewiesen, die die Häufigkeit von Eigenschaften in der Umwelt manipulieren:

```
\*\*Realistische Ökologie (Natural Rarity)\*\*: Vertrauenswürdige Profile sind die Norm. 70% der gezeigten Profile sind vertrauenswürdig, 30% sind betrügerisch. \*\*Invertierte Ökologie (Negative Rarity)\*\*: Betrügerische Profile sind die Norm. 30% der Profile sind vertrauenswürdig, 70% sind betrügerisch. 
```

Faktor 2: Valenz (Within-Subjects) Die Manipulation der Valenz (vertrauenswürdig vs. betrügerisch) erfolgt über verhaltensbeschreibende Vignetten, die zusammen mit dem Profilbild präsentiert werden.

```
\*\*Vertrauenswürdige Profile:\*\* Enthalten Aussagen, die Zuverlässigkeit, Ehrlichkeit und Loyalität signalisieren (z.B. "In einer Beziehung ist mir Verlässlichkeit und Beständigkeit sehr wichtig.”). \*\*Betrügerische Profile:\*\* Enthalten Aussagen, die auf Untreue, Unzuverlässigkeit oder opportunistisches Verhalten hindeuten (z.B. “Ich finde nicht, dass alles geteilt werden muss \\- Freiraum gehört für mich zur Beziehung.”). 
```

Randomisierung: Die Zuordnung der Vignetten zu den Profilbildern erfolgt randomisiert, um sicherzustellen, dass die Valenz-Zuschreibung unabhängig von physischen Merkmalen der gezeigten Personen ist. Die Profilbilder werden codiert, um im Nachgang auszuschließen, dass es überzufällig häufige Paarungen zwischen Gesichtern und Vignetten gibt.

Zusätzliche gemessene Variable: Die Beziehungspräferenz (Langzeit- vs. Kurzzeitorientierung) wird mittels Fragebogen (z.B. Schwarz & Hassebrauck, 2007) erhoben und als kontinuierliche Variable (Kovariate) in das Design aufgenommen.

## Specify exactly which analyses you will conduct to examine the main hypothesis

### Hauptanalyse (Linear Mixed Model, LMM)

 Wir berechnen ein lineares gemischtes Modell (LMM) mit der \*Source-Memory-Leistung (CSIM)\*\* als abhängige Variable.  
\* Informationsökologie (realistisch vs. invertiert) und Valenz (vertrauenswürdig vs. betrügerisch) werden als Fixed Effects modelliert.  
\* Da Langzeit- und Kurzzeitorientierung unabhängige Dimensionen darstellen, werden Langzeitorientierung (LT) und Kurzzeitorientierung (ST) als zwei separate, kontinuierliche Prädiktoren (zentriert) in das Modell aufgenommen.  
\* Das Modell umfasst alle Haupteffekte sowie alle resultierenden Zweifach- und Dreifach-Interaktionen (insbesondere Ökologie × Valenz × LT sowie Ökologie × Valenz × ST).  
\* Die Versuchspersonen werden als Random Effect berücksichtigt. Um der Messwiederholung Rechnung zu tragen, spezifizieren wir ein Modell mit Random Intercepts und Random Slopes für den Faktor Valenz pro Versuchsperson, sofern das Modell konvergiert.

### H1 & H2 (Informationsökologie × Valenz)

 Die Zweifach-Interaktion zwischen Informationsökologie und Valenz wird im LMM getestet.  
 Zur Auflösung dieser Interaktion werden getrennte Mittelwertsvergleiche durchgeführt, um die Effekte der Valenz innerhalb der Bedingungen „Realistische Ökologie“ und „Invertierte Ökologie“ zu prüfen.  
\* Für die Bedingung „realistische Ökologie“ wird ein gerichteter Kontrast berechnet; erwartet wird Source Memory (betrügerisch) &gt; Source Memory (vertrauenswürdig).  
\* Für die Bedingung „invertierte Ökologie“ wird ein gerichteter Kontrast berechnet; erwartet wird eine Abflachung oder Umkehrung des Effekts (Source Memory vertrauenswürdig &gt; betrügerisch).

### H3 (Einfluss der Beziehungspräferenz)

 Zur Prüfung von H3 werden die Dreifachinteraktionen \*Ökologie × Valenz × LT\*\* sowie Ökologie × Valenz × ST betrachtet.  
\* Zur Interpretation signifikanter Interaktionen werden Simple Slopes berechnet. Hierbei wird die Steigung des Valenzeffekts (Cheater-Vorteil) auf verschiedenen Stufen der Beziehungspräferenz (z. B. +/- 1 SD) betrachtet.  
\* Es wird erwartet, dass eine hohe Ausprägung der Langzeitorientierung (LT) den Rarity-Effekt verstärkt steilere Slopes für die jeweils seltene Information zeigt), während die Kurzzeitorientierung (ST) einen geringeren oder keinen moderierenden Einfluss auf die Source Memory für vertrauensrelevante Merkmale hat.

### Inferenzen

 Alpha-Niveau =.05.  
 für die geplanten gerichteten Kontraste und post-hoc-Vergleiche innerhalb des linearen gemischten Modells wird eine Alphafehlerkorrektur mittels Bonferroni-Korrektur angewendet

## Describe exactly how outliers will be defined and handled, and your precise rule(s) for excluding observations.

Wir schließen Versuchspersonen von der Analyse aus, wenn mindestens eines der folgenden Kriterien erfüllt ist:

1. Unvollständige Teilnahme: Der Datensatz ist unvollständig (Abbruch vor Ende der Studie).
2. Zielgruppen-Verfehlung (Pre-Screening Fail): Die Person erfüllt nicht die Einschlusskriterien (kein Single, nicht im Altersbereich 18-35 Jahre), falls diese nicht bereits technisch durch einen Fragebogen-Filter ausgeschlossen wurde.
3. Careless Responding / auffällige Antwortmuster (&gt; 90% identische Antworten im Recognition Test oder Beziehungspräferenzfragebogen).

## How many observations will be collected or what will determine sample size?

Power-Analyse: Die Bestimmung der Stichprobengröße basiert auf einer A-priori-Poweranalyse für eine Mixed ANOVA (Within-Between Interaction), durchgeführt mit G\\\*Power (v3.1.9.7). Da die Stabilität der Source-Memory-Leistung über verschiedene Valenzstufen hinweg variieren kann, wurde für die Korrelation zwischen den Messwiederholungen eine konservative Schätzung von r=.30 angenommen.

Parameter:

\* Effektstärke: f=0.18 (kleiner bis mittlerer Interaktionseffekt)  
\* Alpha-Fehlerniveau (α): .05  
\* Power (1−β): .80  
\* Anzahl der Gruppen: 2 (Informationsökologie: realistisch vs. invertiert)  
\* Anzahl der Messungen: 2 (Valenz: vertrauenswürdig vs. betrügerisch)  
\* Schätzung Korrelation Messwiederholungen: r=.30

Ergebnis und Rekrutierungsziel: Die Analyse ergibt eine benötigte Mindeststichprobe von N=88. Um potenziellen Datenausschluss (z. B. durch nicht bestandene Attention-Checks oder technische Fehler, siehe Punkt 6) zu kompensieren, streben wir eine Brutto-Stichprobe von N=100 an. Die Datenerhebung wird beendet, sobald 100 vollständige Datensätze vorliegen. Sollte die Rekrutierung vorzeitig stagnieren, gilt eine strikte Untergrenze von N=88 validen Datensätzen, um die angestrebte Power von .80 zu gewährleisten.

## Anything else you would like to pre-register?

#### Stichprobe und Rekrutierung

Die Zielpopulation umfasst Personen aller Geschlechter im Alter von 18 bis 35 Jahren, die zum Zeitpunkt der Erhebung den Beziehungsstatus „Single“ angeben. Die Rekrutierung erfolgt über Online-Kanäle, primär durch Ausschreibungen in sozialen Medien (z. B. Instagram, Reddit) sowie über Messengerdienste.

#### Experimentelles Design und Ablauf

Die Studie wird als computergestütztes Online-Experiment durchgeführt und gliedert sich in folgende Phasen:

1. Instruktion & Demografie: Nach der informierten Einwilligung werden demografische Daten (Alter, Geschlecht, sexuelle Orientierung) erhoben. Basierend auf der sexuellen Präferenz erfolgt die Zuweisung zum entsprechenden Stimulus-Set (geschlechtskongruent).
2. Lernphase (Encoding): Den Probanden werden nacheinander 24 Profilbilder präsentiert, die jeweils mit einer Vignette (Verhaltensbeschreibung) gekoppelt sind. Die Probanden werden randomisiert einer von zwei Bedingungen zugewiesen, welche die Verteilung der Valenz steuert:  
   \* Bedingung A: 70 % vertrauenswürdige / 30 % betrügerische Beschreibungen.  
   \* Bedingung B: 30 % vertrauenswürdige / 70 % betrügerische Beschreibungen.
3. Testphase (Retrieval): In einem unangekündigten Gedächtnistest werden die 24 alten sowie 24 neue Gesichter (Distraktoren) präsentiert.  
   \* Recognition: Probanden entscheiden per „Ja/Nein“-Urteil, ob das Gesicht bereits in der Lernphase gezeigt wurde.  
   \* Source Memory: Bei einer „Ja“-Antwort wird die Quelle abgefragt. Hierbei stehen drei Optionen zur Auswahl: „Vertrauenswürdig“, „Betrügerisch“ oder „Weiß nicht“. Die Einbindung der dritten Kategorie dient dazu, die Datenqualität zu erhöhen und das Rauschen durch erzwungene Ratetendenzen zu minimieren.
4. Abschluss & Fragebögen: Erhebung der Beziehungspräferenz (7-Punkte-Likert-Skala) sowie ein abschließendes Debriefing.

#### Stimuli und Validierung

\* Vignetten: Die Verhaltensbeschreibungen für die Kategorien „vertrauenswürdig“ und „betrügerisch“ werden vorab in einem Pretest hinsichtlich ihrer Valenz validiert, um sicherzustellen, dass die intendierten Kategorien eindeutig wahrgenommen werden.  
\* Bildmaterial: Es werden Gesichter aus dem MEBeauty-Dataset (Lebedeva et al., 2022) verwendet. Um Konfundierende Effekte durch den Beauty-is-good-Stereotyp zu vermeiden, werden ausschließlich Gesichter mit durchschnittlicher Attraktivität ausgewählt. Dies stellt sicher, dass die Gedächtnisleistung primär auf die zugewiesene Valenz und nicht auf die physische Attraktivität zurückzuführen ist.
