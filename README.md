# StatApp-Collaborative-Filtering
ENSAE ParisTech Master Project 2015-2016 "Films Recommendation System by Collaborative Filtering" 

Authors group: CUI Biwei/ DELGADO Claudia/ MIAH Mehdi/ MPELI MPELI Ulrich/ <br />
Tutors group: COTTET Vincent/ SEBBAR Mehdi

## Objectif

Dans ce projet, nous souhaitons recommander des films à un utilisateur au moyen des notes attribuées par l'ensemble des utilisateurs.
Pour cela, nous avons identifier trois grandes façons de procéder : 
- par une approche naïve : recommander les films qui ont obtenus les meilleurs moyennes ; 
- par une approche des plus proches voisins (knn_user) : recommander les meilleurs films parmi les utilisateurs partageant les mêmes goûts que l'utilisateur final ; 
- par une approche de réduction (svd) : recommander les films après une décomposition matricielle des notes

## Phases du programme

Pour faire uniquement de la recommandation, allez directement à la phase 5.

### Phase 1 - Nettoyage

Les données originales sont disponibles sur le site de Grouplens : http://grouplens.org/datasets/movielens/

Les modifications apportées sont : 
- pour le problème ml-100k : 
	- suppression des doublons de films (ID des films en doublons : 246 268|297 303|329 348|304 500|573 670|266 680|305 865|876 881|878 1003|1256 1257|309 1606|1395 1607|1175 1617| 1477 1625|1645 1650|1234 1654|711 1658|1429 1680|)
- pour le problème ml-1m : 
	- suppression des films présents dans la base des films mais absents de la base des notes (177 films)
	- correction du titre de film "Et Dieu créa la femme"

### Phase 2 - Préparation des données

Les données nécessaires à la prédiction sont générées.
```R
# Se placer dans le dossier courant
source("main_preparation.R")
```

### Phase 3 - Analyser les données

Deux fichiers pdf sont générés contenant les analyses descriptives des deux problèmes
```R
# Se placer dans le dossier Analysis
source("pdf_analysis_ml-100k.Rmd")
source("pdf_analysis_ml-1m.Rmd")
```

### Phase 4 - Evaluer les prédictions

Pour chaque famille de modèles, les notes des bases de test sont prédites à partir de l'étude des bases d'apprentissage
```R
# Se placer dans le dossier courant
source("./CrossValidation/main_split.R") pour générer les datasets et la base vierge

# Remplacer '***' par le nom de la famille de méthode
source("./***/main_predictionsTest_***.R")
```

### Phase 5 - Recommander

Pour effectuer les recommandations pour un utisateur :
```R
# Se placer dans le dossier courant
source("main.R")
```

### Phase 6 - Tests a posteriori des recommandations

Les recommandations sont testées selon divers critères
```R
# Se placer dans le dossier courant
source("./Tests/main_test_popularitybias.R")
source("./Tests/main_test_robust.R")
```

## Organisation des dossiers

**Analysis**				: analyse les données de chaque problème <br />

**CrossValidation** 				: génére les sous-datasets servant à la validation croisée et la base vierge, contient les données nécessaires à la prédiction, générées par "main_preparation.R" <br />

**Data**						: contient les données de ml-100k et ml-1m nettoyées <br />

**NaiveAlgorithms**				: prédit et recommande en suivant les algorithmes naïfs <br />

**NeighborhoodBasedAlgorithms**		: prédit et recommande en suivant les algorithmes des plus proches voisins <br />

**Results**					: contient les données nécessaires à la recommandation finale, contient les résultats de la validation croisée à propos de l'estimation de modèles et de paramètres <br />

**SVD**						: prédit et recommande en suivant les algorithmes basés sur la décomposition de matrices <br />

**Tests**						: test les méthodes sur la présence d'un biais de popularité et sur l'influence des notes sur les recommandations <br />

**Util**						: contient les fonctions les plus utiles

