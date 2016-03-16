# StatApp-Collaborative-Filtering
ENSAE ParisTech Master Project 2015-2016 "Films Recommandation System by Collaborative Filtering" 

Authors group: CUI Biwei/ DELGADO Claudia/ MIAH Mehdi/ MPELI MPELI Ulrich/ <br />
Tutors group: COTTET Vincent/ SEBBAR Mehdi

## Architecture

```
project
|   README.md
|   main_preparation.R

|___ANALYSIS
    |   pdf_analysis.Rmd

|___DATA
    |   ml-100k
        |   data.Movies.tsv
        |   data.Ratings.tsv
        |   data.Users.tsv
        
|___NAIVEALGORITHMS
    |   main_predictionsTest.R
    |   naive_predictions.R
    |   recommandation_meanByMovie.R
    
|___NEIGHBORHOODBASEDALGORITHMS
    |   nearest_neighbor.R
    |   proxi_Users.R
    |   proxi_UsersComplet.R
    |   Q_Neighbors.R
    
|___RESULTS
    |   ml-100k
    		|   dejaVu.Rdata
    		|   recap.Movies.tsv
    		|   recap.Users.tsv
	  	|   results_naivePredictionTest.tsv

|___UTIL
    |   Clean
        |   main_clean_ml-100k.R
    |   deja_Vu.R    
    |   error_function.R
    |   open_files.R
    |   split_data.R
    |   stat_Movies.R
    |   stat_Users.R
    
```

## Descriptifs

### Fonctions

naive_predictions.R <br />
recommandation_meanByMovie.R <br />

nearest_neighbor.R <br />
proxi_Users.R <br />
proxi_UsersComplet.R <br />
Q_Neighbors.R <br />

deja_Vu.R <br />
error_function.R <br />
open_files.R <br />
split_data.R <br />
stat_Movies.R <br />
stat_Users.R <br />
	
### Fonctions main

main_predictionsTest.R <br />

main_clean_ml-100k.R  <br />

main_preparation.R
