# StatApp-Collaborative-Filtering
ENSAE ParisTech Master Project 2015-2016 "Films Recommandation System by Collaborative Filtering" 

Authors group: CUI Biwei/ DELGADO Claudia/ MIAH Mehdi/ MPELI MPELI Ulrich/ <br />
Tutors group: COTTET Vincent/ SEBBAR Mehdi

## Architecture

```
project
|   README.md

|___ANALYSIS
    |   main_analysis.R

|___DATA
    |   ml-100k
        |   data.Ratings.csv
        |   data.Movies.csv
        |   data.Users.csv
        
|___NAIVEALGORITHMS
    |   recommandation_naive.R
    |   recommandation_naive2.R
    |   main_PredictionsTest.R
    |   naive_predictions.R
    
|___NEIGHBORHOODBASEDALGORITHMS
    |   nearest_neighbor.R
    |   proxi_Users.R
    |   proxi_UsersComplet.R
    
|___RESULTS
    |   ml-100k
	  	|   results_naivePredictionTest.csv
    	|   stat.Movies.csv
    	|   stat.Users.csv

|___UTIL
    |   Clean
        |   main_clean_ml-100.R
    |   error_function.R
    |   open_files.R
    |   split_data.R
    |   stat_Movies.R
    |   stat_Users.R
    
```

## Descriptifs

### Fonctions

naive_predictions.R

proxi_Users.R
proxi_UsersComplet.R
nearest_neighbor.R

error_function.R
open_files.R
split_data.R
stat_Movies.R
stat_Users.R
	
### Fonctions main

main_analysis.R

main_PredictionsTest.R
recommandation_naive.R
recommandation_naive2.R

main_clean_ml-100.R