## Lake Prediction-Extrapolation Project (LPEP)
The overall goal of this effort is to identify the factors that influence our ability to predict lake water quality in unsampled lakes at subcontinental scales. In other words, under what conditions do we will have trouble predicting lake water quality in unsampled lakes? 

### Objective

Specifically, we will try to identify the importance of different degrees of ‘sampling bias’ in influencing the predictive error of models of lake TP, TN, Secchi, and chlorophyll. The sampling bias in the model-building dataset that we will examine include:

> Sample size, lake-ecological context characteristics, regions, and regional land use

### Approach
The quant teams will each build a model - one stats model and one machine-learning model, which will be fit using 7 different hold-out datasets (see below). We will compare the model endpoints (see below) for the two models for each hold-out dataset.

#### Model structure
Based on the models that we have built for LAGOS-NE for understanding the controls of lake water quality, we will build models with the factors that we know are important: joint distributions of responses, local covariates, regional covariates, and region-specific covariates. 

#### Model details 
Model endpoint: Point estimate of nutrients for every lake in hold-out dataset [uncertainty later, all 50k lakes later]
Model endpoint: Mean squared predictive error on natural log scale
Note: Add 0.1 to the zero values

#### Response variables and response datasets
> TP, TN, chl, and Secchi

> FOR THE WORKSHOP, WE WILL USE the wq2_single.csv dataset below!

1. LATER: [data/wq1_temporal.csv](data/wq1_temporal.csv) - All SUMMER observations are retained (mid-june to mid-sept) through time per lake. the multiple temporal variables to model the variation rather than modeling temporal pattern in the data….treated as repeated measure.

2. WORKSHOP: [data/wq2_single.csv](data/wq2_single.csv) - A single SUMMER observation is selected that has the most water quality data from the above filtered dataset.

#### Holdouts

|        Metric        |                             Description                             |
|:--------------------:|:-------------------------------------------------------------------:|
|   random25_holdout   |           Hold out 25% of the dataset, randomly selected            |
|   random75_holdout   |           Hold out 75% of the dataset, randomly selected.           |
|   hu4_ag50_holdout   | Sort the HU4 % total ag and select lakes in the top 50% as holdouts |
| hu4_strat75_holdout  |        Randomly select 75% of lakes in each HU4 as holdouts         |
| hu4_random50_holdout |    Randomly select 50% of HU4s and holdout all lakes in each HU4    |

#### Predictor variables

See this [Google Doc](https://docs.google.com/document/d/1SupPLm-Tww5CyHHUf4k3YYq-UqnGKzyErm4Bz9yBOZg/edit?usp=sharing) for a listing of predictor variables

1. [data/local_predictors.csv](data/local_predictors.csv)

2. [data/regional_predictors.csv](data/regional_predictors.csv)



See this [Google Doc](https://docs.google.com/document/d/1SupPLm-Tww5CyHHUf4k3YYq-UqnGKzyErm4Bz9yBOZg/edit?usp=sharing) for description of the overall analysis plan
