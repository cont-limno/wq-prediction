## Lake Prediction-Extrapolation Project (LPEP)

See this [Google Doc](https://docs.google.com/document/d/1SupPLm-Tww5CyHHUf4k3YYq-UqnGKzyErm4Bz9yBOZg/edit?usp=sharing) for a listing of predictor variables

See this [Google Doc](https://docs.google.com/document/d/1SupPLm-Tww5CyHHUf4k3YYq-UqnGKzyErm4Bz9yBOZg/edit?usp=sharing) for description of the overall analysis plan

### Objective

To identify the importance of different degrees of ‘sampling bias’ in influencing the predictive error of models of lake TP, TN, water color and chlorophyll. The sampling bias in the model-building dataset that we will examine include:

> Sample size, ecological context characteristics, regions, land use

### Approach

#### Response variables

> TP, TN, chl, Secchi

1. [data/wq1_temporal.csv](data/wq1_temporal.csv) - All SUMMER observations are retained (mid-june to mid-sept) through time per lake. the multiple temporal variables to model the variation rather than modeling temporal pattern in the data….treated as repeated measure.

2. [data/wq2_single.csv](data/wq2_single.csv) - A single SUMMER observation is selected that has the most water quality data from the above filtered dataset.

#### Holdouts

|        Metric        |                             Description                             |
|:--------------------:|:-------------------------------------------------------------------:|
|   random25_holdout   |           Hold out 25% of the dataset, randomly selected            |
|   random75_holdout   |           Hold out 75% of the dataset, randomly selected.           |
|   hu4_ag50_holdout   | Sort the HU4 % total ag and select lakes in the top 50% as holdouts |
| hu4_strat75_holdout  |        Randomly select 75% of lakes in each HU4 as holdouts         |
| hu4_random50_holdout |    Randomly select 50% of HU4s and holdout all lakes in each HU4    |

#### Predictor variables

1. [data/local_predictors.csv](data/local_predictors.csv)

2. [data/regional_predictors.csv](data/regional_predictors.csv)
