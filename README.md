## Lake Prediction-Extrapolation Project (LPEP)

See this [Google Doc](https://docs.google.com/document/d/1SupPLm-Tww5CyHHUf4k3YYq-UqnGKzyErm4Bz9yBOZg/edit?usp=sharing) for more info.

### Goal
To identify the factors that influence our ability to predict lake water quality in unsampled lakes at subcontinental scales. In other words, under what conditions do we will have trouble predicting lake water quality in unsampled lakes? 

### Objective
To identify the importance of different degrees of ‘sampling bias’ in influencing the predictive error of models of lake TP, TN, water color and chlorophyll. The sampling bias in the model-building dataset that we will examine include:

> Sample size, ecological context characteristics, regions, land use

### Approach

#### Response variables

TP, TN, chl, Secchi

Two response variable datasets:

1. [data/wq1_temporal.csv](data/wq1_temporal.csv) - All SUMMER observations are retained (mid-june to mid-sept) through time per lake. the multiple temporal variables to model the variation rather than modeling temporal pattern in the data….treated as repeated measure.

2. [data/wq2_single.csv](data/wq2_single.csv) - A single SUMMER observation is selected that has the most water quality data from the above filtered dataset.

#### Predictor variables

1. [data/local_iws.csv](data/local_iws.csv)

2. [data/local_buff100.csv](data/local_buff100.csv)

3. [data/local_lake_watershed.csv](data/local_lake_watershed.csv)

4. [data/regional_hu4.csv](data/regional_hu4.csv)

#### Model structure

Based on the models that we have built for LAGOS-NE for understanding the controls of lake water quality, we will build models with the factors that we know are important: joint distributions, region-specific covariates, local covariates, regional covariates. We will do this in a machine learning environment and a statistical environment

#### Hold-out datasets

We will fit the above models to 7 different datasets that have different kinds of ‘hold-out’ datasets to examine the effects of sampling bias on model predictive performance. We will base these hold-out datasets on ecological understanding related to the following. 

_FINAL DECISIONS (June )_

1. Random-25 -- Hold out 25% of the dataset, randomly selected. This is a common approach in general and may help serve as a ‘baseline’ for comparison

2. Random-75 -- Hold out 75% of the data (this is close to the proportion of data we have in-situ measurements for in LAGOS-NE)

3. Stratified-Random-75 by ecological-context cluster -- Hold out 75% of the data, stratified by ecological context clusters with the key predictors we think are important.

4. Random-75 hold out 2-3 entire clusters -- 

5. Stratified-Random-75 by HUC4 -- Hold out 75% of the data, stratified only by HUC4

6. Geographic-HUC4 -hold out 20 random HUC4

7. Land use-HUC4 - hold out the top 25% of HUC4 with high ag

Thus, by comparing across hold-out datasets, we will learn which type of dataset performs best and which types of bias most limit our ability to extrapolate predictive models to unsampled lakes.
