## Lake Prediction-Extrapolation Project (LPEP)

### Goal
To identify the factors that influence our ability to predict lake water quality in unsampled lakes at subcontinental scales. In other words, under what conditions do we will have trouble predicting lake water quality in unsampled lakes? 

### Objective
To identify the importance of different degrees of ‘sampling bias’ in influencing the predictive error of models of lake TP, TN, water color and chlorophyll. The sampling bias in the model-building dataset that we will examine include:

a.  Sample size 
b.  The distribution of ecological context characteristics 
c.  The distribution of regions 
d.  The land use

### Approach
#### RESPONSE variables
TP, TN, color-true, color-apparent, chl

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
