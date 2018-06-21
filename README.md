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

#### Models and numbers of predictor variables
We recommend starting to fit models with a reduced number of predictors (the list of 16 listed below). If 16 is too many, you can use 9 predictors (#'s 1-6; and 10-12 in the list below). However, eventually (and perhaps for the machine learning models), there is the full list of almost 100 predictor variables that should be used.

#### Response variables and response datasets
> TP, TN, chl, and Secchi

> FOR THE WORKSHOP, WE WILL USE the wq2_single.csv dataset below!

1. LATER: [data/wq1_temporal.csv](data/wq1_temporal.csv) - All SUMMER observations are retained (mid-june to mid-sept) through time per lake. the multiple temporal variables to model the variation rather than modeling temporal pattern in the data….treated as repeated measure.

2. WORKSHOP: [data/wq2_single.csv](data/wq2_single.csv) - A single SUMMER observation is selected that has the most water quality data from the above filtered dataset.

#### Holdouts

|        Metric        |                             Description                             |
|:--------------------:|:-------------------------------------------------------------------:|
|   random25_holdout   |           Hold out 25% of the dataset, randomly selected            |
|   random75_holdout   |           Hold out 75% of the dataset, randomly selected           |
|   hu4_ag50_holdout   | Sort the HU4 % total ag and select lakes in the top 50% as holdouts |
| hu4_strat75_holdout  |        Randomly select 75% of lakes in each HU4 as holdouts         |
| hu4_random50_holdout |    Randomly select 50% of HU4s and holdout all lakes in each HU4    |
|** not available yet ** cluster_strat75_holdout|  Hold out 75% of the dataset, stratified by the lake clusters     | 
| ** not available yet ** cluster_random50_holdout| Hold out 50% of the clusters (as a whole), randomly selected.    |

#### Predictor variables
1. Reduced list of preditors:
## Local or lake-scale
1) Maxdepth
2) Iws_lk_ratio [derived]
3) Iws_nlcd2006_for [derived]
4) lake_sdf
5) lakeconnection_v2 [derived]
6) IWS_streamdensity_streams_density_mperha
7) elevation_m
8) wlconnections_allwetlands_shoreline_%perim (CALCULATE AS: wlconnections_allwetlands_shoreline_km / lake_perim)
9) Iws_roaddensity_density_mperha
## Regional scale
10) Hu4_baseflow
11) Hu4_runoff
12) Hu4_Change in total N depos from 1990-2010 
13) hu4_agr
14) Hu4_total N deposition - 1990 
15) Hu4_prism_ppt_30yr_normal_800mm2_annual_mean
16) hu4_prism_tmean_30yr_normal_800mm2_annual_mean


1. [data/local_predictors.csv](data/local_predictors.csv)

2. [data/regional_predictors.csv](data/regional_predictors.csv)


#### Creating natural lake ecological clusters
Noah created these and will provide the link to the code for the clusters. 
### Local natural lake ecological cluster variables used to create the clusters
LAKESLOCUS101:	elevation_m
LAKESLOCUS101:	Lake_area_ha
LAKESLOCUS101:	Lake_perim_meters {included here only to calculated SDF}
CALCULATED:		  lake_sdf
IWS_105:		    Iws_ha
IWS_PERIM: 		  Iws_perimkm {included here only to calculated IWS_SDF}
CALCULATED:		  Iws_sdf
CALCULATED: 		iws_lk_ratio
IWS_CONN105:		IWS_streamdensity_streams_density_mperha
LAKESGEO105:		 Latewisconsinglaciation_glacial (this is from the lagosne$lakes.geo table)
LAKESGEO105: 	  Lakeconnection_v2 (join ISO/HW into one category, then DRSTREAM, DRSTREAMLK as own category)
LAKESGEO105: 	  Upstream_lakes_4ha_area_ha
LAKESGEO105: 	  upstream_lakes_4ha_count
LAKESGEO105: 	  wlconnections_allwetlands_shoreline_km {Included here to calculated % perim}
LAKESGEO105:  	wlconnections_allwetlands_shoreline_%perim (CALCULATE: shoreline_km/lake_perim)
IWS_CONN105:		Iws_wl_allwetlandsundissolved_overlapping_area_pct


FOR ARCHIVING PURPOSES ONLY (use this readme file if possible and update it if it is wrong): See this [Google Doc](https://docs.google.com/document/d/1SupPLm-Tww5CyHHUf4k3YYq-UqnGKzyErm4Bz9yBOZg/edit?usp=sharing) for description of the overall analysis plan
