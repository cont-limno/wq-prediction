## Lake Prediction-Extrapolation Project (LPEP)
The overall goal of this effort is to identify the factors that influence our ability to predict lake water quality in unsampled lakes at subcontinental scales. In other words, under what conditions do we will have trouble predicting lake water quality in unsampled lakes? 

### Objective

Specifically, we will try to identify the importance of different degrees of ‘sampling bias’ in influencing the predictive error of models of lake TP, TN, Secchi, and chlorophyll. The sampling bias in the model-building dataset that we will examine include:

> Sample size, lake-ecological context characteristics, regions, and regional land use

### Approach
The quant teams will each build a model - one stats model and one machine-learning model, which will be fit using 7 different hold-out datasets (see below). We will compare the model endpoints (see below) for the two models for each hold-out dataset.

#### Model structure
Based on the models that we have built for LAGOS-NE for understanding the controls of lake water quality, we will build models with the factors that we know are important: joint distributions of responses, local covariates, regional covariates, and region-specific covariates. 

#### Response variables
> TP, TN, chl, and Secchi

#### Model details 
Model endpoint: Point estimate of nutrients for every lake in hold-out dataset [uncertainty later, all 50k lakes later]

Model endpoint: Mean squared predictive error on natural log scale

Note: To account for zeros, add 0.1 to response variables


#### Models and numbers of predictor variables
We have comppiled a list of about 100 predictor variables (about half at the local (lake) scale and about half at the regional scale, which lakes are nested within (there are about 65 regions). However, we recommend starting to fit models with a reduced number of predictors (the list of 16 predictors listed below). If 16 is too many, you can use 9 predictors (#'s 1-6; and 10-12 in the list below). However, eventually (and perhaps for the machine learning models), there is the full list of 100 predictor variables that should be used.

#### Response variable datasets 
> FOR THE WORKSHOP, WE WILL USE the wq2_single.csv dataset below!

WORKSHOP: [data/wq2_single.csv](data/wq2_single.csv) - A single SUMMER observation is selected that has the most water quality data from the above filtered dataset.

LATER: [data/wq1_temporal.csv](data/wq1_temporal.csv) - All SUMMER observations are retained (mid-june to mid-sept) through time per lake. The multiple samples per lake would be treated as 'repeated measures'. BUT, we likely won't have time to do this for the workshop. 

#### Holdout datasets (listed as 'variables' in the response variable dataset)

|        Metric        |                             Description (TRUE are holdouts/testing data)      |
|:--------------------:|:-------------------------------------------------------------------:|
|   random25_holdout   |           Hold out 25% of the dataset, randomly selected            |
|   random75_holdout   |           Hold out 75% of the dataset, randomly selected           |
|   hu4_ag50_holdout   | Sort the HU4 % total ag and select lakes in the top 50% as holdouts |
| hu4_strat75_holdout  |        Randomly select 75% of lakes in each HU4 as holdouts         |
| hu4_random50_holdout |    Randomly select 50% of HU4s and holdout all lakes in each HU4    |
|** not available yet ** cluster_strat75_holdout|  Hold out 75% of the dataset, stratified by the lake clusters     | 
| ** not available yet ** cluster_random50_holdout| Hold out 50% of the clusters (as a whole), randomly selected.    |

## Predictor variables

## Data files with predictors
> [data/local_predictors.csv](data/local_predictors.csv)

> [data/regional_predictors.csv](data/regional_predictors.csv)

## Shortened predictor list
#### Local or lake-scale
1) Maxdepth
2) Iws_lk_ratio [derived]
3) Iws_nlcd2006_for [derived]
4) lake_sdf
5) lakeconnection_v2 [derived]
6) IWS_streamdensity_streams_density_mperha
7) elevation_m
8) wlconnections_allwetlands_shoreline_%perim (CALCULATE AS: wlconnections_allwetlands_shoreline_km / lake_perim)
9) Iws_roaddensity_density_mperha
#### Regional scale
10) Hu4_baseflow
11) Hu4_runoff
12) Hu4_Change in total N depos from 1990-2010 
13) hu4_agr
14) Hu4_total N deposition - 1990 
15) Hu4_prism_ppt_30yr_normal_800mm2_annual_mean
16) hu4_prism_tmean_30yr_normal_800mm2_annual_mean

## List of local predictors
#### LOCAL: Watershed land use/cover, topography, road density
iws_nlcd2006_pct_11

iws_nlcd2006_pct_21

iws_nlcd2006_pct_22

iws_nlcd2006_pct_23

Iws_nlcd2006_pct_24

iws_nlcd2006_urb

iws_nlcd2006_pct_31

iws_nlcd2006_pct_41

iws_nlcd2006_pct_42

Iws_nlcd2006_pct_43

iws_nlcd2006_for

iws_nlcd2006_pct_52

iws_nlcd2006_pct_71

iws_nlcd2006_pct_81

Iws_nlcd2006_pct_82

iws_nlcd2006_agr

Iws_nlcd2006_pct_90

Iws_nlcd2006_pct_95

iws_nlcd2006_wet

iws_tri_mean

iws_tri_max

Iws_roaddensity_density_mperha


#### LOCAL: 100m buffer land use, topography, road density
Buff100_nlcd2006_pct_11

Buff100_nlcd2006_pct_21

Buff100_nlcd2006_pct_22

Buff100_nlcd2006_pct_23

Buff100_nlcd2006_pct_24

buff100_nlcd2006_urb

Buff100_nlcd2006_pct_31

Buff100_nlcd2006_pct_41

Buff100_nlcd2006_pct_42

Buff100_nlcd2006_pct_43

buff100_nlcd2006_for

Buff100_nlcd2006_pct_52

Buff100_nlcd2006_pct_71

Buff100_nlcd2006_pct_81

Buff100_nlcd2006_pct_82

Buff100_nlcd2006_agr

buff100_nlcd2006_pct_90

buff100_nlcd2006_pct_95

buff100_nlcd2006_wet

Buff100_tri_mean

Buff100_tri_max

Buff100_roaddensity_density_mperha


#### LOCAL: Lake and watershed morphometry and connectivity
Maxdepth

Meandepth

elevation_m

Lake_area_ha

Lake_perim_meters

lake_sdf

Iws_ha

Iws_perimkm

Iws_sdf

iws_lk_ratio

wlconnections_allwetlands_shoreline_%perim (CALCULATED AS: 
wlconnections_allwetlands_shoreline_km / lake_perim)

IWS_streamdensity_streams_density_mperha

Latewisconsinglaciation_glacial (this is from the lagosne$lakes.geo table)

lakeconnection_v2

## List of regional predictors
#### REGIONAL (HU4): land use/cover, topography, road density
hu4_nlcd2006_pct_11

hu4_nlcd2006_pct_21

hu4_nlcd2006_pct_22

hu4_nlcd2006_pct_23

Hu4_nlcd2006_pct_24

hu4_nlcd2006_urb

hu4_nlcd2006_pct_31

hu4_nlcd2006_pct_41

hu4_nlcd2006_pct_42

Hu4_nlcd2006_pct_43

hu4_nlcd2006_for

hu4_nlcd2006_pct_52

hu4_nlcd2006_pct_71

hu4_nlcd2006_pct_81

Hu4_nlcd2006_pct_82

hu4_nlcd2006_agr

hu4_nlcd2006_pct_90

hu4_nlcd2006_pct_95

hu4_nlcd2006_wet

hu4_tri_mean

hu4_tri_max

hu4_roaddensity_density_mperha


#### REGIONAL (HU4): HYDROLOGY and CONNECTIVITY
hu4_baseflowindex_mean

hu4_runoff_mean

Hu4_groundwaterrecharge_mean

hu4_streamdensity_streams_density_mperha

hu4_lakes_overlapping_area_pct 

Hu4_HWisolated_pct

hu4_lakes_drstream_overlapping_area_pct

hu4_lakes_drlakestream_overlapping_area_pct


#### REGIONAL (HU4): Deposition
hu4_dep_so4_1990_mean

hu4_dep_so4_2010_mean

So4dep_19902010_diff

hu4_dep_totaln_1990_mean

hu4_dep_totaln_2010_mean

totalNdep_19902010_diff

hu4_dep_no3_1990_mean

Hu4_dep_no3_2010_mean

No3dep_19902010_diff


#### REGIONAL (HU4): geology
hu4_surficialgeology_alluv_pct

Hu4_surficialgeology_colluv_pct

hu4_surficialgeology_gf_out_pct

hu4_surficialgeology_till_loam_pct

Hu4_surficialgeology_till_sand_pct

Hu4_surficialgeology_till_clay_pct

hu4_surficialgeology_till_oth_pct


#### REGIONAL (HU4): Climate
hu4_prism_ppt_30yr_normal_800mm2_annual_mean

hu4_prism_tmean_30yr_normal_800mm2_annual_mean

hu4_prism_min_30yr_normal_800mm2_annual_mean

hu4_prism_max_30yr_normal_800mm2_annual_mean


#### ADDITIONAL VARIABLES TO INCLUDE IN THE DATASET 
Lagoslakeid

eventida1087

Nhd_lat

nhd_lon

programname

programtype

sampledate

Gnis_name

lake_perm_meters

iws_zoneid

hu4_zoneid

hu6_zoneid

hu8_zoneid

hu12_zoneid

edu_zoneid

county_zoneid

state_zoneid


## Creating natural lake ecological clusters
Noah will create these and will provide the link to the code for the clusters. 

#### Local natural lake ecological cluster variables used to create the clusters
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
