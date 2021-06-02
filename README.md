
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codeBase

# Introduction

## What functions are included in the package?

Currently the master branch contains the following functions however we
have many others as pull request

``` r
ls("package:codeBase")
#>  [1] "check_all_identical"            "create_html_pack"              
#>  [3] "encode_freq"                    "expl_categorical"              
#>  [5] "expl_na"                        "expl_summary"                  
#>  [7] "feature_importance_permutation" "hh_test"                       
#>  [9] "hh_train"                       "metric_confusion_matrix"       
#> [11] "metric_deviance"                "metric_F1"                     
#> [13] "metric_Fbeta"                   "metric_gini"                   
#> [15] "metric_mae"                     "metric_nloglik"                
#> [17] "metric_pode"                    "metric_pove"                   
#> [19] "metric_precision"               "metric_PrecisionRecall"        
#> [21] "metric_recall"                  "metric_rmse"                   
#> [23] "metric_ROC"                     "null_gini"                     
#> [25] "null_PrecisionRecall"           "null_ROC"                      
#> [27] "plot_2way_comparison"           "plot_ALE"                      
#> [29] "plot_feature"                   "plot_feature_predictions"      
#> [31] "plot_gini"                      "plot_lift_curve"               
#> [33] "plot_lift_curve_relative"       "plot_magic_carpet"             
#> [35] "plot_PDP"                       "plot_PrecisionRecall"          
#> [37] "plot_ROC"                       "plotting_numerical_buckets"    
#> [39] "prep_char_num_sort"             "prep_num_band"                 
#> [41] "prep_num_bin"                   "prep_numeric_caps"
```

# Using the Package

## Import ‘Predicting Household Sale Prices’ dataset

Importing the packages inbuilt testing datasets

``` r
data(hh_train)
data(hh_test)
```

This data is the Californian house prices dataset and has a target of
SalePrice

## Data Exploration

### expl\_na

For each variable, find the number or percentage of NA’s in a dataset

``` r
na_vars <- codeBase::expl_na(df = hh_train, na.strings = NULL, ignore.case = FALSE) 
```

We can now see that we have some factors which are useless as they are
mostly unknown:

``` r
head(dplyr::arrange(na_vars, -na)) %>% kable()
```

| var         |   na |     perc |
|:------------|-----:|---------:|
| PoolQC      | 1453 | 99.52055 |
| MiscFeature | 1406 | 96.30137 |
| Alley       | 1369 | 93.76712 |
| Fence       | 1179 | 80.75342 |
| FireplaceQu |  690 | 47.26027 |
| LotFrontage |  259 | 17.73973 |

### expl\_categorical

For each categorical variable, find the number of / percentage of each
level

``` r
codeBase::expl_categorical(df = hh_train, char.level = 3, num.level =2)  %>% kable()
```

| var        | level  | class     |    n |       perc |
|:-----------|:-------|:----------|-----:|-----------:|
| Alley      | Grvl   | character |   50 |  3.4246575 |
| Alley      | Pave   | character |   41 |  2.8082192 |
| Alley      | NA     | character | 1369 | 93.7671233 |
| CentralAir | N      | character |   95 |  6.5068493 |
| CentralAir | Y      | character | 1365 | 93.4931507 |
| LandSlope  | Gtl    | character | 1382 | 94.6575342 |
| LandSlope  | Mod    | character |   65 |  4.4520548 |
| LandSlope  | Sev    | character |   13 |  0.8904110 |
| PavedDrive | N      | character |   90 |  6.1643836 |
| PavedDrive | P      | character |   30 |  2.0547945 |
| PavedDrive | Y      | character | 1340 | 91.7808219 |
| Street     | Grvl   | character |    6 |  0.4109589 |
| Street     | Pave   | character | 1454 | 99.5890411 |
| Utilities  | AllPub | character | 1459 | 99.9315068 |
| Utilities  | NoSeWa | character |    1 |  0.0684932 |

This format can be easily plotted to show factor distributions for each
factor (either in a loop or using ggplot2 facet functions)

### expl\_summary

Find the mean, standard deviation, length, and NA count of numeric and
integer variables, with the option to group by other variables

``` r
codeBase::expl_summary(hh_train, summarise_vars = "SalePrice", group_vars = "YrSold") %>% kable()
```

| YrSold |     mean |       sd |   n | na\_count |
|-------:|---------:|---------:|----:|----------:|
|   2006 | 182549.5 | 79426.84 | 314 |         0 |
|   2007 | 186063.2 | 85768.17 | 329 |         0 |
|   2008 | 177360.8 | 69735.61 | 304 |         0 |
|   2009 | 179432.1 | 80879.24 | 338 |         0 |
|   2010 | 177393.7 | 80451.28 | 175 |         0 |

### encode\_freq

Apply frequency encoding with the option to group the rarest levels

``` r
# Add some unknowns to show what it looks like
hh_train$Neighborhood[5:10] <- NA
```

From the below output we can see that the factor Neighborhood has more
levels than can usefully be modelled

``` r
hh_train %>% group_by(Neighborhood) %>% summarise(Volume = n()) %>% kable()
```

| Neighborhood | Volume |
|:-------------|-------:|
| Blmngtn      |     17 |
| Blueste      |      2 |
| BrDale       |     16 |
| BrkSide      |     57 |
| ClearCr      |     28 |
| CollgCr      |    150 |
| Crawfor      |     51 |
| Edwards      |    100 |
| Gilbert      |     79 |
| IDOTRR       |     37 |
| MeadowV      |     17 |
| Mitchel      |     48 |
| NAmes        |    225 |
| NoRidge      |     40 |
| NPkVill      |      9 |
| NridgHt      |     77 |
| NWAmes       |     72 |
| OldTown      |    112 |
| Sawyer       |     74 |
| SawyerW      |     59 |
| Somerst      |     85 |
| StoneBr      |     25 |
| SWISU        |     25 |
| Timber       |     38 |
| Veenker      |     11 |
| NA           |      6 |

We decide to frequency encode the factor

``` r
nhood_freq <- codeBase::encode_freq(data = hh_train$Neighborhood, 
                                n_levels = 5, 
                                min_level_count = NULL, 
                                unknown_levels = NULL,
                                unknown_treatment_method = 1)
```

which gives us with the transformed factor

``` r
hh_train$nhood_freq <- factor(x = nhood_freq$data, labels = nhood_freq$levels)
hh_train %>% group_by(nhood_freq) %>% summarise(Volume = n()) %>% kable()
```

| nhood\_freq | Volume |
|:------------|-------:|
| Unknown     |      6 |
| NAmes       |    225 |
| CollgCr     |    150 |
| OldTown     |    112 |
| Edwards     |    100 |
| Somerst     |     85 |
| Other       |    782 |

## Build a Model

We build a model using standard R functions

``` r
hh_train %<>% mutate(GarageYrBlt = coalesce(GarageYrBlt, YearBuilt))
model_vars <- c('GarageYrBlt', 'nhood_freq', 'OverallCond', 'YearBuilt', 'YrSold', target)
hh_train %<>% select(all_of(model_vars))

dataset <- sample(1:10, nrow(hh_train), replace = TRUE)
dTrn <- hh_train[dataset[dataset<8],]
dVal <- hh_train[dataset[dataset>7],]

mTrn <- model.matrix(~., select(dTrn, -SalePrice))
mVal <- model.matrix(~., select(dVal, -SalePrice))

xTrn <- xgb.DMatrix(mTrn, label = dTrn$SalePrice)
xVal <- xgb.DMatrix(mVal, label = dVal$SalePrice)

params_xgb <- list(nthread = 4
                   ,alpha = 0
                   ,lambda = 1
                   ,booster = 'gbtree'
                   ,objective = "reg:squarederror" ,eval_metric = 'rmse' ,maximize = FALSE
)

model_xgb <- xgb.train(data = xTrn
                       ,params = params_xgb  
                       ,nrounds = 100
                       ,early_stopping_rounds = 250
                       ,print_every_n = 20
                       ,watchlist = list(train = xTrn, eval = xVal))
#> [09:27:47] WARNING: amalgamation/../src/learner.cc:573: 
#> Parameters: { "maximize" } might not be used.
#> 
#>   This may not be accurate due to some parameters are only used in language bindings but
#>   passed down to XGBoost core.  Or some parameters are not used but slip through this
#>   verification. Please open an issue if you find above cases.
#> 
#> 
#> [1]  train-rmse:150514.562500    eval-rmse:98545.468750 
#> Multiple eval metrics are present. Will use eval_rmse for early stopping.
#> Will train until eval_rmse hasn't improved in 250 rounds.
#> 
#> [21] train-rmse:127.156044   eval-rmse:49563.132812 
#> [41] train-rmse:0.108875 eval-rmse:49639.621094 
#> [61] train-rmse:0.018694 eval-rmse:49639.707031 
#> [81] train-rmse:0.018694 eval-rmse:49639.707031 
#> [100]    train-rmse:0.018694 eval-rmse:49639.707031


dTrn$pred <- predict(object = model_xgb, newdata = xTrn)
dVal$pred <- predict(object = model_xgb, newdata = xVal)

xgboost::xgb.importance(feature_names = colnames(mTrn),
                                                          model = model_xgb)
#>              Feature         Gain       Cover   Frequency
#> 1:       GarageYrBlt 9.866174e-01 0.877731160 0.872964169
#> 2: nhood_freqCollgCr 1.338263e-02 0.117529988 0.123778502
#> 3:         YearBuilt 9.971814e-15 0.004738852 0.003257329
```

## Model Evaluation

### plot\_PDP

We can now plot PDP plots for any factor

``` r
codeBase::plot_PDP(data = as.data.frame(mTrn),
               model = model_xgb,
               explain_col = 'GarageYrBlt',
               n_bins = 20,
               use_plotly = FALSE)
```

![](README_files/figure-gfm/plot_PDP-1.png)<!-- -->

### plot\_lift\_curve

We can also plot useful global model outputs such as lift curves

``` r
codeBase::plot_lift_curve(actual = dTrn$SalePrice,
                      predicted = dTrn$pred,
                      use_plotly = FALSE)
```

![](README_files/figure-gfm/plot_lift_curve_plotly-1.png)<!-- -->

### plotly vs ggplot2

Some users may require plotly and others ggplot2 so all plots work with
both engines. This can be controlled with the `use_plotly` parameter of
the plotting functions.

## Metrics

We have many metrics all of which have the same construction of
arguments meaning they can be used by other functions or in loops

``` r
codeBase::metric_rmse(actual = dTrn$SalePrice, predicted = dTrn$pred)
#> [1] 36530.87
codeBase::metric_mae(actual = dTrn$SalePrice, predicted = dTrn$pred)
#> [1] 35177.08

codeBase::metric_deviance(actual = dTrn$SalePrice, predicted = dTrn$pred, family = "gaussian")
#> [1] 1334504222
codeBase::metric_nloglik(actual = dTrn$SalePrice, predicted = dTrn
                         $pred, family = "gaussian")
#> [1] 12.06549
```
