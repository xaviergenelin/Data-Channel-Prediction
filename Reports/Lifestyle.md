Lifestyle Analysis
================
Group 6, Xavier Genelin, Dave Bergeron
10/28/2021

-   [Introduction](#introduction)
-   [Load and Manipulate Data](#load-and-manipulate-data)
-   [Data Exploration](#data-exploration)
    -   [Graph 1](#graph-1)
    -   [Graph 2](#graph-2)
    -   [Graph 3](#graph-3)
    -   [Graph 4](#graph-4)
    -   [Graph 5](#graph-5)
    -   [Graph 6](#graph-6)
    -   [Contingency Tables](#contingency-tables)
        -   [2-way Contingency Table](#2-way-contingency-table)
        -   [3-Way Contingency Table](#3-way-contingency-table)
    -   [Numerical Summary](#numerical-summary)
-   [Modeling](#modeling)
    -   [Linear Regression](#linear-regression)
        -   [Linear Model 1](#linear-model-1)
        -   [Linear Model 2](#linear-model-2)
    -   [Ensemble](#ensemble)
        -   [Random Forest](#random-forest)
        -   [Boosted Tree](#boosted-tree)
-   [Comparison](#comparison)

# Introduction

For this project, we’ll be analyzing the [Online News Popularity Data
Set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity)
from the UCI Machine Learning Repository. For this analysis, we’ll be
examining the Lifestyle data channel. Our analysis for this channel will
go into an exploratory data analysis with different graphs and numerical
summaries, as well as trying to predict the number of shares with
different types of models. We’ll examine different variables in our
analysis, but `shares` is our main variable of interest throughout this
report. This is the response variable that will be used in each of the
predictive models.

Our exploratory analysis will look at how the variables `weekday` (day
of the week), , `num_imgs` (number of images), and `timedelta` (days
between article publication and dataset acquisition) relate to the
number of shares. We also examine how `rate_positive_words` (rate of
positive words among non-neutral tokens) and `global_subjectivity` (text
subjectivity) relate to one another.

Our predictive models will be looking at how well the variables
`num_imgs`, `kw_avg_avg` (Average keyword for average shares), `LDA_02`
(Closeness to LDA topic 2), `LDA_03` (Closeness to LDA topic 3),
`average_token_length` (average length of words in the content),
`rate_negative_words` (rate of negative words among non-neutral tokens),
`kw_min_avg` (worst keyword for minimum shares), and `kw_max_avg` (best
keyword for average shares) are able to predict the number of `shares`
for different models.

Libraries that are being used:

-   `tidyverse`  
-   `rmarkdown`  
-   `caret`  
-   `corrplot`  
-   `doParellel`

# Load and Manipulate Data

Before doing our analysis, we need to load in the data and do some data
manipulation. Since we are only looking at the Lifestyle data channel,
we’ll filter the dataset on this channel and create a new variable for
the weekday from the columns `weekday_is_*`. After filtering and
creating the weekday variable, we’ll remove the columns that went into
creating those and the url column since these won’t help further in our
analysis. From the filtered data set we made a training and test set
with a 70:30 split to help with our modeling later.

``` r
# load in the data
news <- read_csv("OnlineNewsPopularity.csv")

#### this will be part of the overall parameters but we can change this as we go forward for different datasets
channel <- params$channel

# create a new column for the data channel and weekday
news <- news %>% 
 mutate(data_channel = 
          if_else(data_channel_is_lifestyle == 1, "Lifestyle", 
                  if_else(data_channel_is_entertainment == 1, "Entertainment", 
                          if_else(data_channel_is_bus == 1, "Bus", 
                                  if_else(data_channel_is_socmed == 1, "Socmed", 
                                          if_else(data_channel_is_tech == 1, "Tech", 
                                                  if_else(data_channel_is_world == 1, "World", "other")))))),
        weekday = if_else(weekday_is_monday == 1, "Monday", 
                          if_else(weekday_is_tuesday == 1, "Tuesday",
                                  if_else(weekday_is_wednesday == 1, "Wednesday",
                                          if_else(weekday_is_thursday == 1, "Thursday",
                                                  if_else(weekday_is_friday == 1, "Friday",
                                                          if_else(weekday_is_saturday == 1, "Saturday", "Sunday"))))))
        )

news$weekday <- factor(news$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Filter the news data set on the channel of interest
# remove the url and data_channel_is_* and weekday_is_* columns
news <- news %>% filter(data_channel == channel) %>% select(!c(url, starts_with("data_channel"), starts_with("weekday_is")))

# set seed for reproducibility
set.seed(55)

# create a list of indicies for the training set
trainIndex <- createDataPartition(news$shares, p = 0.7, list = FALSE)

# make training and test sets
newsTrain <- news[trainIndex, ]
newsTest <- news[-trainIndex, ]
```

# Data Exploration

In this section we’ll be analyzing our training set both graphically and
numerically.

## Graph 1

This plot shows the number of images associated with the number of
keywords. The chart bins the results and shows the clustering of images
for each count of keywords. The clustering should reveal if there is a
positive or negative relationship between the two variables, and what
number of keywords had the most or least amount of images associated
with it.

``` r
ggplot(data = newsTrain, aes(x = num_keywords, y = num_imgs)) + 
  geom_point(aes(color = num_imgs), position = "jitter") + 
  labs(x = "Keywords", y = "Number of Images", title="Images to Keywords", colour = "num_imgs")
```

![](../Reports/Lifestyle_files/figure-gfm/graph1-1.png)<!-- -->

## Graph 2

The plot below shows boxplots of the shares by weekday. The box itself
shows the middle 50% of the data for that weekday and any outliers are
shown as points outside of the box. The bottom end of the box shows the
number of shares that are at 25% for that weekday, the top end shows the
75th percentile of the data, and the line in the box shows the median.
This can help us tell if the data is skewed one way or another based on
where the line is within the box. We also added data points in blue that
show the average value based on the weekday as well to compare it with
the boxplot.

``` r
# get the average shares by weekday
avgValues <- newsTrain %>% group_by(weekday) %>% summarise(avg = mean(shares)) 

# ylim added because it was impossible to see anything without it due to large outliers
graph2 <- ggplot(newsTrain, aes(x = weekday, y = shares)) +
  geom_boxplot(fill = "grey") + 
  coord_cartesian(ylim = c(0,10000)) +
  geom_point(avgValues, mapping = aes(x = weekday, y = avg), color = "navy") + 
  geom_line(avgValues, mapping = aes(x = weekday, y = avg, group = 1), color = "navy") +
  labs(title = "Shares by Weekday", subtitle = "Means shown in navy blue", x = "Weekday", y = "Shares")
```

## Graph 3

The next graph shows the number of images and the number of shares that
we have in the data set, along with the different days in different
colors. This can give us some insight in a more detailed level if there
is any relationship with shares and images as a whole, or even if it
depends on the day. If there is a positive relationship between the two,
we’d expect to see that as images increase so does shares. If there’s a
negative relationship, as shares increase the number of images would go
down or vice versa.

``` r
ggplot(newsTrain, aes(x = num_imgs, y = shares)) +
  geom_point(aes(color = weekday)) +
  labs(title = "Shares vs Number of Images by Weekday", x = "Number of Images", y = "Shares")
```

![](../Reports/Lifestyle_files/figure-gfm/graph3-1.png)<!-- -->

## Graph 4

This graph shows the relationship between the global subjectivity and
rate of positive words variables. The graph returns a scatter plot along
with a trend line in order to determine what type of relationship may
exist between the data and variables. The plot coupled with the trend
line should reveal if the variables potential influence one another of
if they have a positive or negative relationship.

``` r
graph4 <- ggplot(data = newsTrain, aes(y = rate_positive_words, x = global_subjectivity)) + 
  geom_point(aes(color = rate_positive_words), position = "jitter") + 
  geom_smooth(formula = y ~ x, method = "loess") + 
  labs(x = "Global Subjectivity", y = "Rate Positive Words", 
       title="Correlation of Global Subjectivity to Rate of Positive Words", colour = "Rate Positive Words")
```

## Graph 5

This plot shows the number of shares across the time delta variable. The
time delta variable represents days between article publication and data
set acquisition. This will show where along the time delta number line
shares may be greatest or lowest, if some sort of pattern may exist that
is worth further exploration, or where the maximum number of shares were
observed.

``` r
graph5 <- ggplot(newsTrain, aes(x=timedelta)) + 
  geom_line(aes(y=shares)) + 
  labs(title="Shares across timedelta", y="Shares")
```

## Graph 6

The last graph we’ll examine is a correlation plot between all the
variables. The correlation will tell us how strong the relationship
between two variables is, and whether that relationship is positive or
negative. The color index will show the degree of the relationship
between the two. The darker the color is, the stronger the relationship
between those two variables. Our main variable of interest is `shares`
so we’re hoping to see some variables having a strong relationship in
either direction with that.

``` r
# remove the weekday column since this isn't numeric after mutating it
corrs <- cor(newsTrain %>% select(!weekday))

corrplot(corrs, method =  "color", tl.cex = 0.5, type = "upper")
```

![](../Reports/Lifestyle_files/figure-gfm/graph6-1.png)<!-- -->

## Contingency Tables

### 2-way Contingency Table

This contingency table shows the relationship of number of keywords
observed during the days of the week and the weekend. The 0 column
represents weekdays with the 1 column representing the weekend along
with counts of how may times the number of keywords was observed. The
column on the left shows the number of keywords observed. Analysis can
be conducted to determine where the greatest and least concentrations of
keywords were observed.

``` r
table(newsTrain$num_keywords, newsTrain$is_weekend)
```

    ##     
    ##        0   1
    ##   3    8   0
    ##   4   23   4
    ##   5   52  10
    ##   6  129  11
    ##   7  199  41
    ##   8  217  53
    ##   9  183  58
    ##   10 377 107

### 3-Way Contingency Table

This contingency table takes the 2-way table above and breaks it down
further by binning it across values of the worst keyword (min shares)
variable. This will allow exploration of number of keywords observed on
a weekday or weekend and see what those counts look like across the
different worst keyword (min shares) segments

``` r
table(newsTrain$num_keywords, newsTrain$is_weekend, newsTrain$kw_min_min)
```

    ## , ,  = -1
    ## 
    ##     
    ##        0   1
    ##   3    5   0
    ##   4   14   2
    ##   5   32   6
    ##   6   57   7
    ##   7   93  28
    ##   8   89  34
    ##   9   75  40
    ##   10 173  64
    ## 
    ## , ,  = 0
    ## 
    ##     
    ##        0   1
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    1   0
    ##   8    1   0
    ##   9    0   0
    ##   10   1   0
    ## 
    ## , ,  = 4
    ## 
    ##     
    ##        0   1
    ##   3    1   0
    ##   4    7   1
    ##   5   14   4
    ##   6   45   3
    ##   7   65   9
    ##   8   87  13
    ##   9   64  15
    ##   10 129  26
    ## 
    ## , ,  = 57
    ## 
    ##     
    ##        0   1
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   1   0
    ## 
    ## , ,  = 80
    ## 
    ##     
    ##        0   1
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   1   0
    ## 
    ## , ,  = 138
    ## 
    ##     
    ##        0   1
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   0   1
    ## 
    ## , ,  = 217
    ## 
    ##     
    ##        0   1
    ##   3    2   0
    ##   4    2   1
    ##   5    6   0
    ##   6   27   1
    ##   7   40   4
    ##   8   40   6
    ##   9   44   3
    ##   10  71  16
    ## 
    ## , ,  = 377
    ## 
    ##     
    ##        0   1
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   1   0

## Numerical Summary

Earlier we saw the boxplots show the number of shares based on weekdays,
but with the large values it could be difficult to see where specific
points lie on the graph, like the minimum value. We may not even have
seen the maximum value for some of them due to large outliers. The table
below will give the numeric summary of what we saw within the boxplots
to help give some values to what we saw visually earlier.

``` r
# Shares by weekday
numSum <- newsTrain %>% 
  group_by(weekday) %>% 
  summarise(Min = min(shares), Q1 = quantile(shares, 0.25), Mean = mean(shares), 
            Median = median(shares), Q3 = quantile(shares, 0.75), Max = max(shares), SD = sd(shares))

knitr::kable(numSum)
```

| weekday   | Min |   Q1 |     Mean | Median |   Q3 |    Max |        SD |
|:----------|----:|-----:|---------:|-------:|-----:|-------:|----------:|
| Monday    | 109 |  998 | 4263.553 |   1600 | 3600 | 196700 | 14153.637 |
| Tuesday   | 237 | 1025 | 4796.077 |   1500 | 3075 | 208300 | 15991.231 |
| Wednesday |  78 | 1000 | 3124.978 |   1600 | 2800 |  73100 |  5832.716 |
| Thursday  |  28 | 1050 | 3363.773 |   1600 | 3000 |  56000 |  6009.055 |
| Friday    | 127 | 1075 | 2723.373 |   1500 | 3100 |  32299 |  3558.199 |
| Saturday  | 446 | 1400 | 3719.417 |   2100 | 4000 |  43000 |  5040.609 |
| Sunday    | 613 | 1400 | 4022.408 |   2200 | 4300 |  33100 |  5165.628 |

# Modeling

In this section we’re going to compare the following models: random
forest, linear regression, and two ensemble models (we can fill this in
when we decide which two to do). Each of the models will be trying to
predict the amount of shares for each of the data channels.

## Linear Regression

Linear Regression models are used for understanding the relationship
between input and output numerical variables. These models can take on
many forms from simple where one variable is used to predict a response
to multiple regression where multiple predictor variables are used to
predict a response. Additionally, and in the context of this assignment,
different techniques can be used to prepare or train the linear
regression equation from data, the most common of which is called
Ordinary Least Squares. The two models below are both multiple linear
regression models, where multiple predictors from a training set of data
are being used to predict the number of shares which is the response
variable.

### Linear Model 1

The first linear model we’ll look at uses the variables `num_imgs`,
`kw_min_avg`, `kw_max_avg`, and `kw_avg_avg`. The values are centered
and scaled and the model use 10-fold cross validation on the training
set.

``` r
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

trainglmfit <- train(shares ~ num_imgs + kw_min_avg + kw_max_avg + kw_avg_avg,
                     data = newsTrain, method = "lm",
                     trControl = trainControl(method = "cv", number = 10),
                     preProcess = c("center", "scale"))

pred1 <- predict(trainglmfit, newdata = newsTest)

lm1Results <- postResample(pred1, obs = newsTest$shares)

stopCluster(cl)
```

### Linear Model 2

The next linear model uses the variables `num_imgs`, `kw_avg_avg`,
`LDA_02`, `LDA_03`, `average_token_length`, and `rate_negative_words`.
Similar to the previous model, the values are centered and scaled and
this uses 10-fold cross validation on the training set.

``` r
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

mlrFit <- train(shares ~  num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words,
                data = newsTrain, method = "lm",
                trControl = trainControl(method = "cv", number = 10),
                preProcess = c("center", "scale"))

mlrPred <- predict(mlrFit, newsTest)

mlrResults <- postResample(mlrPred, obs = newsTest$shares)

stopCluster(cl)
```

## Ensemble

### Random Forest

This model is a random forest and one of the three tree based methods.
Random Forests use the same idea as bagging, where multiple trees are
created from bootstrap samples then results are averaged. The difference
from bagging is not all predictors are used, rather a random subset of
predictors for each bootstrap sample/tree fit is utilized. This approach
minimizes the impact in the event a strong predictor is utilized.
Instead, by randomly selecting a subset of predictors, a good predictor
won’t dominate the tree fit. The random forest model uses the variables
`num_imgs`, `kw_avg_avg`, `LDA_02`, `LDA_03`, `average_token_length`,
and `rate_negative_words`. Those values are centered and scaled and this
uses 5-fold cross validation on the training set. We’re also testing
different tuning parameters to see which produces the optimal model.
We’ll check the tuning parameter `mtry` for values from 1 to 7.

``` r
# make cluster for parallel computing
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# random forest model with preprocessed data that is centered and scaled
# use 5-fold cross validation
# mtry from 1:7
rfFit <- train(shares ~ num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words,
               data = newsTrain, method = "rf",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", number = 5),
               tuneGrid = expand.grid(mtry = 1:7))

rfPred <- predict(rfFit, newsTest)

rfResults <- postResample(rfPred, obs = newsTest$shares)

stopCluster(cl)
```

The optimal mtry value for the random forest model is 1.

### Boosted Tree

This model is a boosted tree model which falls in the family of tree
based methods and is used in the slow training of trees. This is done by
having the trees grown sequentially, where each subsequent tree is grown
on a modified version of original data. The predictions are updated as
the trees are grown. Given the slow fitting of the model, it can often
be one of the preferred choices compared to bagging and random forest.
In the code, one can observe the shrinkage parameter is 0.1, sets the
speed of the fitting process and can be used to slow it down. The
boosted tree is using the variables `num_imgs`, `kw_avg_avg`, `LDA_02`,
`LDA_03`, `average_token_length`, and `rate_negative_words`. Those
values are centered and scaled and this uses 5-fold cross validation on
the training set. Similar to the random forest model, we’ll be besting
different tuning parameters to determine which is the optimal model for
these variables. The shrinkage and n.minobsinnode are set to 0.1 and 10
respectively, but then we’re also checking for the number of trees from
25, 50, 75, …, 200 and an interaction depth of 1, 2, 3, and 4.

``` r
cb <- makePSOCKcluster(5)
registerDoParallel(cb)

set.seed(30)
wtFit <- train(shares ~ num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words,
                data = newsTrain, method = "gbm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 5),
                tuneGrid = expand.grid(.n.trees = seq(25, 200, by = 25),
                                       .interaction.depth = seq(1, 4, by = 1),
                                       .shrinkage = (0.1),
                                       .n.minobsinnode = (10)))
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 89532063.7671             nan     0.1000 -48398.4937
    ##      2 89410095.3961             nan     0.1000 96234.8983
    ##      3 88888650.5873             nan     0.1000 -44827.8038
    ##      4 88780497.9058             nan     0.1000 58352.1677
    ##      5 88397014.5585             nan     0.1000 -153822.4566
    ##      6 88334421.3963             nan     0.1000 -27358.3104
    ##      7 88100015.3776             nan     0.1000 -236338.0443
    ##      8 88000605.8171             nan     0.1000 -34292.6297
    ##      9 87882216.0928             nan     0.1000 -341886.9941
    ##     10 87831093.7435             nan     0.1000 -278787.2538
    ##     20 87382640.8613             nan     0.1000 -30897.6934
    ##     40 86914523.6417             nan     0.1000 -108740.7182
    ##     50 86642477.9048             nan     0.1000 -306845.5446

``` r
wtPred <- predict(wtFit, newsTest)

wtResults <- postResample(wtPred, obs = newsTest$shares)

stopCluster(cb)
```

The optimal tuning parameters for the boosted tree were 50 trees and an
interaction depth of 1, along with the default values for `shrinkage`
and `n.minobsinnode`.

# Comparison

``` r
results <- rbind(t(lm1Results), t(mlrResults), t(rfResults), t(wtResults))
models <- c("Linear Model 1", "Linear Model 2", "Random Forest", "Boosted Trees")

results <- data.frame(results, row.names = models)

bestModel <- results %>% mutate(model = models) %>% filter(RMSE == min(RMSE))

knitr::kable(results)
```

|                |     RMSE |  Rsquared |      MAE |
|:---------------|---------:|----------:|---------:|
| Linear Model 1 | 7338.378 | 0.0063790 | 3296.471 |
| Linear Model 2 | 7361.524 | 0.0029709 | 3293.983 |
| Random Forest  | 7499.166 | 0.0032886 | 3406.172 |
| Boosted Trees  | 7424.820 | 0.0015421 | 3319.940 |

The best model out of the 4 that were tested was Linear Model 1 with an
RMSE of 7338.3777942.
