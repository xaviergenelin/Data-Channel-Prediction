Bus Analysis
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
examining the Bus data channel. Our analysis for this channel will go
into an exploratory data analysis with different graphs and numerical
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
manipulation. Since we are only looking at the Bus data channel, we’ll
filter the dataset on this channel and create a new variable for the
weekday from the columns `weekday_is_*`. After filtering and creating
the weekday variable, we’ll remove the columns that went into creating
those and the url column since these won’t help further in our analysis.
From the filtered data set we made a training and test set with a 70:30
split to help with our modeling later.

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

![](../Reports/Bus_files/figure-gfm/graph1-1.png)<!-- -->

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

![](../Reports/Bus_files/figure-gfm/graph3-1.png)<!-- -->

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

![](../Reports/Bus_files/figure-gfm/graph6-1.png)<!-- -->

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
    ##   2   12   0
    ##   3  139  22
    ##   4  496  26
    ##   5  770  69
    ##   6  759  90
    ##   7  594  64
    ##   8  443  44
    ##   9  314  39
    ##   10 438  63

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
    ##   2    8   0
    ##   3   99  18
    ##   4  300  17
    ##   5  397  41
    ##   6  397  51
    ##   7  312  27
    ##   8  221  17
    ##   9  140  21
    ##   10 210  33
    ## 
    ## , ,  = 0
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    0   0
    ##   4    2   0
    ##   5    1   0
    ##   6    4   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   1   0
    ## 
    ## , ,  = 4
    ## 
    ##     
    ##        0   1
    ##   2    2   0
    ##   3   26   4
    ##   4  133   9
    ##   5  253  26
    ##   6  271  28
    ##   7  204  25
    ##   8  165  18
    ##   9  126   9
    ##   10 161  17
    ## 
    ## , ,  = 47
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   1   0
    ## 
    ## , ,  = 63
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    0   0
    ##   8    0   0
    ##   9    0   0
    ##   10   1   0
    ## 
    ## , ,  = 217
    ## 
    ##     
    ##        0   1
    ##   2    2   0
    ##   3   14   0
    ##   4   61   0
    ##   5  119   2
    ##   6   87  11
    ##   7   78  12
    ##   8   57   9
    ##   9   48   9
    ##   10  64  13

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

| weekday   | Min |      Q1 |     Mean | Median |   Q3 |    Max |        SD |
|:----------|----:|--------:|---------:|-------:|-----:|-------:|----------:|
| Monday    |   1 |  955.00 | 3518.600 |   1400 | 2600 | 652900 | 23086.197 |
| Tuesday   | 156 |  913.00 | 3296.582 |   1300 | 2300 | 310800 | 12884.709 |
| Wednesday |  63 |  882.25 | 2773.676 |   1300 | 2100 | 158900 |  9143.581 |
| Thursday  |  81 |  904.25 | 3132.276 |   1300 | 2300 | 306100 | 15395.042 |
| Friday    |  22 |  979.00 | 2253.142 |   1400 | 2300 |  92100 |  4345.576 |
| Saturday  | 150 | 1700.00 | 4546.887 |   2550 | 4100 | 144400 | 11731.601 |
| Sunday    | 746 | 1400.00 | 3666.594 |   2100 | 3900 |  56900 |  5463.199 |

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
    ##      1 202483084.4328             nan     0.1000 193990.5714
    ##      2 201451562.6944             nan     0.1000 368818.8689
    ##      3 198708827.7580             nan     0.1000 28794.5475
    ##      4 196435745.6394             nan     0.1000 355576.1948
    ##      5 196102479.1185             nan     0.1000 -233212.0880
    ##      6 195684118.5696             nan     0.1000 -44524.5485
    ##      7 195451302.4860             nan     0.1000 -151495.5590
    ##      8 194285588.3303             nan     0.1000 -203255.9312
    ##      9 193336820.4406             nan     0.1000 -456105.6499
    ##     10 191824283.6307             nan     0.1000 -614305.9017
    ##     20 185683877.6964             nan     0.1000 -688048.3212
    ##     25 183459761.2646             nan     0.1000 -718378.3127

``` r
wtPred <- predict(wtFit, newsTest)

wtResults <- postResample(wtPred, obs = newsTest$shares)

stopCluster(cb)
```

The optimal tuning parameters for the boosted tree were 25 trees and an
interaction depth of 2, along with the default values for `shrinkage`
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
| Linear Model 1 | 16626.96 | 0.0075013 | 2611.534 |
| Linear Model 2 | 16635.20 | 0.0064121 | 2620.235 |
| Random Forest  | 16718.69 | 0.0064374 | 2684.625 |
| Boosted Trees  | 16849.70 | 0.0004825 | 2777.023 |

The best model out of the 4 that were tested was Linear Model 1 with an
RMSE of 1.6626958^{4}.
