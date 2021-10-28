World Analysis
================
Group 6, Xavier Genelin, Dave Bergeron
10/27/2021

I don’t think we need all of these libraries. We can keep them for now
but the project doesn’t require anything with SQL so those can be
removed later.

Dave: Agree, one of my bad habits not pruning the list from one
assignment to the other.

Xavier: I’ve added a comment to each of them that I’ve used

# Introduction

For this project, we’ll be analyzing the(Online News Popularity Data
Set)\[<https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity>\]
from the UCI Machine Learning Repository. For this analysis, we’ll be
examining the World data channel. Our analysis for this channel will go
into an exploratory data analysis with different graphs and numerical
summaries, as well as trying to predict the number of shares with
different types of models.

Libraries that are being used:

-   `tidyverse`  
-   `caret`  
-   `corrplot`  
-   `doParellel`

# Load Data w/Automation

Before doing our analysis, we need to load in the data and do some data
manipulation. Since we are only looking at the World data channel, we’ll
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

# Summarizations

I was hoping to see the correlation plot for the variables to see what
was most related to shares. Kind of rough with the large number of
variables

## Graph 1

This plot shows the binning of the number of images associated with
number of keywords. Generally speaking the number of images remains low
for each count of keywords, but does slowly increase as the number of
keywords increases. This does suggest there is somewhat of a positive
correlation betweent the two variables.

``` r
ggplot(data = newsTrain, aes(x = num_keywords, y = num_imgs)) + 
  geom_point(aes(color = num_imgs), position = "jitter") + 
  labs(x = "Keywords", y = "Number of Images", title="Images to Keywords", colour = "num_imgs")
```

![](../Reports/World_files/figure-gfmgraph1-1.png)<!-- -->

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

ggplot(newsTrain, aes(x = weekday, y = shares)) +
  geom_boxplot(fill = "grey") + 
  coord_cartesian(ylim = c(0,10000)) +
  geom_point(avgValues, mapping = aes(x = weekday, y = avg), color = "navy") + 
  geom_line(avgValues, mapping = aes(x = weekday, y = avg, group = 1), color = "navy") +
  labs(title = "Shares by Weekday", subtitle = "Means shown in navy blue", x = "Weekday", y = "Shares")
```

![](../Reports/World_files/figure-gfmgraph2-1.png)<!-- -->

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

![](../Reports/World_files/figure-gfmgraph3-1.png)<!-- --> \#\# Graph 4

This graph shows the relationship between the global subjectivity and
rate of positive words. Their is a positive correlation between the
variables that increases at a decreasing rate then starts to decline,
suggesting that the rate of positive words is highest when global
subjectivity is .75.

``` r
ggplot(data = newsTrain, aes(y = rate_positive_words, x = global_subjectivity)) + 
  geom_point(aes(color = rate_positive_words), position = "jitter") + 
  geom_smooth(formula = y ~ x, method = "loess") + 
  labs(x = "Global Subjectivity", y = "Rate Positive Words", 
       title="Correlation of Global Subjectivity to Rate of Positive Words", colour = "Rate Positive Words")
```

![](../Reports/World_files/figure-gfmgraph4-1.png)<!-- -->

## Graph 5

This plot shows the number of shares across the time delta variable. The
overall number of shares is highest when the timedelta is around 75,
then is less frequent as the timedelta grows larger. There are some
intermittent spikes of shares time deltas of 275 and 425. This chart
also suggests that number of shares will be less as the timedelta grows
larger.

``` r
ggplot(newsTrain, aes(x=timedelta)) + 
  geom_line(aes(y=shares)) + 
  labs(title="Shares across timedelta", y="Shares")
```

![](../Reports/World_files/figure-gfmgraph5-1.png)<!-- -->

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

![](../Reports/World_files/figure-gfmgraph6-1.png)<!-- -->

## Contingency Tables

### 2-way Contingency Table

This contingency table shows the relationship of number of keywords
observed on the weekend. The 0 column represents weekdays, which should
suggest the overall counts for that column will be larger that the 1
column. The interesting observation is the counts appear to be someone
proportional to one another, with the distributions appearing to mirror
one another. The implies there is consistency of the number of key words
observed between the weekdays and weekends.

``` r
table(newsTrain$num_keywords, newsTrain$is_weekend)
```

    ##     
    ##        0   1
    ##   2    1   0
    ##   3   65  21
    ##   4  274  49
    ##   5  594  95
    ##   6  947 136
    ##   7  970 109
    ##   8  755 111
    ##   9  628  81
    ##   10 915 149

### 3-Way Contingency Table

This contingency table takes the 2-way table above and breaks it down
further by the worst keyword (min shares) variable. The majority of the
keywords fall into the -1, 4, and 217 bins.

``` r
table(newsTrain$num_keywords, newsTrain$is_weekend, newsTrain$kw_min_min)
```

    ## , ,  = -1
    ## 
    ##     
    ##        0   1
    ##   2    1   0
    ##   3   58  19
    ##   4  224  40
    ##   5  485  81
    ##   6  721 117
    ##   7  680  87
    ##   8  510  82
    ##   9  391  50
    ##   10 599 111
    ## 
    ## , ,  = 0
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    0   0
    ##   4    0   0
    ##   5    1   0
    ##   6    3   0
    ##   7    2   0
    ##   8    1   0
    ##   9    0   0
    ##   10   0   0
    ## 
    ## , ,  = 4
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    4   2
    ##   4   28   6
    ##   5   62   8
    ##   6  140  14
    ##   7  205  17
    ##   8  175  22
    ##   9  203  27
    ##   10 262  33
    ## 
    ## , ,  = 42
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
    ## , ,  = 201
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
    ##   2    0   0
    ##   3    3   0
    ##   4   22   3
    ##   5   46   6
    ##   6   83   5
    ##   7   83   5
    ##   8   69   7
    ##   9   34   4
    ##   10  52   5

## Numerical Summary

Earlier we saw the boxplots show the number of shares based on weekdays,
but with the large values it could be difficult to see where specific
points lie on the graph, like the minimum value. We may not even have
seen the maximum value for some of them due to large outliers. The table
below will give the numeric summary of what we saw within the boxplots
to help give some values to what we saw visually earlier.

``` r
# Shares by weekday
newsTrain %>% 
  group_by(weekday) %>% 
  summarise(Min = min(shares), Q1 = quantile(shares, 0.25), Mean = mean(shares), 
            Median = median(shares), Q3 = quantile(shares, 0.75), Max = max(shares), SD = sd(shares))
```

    ## # A tibble: 7 x 8
    ##   weekday     Min    Q1  Mean
    ##   <fct>     <dbl> <dbl> <dbl>
    ## 1 Monday      111  835  2441.
    ## 2 Tuesday      42  761  2395.
    ## 3 Wednesday    48  785  1862.
    ## 4 Thursday     42  788. 2591.
    ## 5 Friday       35  853  2163.
    ## 6 Saturday     43 1000  2434.
    ## 7 Sunday       91 1100  2635.
    ## # ... with 4 more variables:
    ## #   Median <dbl>, Q3 <dbl>,
    ## #   Max <dbl>, SD <dbl>

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
are being used to predict the number of shares.

``` r
worldtrainglmfit <- lm(shares ~ num_imgs + kw_min_avg + kw_max_avg + kw_avg_avg, data = newsTrain, trControl = trainControl(method = "cv", number = 10), preProcess = c("center", "scale"))

pred1 <- predict(worldtrainglmfit, newdata = newsTest)

lm1Results <- postResample(pred1, obs = newsTest$shares)
```

``` r
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

mlrFit <- train(shares ~  num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, data = newsTrain, method = "lm", trControl = trainControl(method = "cv", number = 10), preProcess = c("center", "scale"))

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
won’t dominate the tree fit.

``` r
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

rfFit <- train(shares ~ num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, 
               data = newsTrain, method = "rf", 
               preProcess = c("center", "scale"), 
               trControl = trainControl(method = "cv", number = 5), 
               tuneGrid = expand.grid(mtry = 1:7))

rfPred <- predict(rfFit, newsTest)

rfResults <- postResample(rfPred, obs = newsTest$shares)
#rfFit$bestTune best mtry value
stopCluster(cl)
```

### Boosted Tree

This model is a boosted tree model which falls in the family of tree
based methods and is used in the slow training of trees. This is done by
having the trees grown sequentially, where each subsequent tree is grown
on a modified version of original data. The predictions are updated as
the trees are grown. Given the slow fitting of the model, it can often
be one of the preferred choices compared to bagging and random forest.
In the code, one can observe the shrinkage parameter is 0.1, sets the
speed of the fitting process and can be used to slow it down.

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
    ##      1 43564849.5571             nan     0.1000 9520.5888
    ##      2 43393306.4875             nan     0.1000 20774.2917
    ##      3 43327404.7814             nan     0.1000 38360.1262
    ##      4 43191806.7892             nan     0.1000 40025.6883
    ##      5 43112178.9371             nan     0.1000 37063.6335
    ##      6 43014545.7794             nan     0.1000 -17000.2831
    ##      7 42905318.9284             nan     0.1000 47760.7874
    ##      8 42828100.4691             nan     0.1000 42015.3671
    ##      9 42760680.1587             nan     0.1000 9067.6491
    ##     10 42714361.4666             nan     0.1000 -28914.8396
    ##     20 42319615.1150             nan     0.1000 -20513.6913
    ##     40 41941082.3694             nan     0.1000 15724.4369
    ##     60 41614400.8675             nan     0.1000 -1019.3049
    ##     75 41358651.2660             nan     0.1000 -26784.8451

``` r
stopCluster(cb)
wtFit
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 5900 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered
    ##  (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4720, 4721, 4720, 4719, 4720 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees
    ##   1                   25    
    ##   1                   50    
    ##   1                   75    
    ##   1                  100    
    ##   1                  125    
    ##   1                  150    
    ##   1                  175    
    ##   1                  200    
    ##   2                   25    
    ##   2                   50    
    ##   2                   75    
    ##   2                  100    
    ##   2                  125    
    ##   2                  150    
    ##   2                  175    
    ##   2                  200    
    ##   3                   25    
    ##   3                   50    
    ##   3                   75    
    ##   3                  100    
    ##   3                  125    
    ##   3                  150    
    ##   3                  175    
    ##   3                  200    
    ##   4                   25    
    ##   4                   50    
    ##   4                   75    
    ##   4                  100    
    ##   4                  125    
    ##   4                  150    
    ##   4                  175    
    ##   4                  200    
    ##   RMSE      Rsquared  
    ##   6000.173  0.01574460
    ##   6014.412  0.01874216
    ##   5996.015  0.02072217
    ##   6010.621  0.02025284
    ##   6015.465  0.01987163
    ##   6014.627  0.02050615
    ##   6017.356  0.02017823
    ##   6009.963  0.02027472
    ##   6070.774  0.01409621
    ##   6114.202  0.01561580
    ##   6133.134  0.01478230
    ##   6150.798  0.01572920
    ##   6153.054  0.01704299
    ##   6187.009  0.01669648
    ##   6193.575  0.01764587
    ##   6205.638  0.01831524
    ##   6057.341  0.01642703
    ##   6086.757  0.01712460
    ##   6070.288  0.01942412
    ##   6097.179  0.01962290
    ##   6128.864  0.01988972
    ##   6125.864  0.02114285
    ##   6163.859  0.01904161
    ##   6193.823  0.01986473
    ##   6091.218  0.01272326
    ##   6127.421  0.01635532
    ##   6158.858  0.01705550
    ##   6201.300  0.01711393
    ##   6237.283  0.01685410
    ##   6263.314  0.01833831
    ##   6250.534  0.01834005
    ##   6291.418  0.01844238
    ##   MAE     
    ##   1970.812
    ##   1974.064
    ##   1956.375
    ##   1946.557
    ##   1956.601
    ##   1954.242
    ##   1962.279
    ##   1950.003
    ##   1987.863
    ##   1989.086
    ##   1992.917
    ##   1994.303
    ##   2007.735
    ##   2018.342
    ##   2021.068
    ##   2024.994
    ##   1969.028
    ##   1981.547
    ##   1981.860
    ##   1997.855
    ##   2008.983
    ##   2008.871
    ##   2022.920
    ##   2038.959
    ##   1974.295
    ##   2001.058
    ##   2016.640
    ##   2047.706
    ##   2052.036
    ##   2068.158
    ##   2080.778
    ##   2093.046
    ## 
    ## Tuning parameter
    ##  held constant at a value
    ##  of 10
    ## RMSE was used to select
    ##  the optimal model using
    ##  the smallest value.
    ## The final values used for
    ##  1, shrinkage = 0.1
    ##  and n.minobsinnode = 10.

# Comparison

``` r
# started the comparison with a table
data.frame(lm1Results, mlrResults, rfResults)
```

    ##            lm1Results
    ## RMSE     4.617892e+03
    ## Rsquared 2.334528e-02
    ## MAE      1.844947e+03
    ##            mlrResults
    ## RMSE     4.599819e+03
    ## Rsquared 3.088937e-02
    ## MAE      1.835799e+03
    ##             rfResults
    ## RMSE     4677.4276319
    ## Rsquared    0.0220365
    ## MAE      1871.8115205