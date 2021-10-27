world Analysis
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
examining the world data channel. Our analysis for this channel will go
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
manipulation. Since we are only looking at the world data channel, we’ll
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
          if_else(data_channel_is_lifestyle == 1, "lifestyle", 
                  if_else(data_channel_is_entertainment == 1, "entertainment", 
                          if_else(data_channel_is_bus == 1, "bus", 
                                  if_else(data_channel_is_socmed == 1, "socmed", 
                                          if_else(data_channel_is_tech == 1, "tech", 
                                                  if_else(data_channel_is_world == 1, "world", "other")))))),
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

``` r
ggplot(data = newsTrain, aes(x = num_keywords, y = num_imgs)) + 
  geom_point(aes(color = shares), position = "jitter") + 
  labs(x = "Keywords", y = "Number of Images", title="Images to Keywords", colour = "Shares")
```

![](../Reports/world_files/figure-gfmgraph1-1.png)<!-- -->

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

![](../Reports/world_files/figure-gfmgraph2-1.png)<!-- -->

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

![](../Reports/world_files/figure-gfmgraph3-1.png)<!-- --> ## Graph 4

``` r
ggplot(data = newsTrain, aes(y = rate_positive_words, x = global_subjectivity)) + 
  geom_point(aes(color = rate_positive_words), position = "jitter") + 
  geom_smooth(formula = y ~ x, method = "loess") + 
  labs(x = "Global Subjectivity", y = "Rate Positive Words", 
       title="Correlation of Global Subjectivity to Rate of Positive Words", colour = "Rate Positive Words")
```

![](../Reports/world_files/figure-gfmgraph4-1.png)<!-- -->

## Graph 5

``` r
ggplot(newsTrain, aes(x=timedelta)) + 
  geom_line(aes(y=shares)) + 
  labs(title="Shares across timedelta", y="Shares")
```

![](../Reports/world_files/figure-gfmgraph5-1.png)<!-- -->

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

![](../Reports/world_files/figure-gfmgraph6-1.png)<!-- -->

## Contingency Tables

### 2-way Contingency Table

Showing counts of keywords that appear on the weekend vs. not on the
weekend.

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

I need to figure out what a kw_min_min means, but it works for the three
way table given the fixed number of categories.

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
    ##   weekday     Min    Q1  Mean Median    Q3    Max     SD
    ##   <fct>     <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl>
    ## 1 Monday      111  835  2441.   1100  1800 141400  7466.
    ## 2 Tuesday      42  761  2395.   1100  1700 115700  6536.
    ## 3 Wednesday    48  785  1862.   1100  1700  49800  2922.
    ## 4 Thursday     42  788. 2591.   1100  1800 284700 10019.
    ## 5 Friday       35  853  2163.   1100  1800 128500  5432.
    ## 6 Saturday     43 1000  2434.   1500  2600  25200  3010.
    ## 7 Sunday       91 1100  2635.   1400  2300  55600  4807.

# Modeling

In this section we’re going to compare the following models: random
forest, linear regression, and two ensemble models (we can fill this in
when we decide which two to do). Each of the models will be trying to
predict the amount of shares for each of the data channels.

## Linear Regression

``` r
#Model 1 -  Selecting predictors based on relevancy from ANOVA results after running the summary from model using all predictors in another model - 4 predictors
worldtrainglmfit <- lm(shares ~ num_imgs + kw_min_avg + kw_max_avg + kw_avg_avg, data = newsTrain, trControl = trainControl(method = "cv", number = 10), preProcess = c("center", "scale"))

pred1 <- predict(worldtrainglmfit, newdata = newsTest)

postResample(pred1, obs = newsTest$shares)[2]
```

    ##   Rsquared 
    ## 0.02334528

``` r
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

mlrFit <- train(shares ~  num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, data = newsTrain, trControl = trainControl(method = "cv", number = 10), preProcess = c("center", "scale"))

mlrPred <- predict(mlrFit, newsTest)

mlrResults <- postResample(mlrPred, obs = newsTest$shares)

stopCluster(cl)
```

## Ensemble

### Random Forest

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

This is the boosted tree model using all predictors in the `newsTrain`
data set.

``` r
# set.seed(30)
# wtFit <- train(shares ~ num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, 
#                data = newsTrain, method = "gbm", 
#                preProcess = c("center", "scale"), 
#                trControl = trainControl(method = "cv",number = 5), 
#                tuneGrid = expand.grid(.n.trees = seq(25, 200, by = 25), 
#                                       .interaction.depth = seq(1, 4, by = 1), 
#                                       .shrinkage = (0.1), 
#                                       .n.minobsinnode = (10)))
# wtFit
```

#### Boosted Tree Test Results

Still working on this,getting an error

# Comparison

``` r
# started the comparison with a table
data.frame(mlrResults, rfResults)
```

    ##            mlrResults    rfResults
    ## RMSE     4.717567e+03 4.652023e+03
    ## Rsquared 2.421682e-02 2.605321e-02
    ## MAE      1.922787e+03 1.863662e+03
