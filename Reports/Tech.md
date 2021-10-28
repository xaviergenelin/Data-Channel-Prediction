Tech Analysis
================
Group 6, Xavier Genelin, Dave Bergeron
10/28/2021

# Introduction

For this project, we’ll be analyzing the [Online News Popularity Data
Set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity)
from the UCI Machine Learning Repository. For this analysis, we’ll be
examining the Tech data channel. Our analysis for this channel will go
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
manipulation. Since we are only looking at the Tech data channel, we’ll
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

In this section we’ll be analyzing our training set both graphically and
numerically.

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

![](Reports/Tech_files/figure-gfm/graph1-1.png)<!-- -->

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

![](Reports/Tech_files/figure-gfm/graph2-1.png)<!-- -->

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

![](Reports/Tech_files/figure-gfm/graph3-1.png)<!-- -->

## Graph 4

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

![](Reports/Tech_files/figure-gfm/graph4-1.png)<!-- -->

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

![](Reports/Tech_files/figure-gfm/graph5-1.png)<!-- -->

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

![](Reports/Tech_files/figure-gfm/graph6-1.png)<!-- -->

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
    ##   3   26   6
    ##   4  105  13
    ##   5  356  36
    ##   6  635  50
    ##   7  916 106
    ##   8  803 123
    ##   9  690 120
    ##   10 960 199

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
    ##   3   12   1
    ##   4   58   7
    ##   5  174  15
    ##   6  314  19
    ##   7  495  54
    ##   8  369  46
    ##   9  342  46
    ##   10 473  96
    ## 
    ## , ,  = 0
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    0   0
    ##   4    0   0
    ##   5    0   0
    ##   6    0   0
    ##   7    5   0
    ##   8    6   0
    ##   9   10   0
    ##   10   7   0
    ## 
    ## , ,  = 4
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3   13   3
    ##   4   31   3
    ##   5  125  17
    ##   6  217  26
    ##   7  300  44
    ##   8  302  61
    ##   9  257  50
    ##   10 357  76
    ## 
    ## , ,  = 41
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
    ## , ,  = 45
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
    ##   10   0   1
    ## 
    ## , ,  = 57
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
    ##   10   1   1
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
    ##   10   0   1
    ## 
    ## , ,  = 82
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
    ## , ,  = 86
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
    ##   10   2   0
    ## 
    ## , ,  = 217
    ## 
    ##     
    ##        0   1
    ##   2    0   0
    ##   3    1   2
    ##   4   16   3
    ##   5   57   4
    ##   6  104   5
    ##   7  116   8
    ##   8  126  16
    ##   9   81  24
    ##   10 116  24
    ## 
    ## , ,  = 294
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
| Monday    | 192 | 1100 | 2659.290 |   1600 | 2800 |  51000 |  3604.561 |
| Tuesday   | 104 | 1075 | 2901.777 |   1500 | 2800 |  88500 |  4877.529 |
| Wednesday | 181 | 1000 | 3772.987 |   1600 | 3075 | 663600 | 21372.140 |
| Thursday  |  92 | 1000 | 2722.850 |   1600 | 2700 |  55200 |  4027.849 |
| Friday    | 140 | 1200 | 3049.384 |   1800 | 3000 | 104100 |  5599.108 |
| Saturday  | 168 | 1600 | 3724.418 |   2200 | 3800 |  96100 |  6047.669 |
| Sunday    | 206 | 1600 | 4056.670 |   2400 | 4000 |  83300 |  6250.376 |

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
    ##      1 108452260.5292             nan     0.1000 52659.5336
    ##      2 108396530.3181             nan     0.1000 51514.5430
    ##      3 108324489.0502             nan     0.1000 5790.2239
    ##      4 108226131.0044             nan     0.1000 75209.7640
    ##      5 108184151.8914             nan     0.1000 9421.0427
    ##      6 108139691.9687             nan     0.1000 18791.7977
    ##      7 108116559.0902             nan     0.1000 10762.4852
    ##      8 108065117.2098             nan     0.1000 -3536.9450
    ##      9 108018554.0814             nan     0.1000 -54268.8952
    ##     10 107966829.1508             nan     0.1000 -14811.1156
    ##     20 107668581.5381             nan     0.1000 17221.5567
    ##     40 107434680.6532             nan     0.1000 12516.9614
    ##     50 107318331.9138             nan     0.1000 -27881.9052

``` r
wtPred <- predict(wtFit, newsTest)

wtResults <- postResample(wtPred, obs = newsTest$shares)

stopCluster(cb)
```

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
| Linear Model 1 | 4242.898 | 0.0084658 | 2206.994 |
| Linear Model 2 | 4259.580 | 0.0057297 | 2213.372 |
| Random Forest  | 4409.044 | 0.0050876 | 2255.791 |
| Boosted Trees  | 4255.997 | 0.0053452 | 2159.343 |

The best model out of the 4 that were tested was Linear Model 1 with an
RMSE of 4242.9.
