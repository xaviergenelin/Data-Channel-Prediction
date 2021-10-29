World Analysis
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
examining the World data channel. Our analysis for this channel will go
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
graph1 <- ggplot(data = newsTrain, aes(x = num_keywords, y = num_imgs)) + 
  geom_point(aes(color = num_imgs), position = "jitter") + 
  labs(x = "Keywords", y = "Number of Images", title="Images to Keywords", colour = "num_imgs")

graph1
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

# ylim added because it was impossible to see anything without it due to large outliers
graph2 <- ggplot(newsTrain, aes(x = weekday, y = shares)) +
  geom_boxplot(fill = "grey") + 
  coord_cartesian(ylim = c(0,10000)) +
  geom_point(avgValues, mapping = aes(x = weekday, y = avg), color = "navy") + 
  geom_line(avgValues, mapping = aes(x = weekday, y = avg, group = 1), color = "navy") +
  labs(title = "Shares by Weekday", subtitle = "Means shown in navy blue", x = "Weekday", y = "Shares")

graph2
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
graph3 <- ggplot(newsTrain, aes(x = num_imgs, y = shares)) +
  geom_point(aes(color = weekday)) +
  labs(title = "Shares vs Number of Images by Weekday", x = "Number of Images", y = "Shares")

graph3
```

![](../Reports/World_files/figure-gfmgraph3-1.png)<!-- -->

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

graph4
```

![](../Reports/World_files/figure-gfmgraph4-1.png)<!-- -->

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

graph5
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

graph6 <- corrplot(corrs, method =  "color", tl.cex = 0.5, type = "upper")
```

![](../Reports/World_files/figure-gfmgraph6-1.png)<!-- -->

``` r
graph6
```

    ## $corr
    ##                                  timedelta n_tokens_title n_tokens_content n_unique_tokens n_non_stop_words n_non_stop_unique_tokens    num_hrefs
    ## timedelta                     1.0000000000   -0.240908068     -0.114045646    0.1794954759      0.064154434              0.146669686 -0.014256693
    ## n_tokens_title               -0.2409080678    1.000000000      0.058704759   -0.0586917501      0.002769819             -0.035750889 -0.012650404
    ## n_tokens_content             -0.1140456458    0.058704759      1.000000000   -0.3147462903      0.248126685             -0.157408848  0.391433760
    ## n_unique_tokens               0.1794954759   -0.058691750     -0.314746290    1.0000000000      0.738832902              0.952341847 -0.038521809
    ## n_non_stop_words              0.0641544342    0.002769819      0.248126685    0.7388329015      1.000000000              0.817032352  0.197240311
    ## n_non_stop_unique_tokens      0.1466696858   -0.035750889     -0.157408848    0.9523418467      0.817032352              1.000000000 -0.034435917
    ## num_hrefs                    -0.0142566929   -0.012650404      0.391433760   -0.0385218086      0.197240311             -0.034435917  1.000000000
    ## num_self_hrefs                0.0891465587    0.012314910      0.159883740    0.0556760946      0.172291258              0.084306171  0.248060123
    ## num_imgs                     -0.0656553410   -0.001876207      0.235510968   -0.3593601228     -0.226983311             -0.395468806  0.123680237
    ## num_videos                   -0.0576289677    0.022493381      0.059850144    0.0265228264      0.021946568              0.018709584  0.099199970
    ## average_token_length          0.0621714859   -0.032348213      0.226573923    0.7223420949      0.962059604              0.772462870  0.232367353
    ## num_keywords                  0.1477936372    0.027750758      0.091448392   -0.0634920981     -0.024184237             -0.043105774  0.095831298
    ## kw_min_min                    0.5730585557   -0.104053867     -0.094166093    0.1135853778      0.040671698              0.087313748 -0.047865638
    ## kw_max_min                    0.0418426514   -0.003625905      0.006051300   -0.0100714463     -0.012642735             -0.008128619  0.001053345
    ## kw_avg_min                    0.1515700343   -0.024141644     -0.006412405    0.0034692310     -0.008827217              0.003227035  0.001111639
    ## kw_min_max                   -0.1341109406    0.002051800     -0.007250130    0.0009253359     -0.001416263             -0.005487595 -0.032628161
    ## kw_max_max                   -0.6243036986    0.121026289      0.098169931   -0.1259910906     -0.047941492             -0.098414872  0.043413854
    ## kw_avg_max                   -0.4053192456    0.080690244     -0.013200409   -0.0067380064     -0.020469952             -0.022982643 -0.011848220
    ## kw_min_avg                   -0.0979210877   -0.004186981      0.007242702   -0.0094461005     -0.012264888             -0.020767922 -0.016618266
    ## kw_max_avg                   -0.0164753938    0.001863922     -0.006167435   -0.0172346835     -0.029855869             -0.021893940  0.008468581
    ## kw_avg_avg                   -0.0559944162   -0.005869247     -0.016448238   -0.0208607053     -0.052599035             -0.036478640  0.016285721
    ##                              num_self_hrefs      num_imgs    num_videos average_token_length num_keywords    kw_min_min   kw_max_min   kw_avg_min
    ## timedelta                      0.0891465587 -0.0656553410 -0.0576289677         0.0621714859  0.147793637  0.5730585557  0.041842651  0.151570034
    ## n_tokens_title                 0.0123149096 -0.0018762069  0.0224933811        -0.0323482133  0.027750758 -0.1040538672 -0.003625905 -0.024141644
    ## n_tokens_content               0.1598837397  0.2355109683  0.0598501442         0.2265739228  0.091448392 -0.0941660927  0.006051300 -0.006412405
    ## n_unique_tokens                0.0556760946 -0.3593601228  0.0265228264         0.7223420949 -0.063492098  0.1135853778 -0.010071446  0.003469231
    ## n_non_stop_words               0.1722912579 -0.2269833111  0.0219465676         0.9620596044 -0.024184237  0.0406716979 -0.012642735 -0.008827217
    ## n_non_stop_unique_tokens       0.0843061709 -0.3954688062  0.0187095836         0.7724628696 -0.043105774  0.0873137484 -0.008128619  0.003227035
    ## num_hrefs                      0.2480601231  0.1236802372  0.0991999697         0.2323673532  0.095831298 -0.0478656385  0.001053345  0.001111639
    ## num_self_hrefs                 1.0000000000  0.0774816550  0.0634627352         0.1741489076  0.118130210 -0.0241398942 -0.015612421 -0.017978733
    ## num_imgs                       0.0774816550  1.0000000000 -0.0312731739        -0.2170088571  0.059512689 -0.0689083675 -0.002574696 -0.010393554
    ## num_videos                     0.0634627352 -0.0312731739  1.0000000000         0.0026983757 -0.014375990 -0.0163861917 -0.017656498 -0.023088037
    ## average_token_length           0.1741489076 -0.2170088571  0.0026983757         1.0000000000 -0.028365967  0.0387970076 -0.011123099 -0.006839345
    ## num_keywords                   0.1181302101  0.0595126891 -0.0143759896        -0.0283659674  1.000000000 -0.0202226266  0.096219158  0.115967275
    ## kw_min_min                    -0.0241398942 -0.0689083675 -0.0163861917         0.0387970076 -0.020222627  1.0000000000  0.028964227  0.143795966
    ## kw_max_min                    -0.0156124209 -0.0025746958 -0.0176564979        -0.0111230992  0.096219158  0.0289642271  1.000000000  0.955513816
    ## kw_avg_min                    -0.0179787333 -0.0103935537 -0.0230880375        -0.0068393450  0.115967275  0.1437959656  0.955513816  1.000000000
    ## kw_min_max                    -0.0235997729  0.0383901589  0.0032373550        -0.0043948854 -0.384388251 -0.0707782885 -0.057577347 -0.108450351
    ## kw_max_max                     0.0228690380  0.0782312467  0.0283224292        -0.0467534479  0.024145975 -0.8654695637 -0.022164986 -0.140057212
    ## kw_avg_max                     0.0114160385  0.0339299660  0.0931206698        -0.0276780218 -0.387993156 -0.4650255063 -0.062160309 -0.178825203
    ## kw_min_avg                     0.0866157052  0.0329120582  0.0241200696        -0.0054368834 -0.335811437 -0.0936569104  0.001097335 -0.019166279
    ## kw_max_avg                    -0.0174718393  0.0548269476  0.0326670135        -0.0399298673  0.116290035 -0.0333122724  0.651755035  0.610611625
    ## kw_avg_avg                     0.0103580858  0.1048295003  0.0729880459        -0.0684718836 -0.004438681 -0.1080128941  0.464417603  0.439923212
    ##                                 kw_min_max   kw_max_max   kw_avg_max   kw_min_avg    kw_max_avg   kw_avg_avg self_reference_min_shares
    ## timedelta                    -0.1341109406 -0.624303699 -0.405319246 -0.097921088 -0.0164753938 -0.055994416              0.0006684678
    ## n_tokens_title                0.0020517999  0.121026289  0.080690244 -0.004186981  0.0018639220 -0.005869247              0.0209723993
    ## n_tokens_content             -0.0072501295  0.098169931 -0.013200409  0.007242702 -0.0061674350 -0.016448238             -0.0162656196
    ## n_unique_tokens               0.0009253359 -0.125991091 -0.006738006 -0.009446101 -0.0172346835 -0.020860705              0.0461744104
    ## n_non_stop_words             -0.0014162627 -0.047941492 -0.020469952 -0.012264888 -0.0298558689 -0.052599035              0.0308211482
    ## n_non_stop_unique_tokens     -0.0054875950 -0.098414872 -0.022982643 -0.020767922 -0.0218939398 -0.036478640              0.0477182371
    ## num_hrefs                    -0.0326281611  0.043413854 -0.011848220 -0.016618266  0.0084685813  0.016285721             -0.0174808115
    ## num_self_hrefs               -0.0235997729  0.022869038  0.011416039  0.086615705 -0.0174718393  0.010358086             -0.0297636138
    ## num_imgs                      0.0383901589  0.078231247  0.033929966  0.032912058  0.0548269476  0.104829500              0.0156884304
    ## num_videos                    0.0032373550  0.028322429  0.093120670  0.024120070  0.0326670135  0.072988046              0.0211966110
    ## average_token_length         -0.0043948854 -0.046753448 -0.027678022 -0.005436883 -0.0399298673 -0.068471884              0.0165912048
    ## num_keywords                 -0.3843882506  0.024145975 -0.387993156 -0.335811437  0.1162900352 -0.004438681             -0.0173484886
    ## kw_min_min                   -0.0707782885 -0.865469564 -0.465025506 -0.093656910 -0.0333122724 -0.108012894              0.0060827272
    ## kw_max_min                   -0.0575773469 -0.022164986 -0.062160309  0.001097335  0.6517550347  0.464417603              0.0014344083
    ## kw_avg_min                   -0.1084503506 -0.140057212 -0.178825203 -0.019166279  0.6106116249  0.439923212              0.0028193160
    ## kw_min_max                    1.0000000000  0.076747542  0.463711639  0.512434802  0.0131008571  0.225078542              0.0139061343
    ## kw_max_max                    0.0767475423  1.000000000  0.540558460  0.100616957  0.0580786846  0.150612804              0.0086789761
    ## kw_avg_max                    0.4637116389  0.540558460  1.000000000  0.383547029  0.0534533882  0.296628312              0.0381310688
    ## kw_min_avg                    0.5124348022  0.100616957  0.383547029  1.000000000  0.0306411195  0.391143065              0.0159933080
    ## kw_max_avg                    0.0131008571  0.058078685  0.053453388  0.030641120  1.0000000000  0.825084357              0.0365663804
    ## kw_avg_avg                    0.2250785419  0.150612804  0.296628312  0.391143065  0.8250843571  1.000000000              0.0596483838
    ##                              self_reference_max_shares self_reference_avg_sharess    is_weekend        LDA_00        LDA_01       LDA_02
    ## timedelta                                  0.021434370                0.012495429 -0.0671380337  0.0458353612 -0.0523553153  0.059650776
    ## n_tokens_title                             0.008854759                0.020398544  0.0623341973 -0.0188133528  0.0392045406 -0.043647511
    ## n_tokens_content                           0.012643067               -0.006229587  0.0343377401 -0.0041376507  0.0029105949  0.043435304
    ## n_unique_tokens                            0.038286426                0.050214758 -0.0247956740  0.0397599534  0.0336082978 -0.052971851
    ## n_non_stop_words                           0.038077106                0.039272574 -0.0120881991  0.0059923019  0.0186121161  0.006178470
    ## n_non_stop_unique_tokens                   0.042343364                0.051972333 -0.0324387925  0.0421836118  0.0404964914 -0.053943596
    ## num_hrefs                                  0.038511390                0.006091501  0.0404238622  0.0304063088 -0.0065701904 -0.026253549
    ## num_self_hrefs                             0.131716408                0.036568191  0.0090713803  0.0089297285 -0.0174348525  0.048219361
    ## num_imgs                                   0.051386843                0.033035070 -0.0068554841 -0.0231703468  0.0047212091 -0.058023498
    ## num_videos                                 0.066591000                0.043224383  0.0103281360 -0.0203783108  0.0121190212 -0.082116113
    ## average_token_length                       0.027610148                0.025275355 -0.0003144333 -0.0002110588  0.0007027216  0.044500186
    ## num_keywords                               0.020474610                0.001915638 -0.0143096686  0.0538202130 -0.0563248610 -0.158468771
    ## kw_min_min                                -0.003455697                0.003443160 -0.0395241378  0.0004403192  0.0076781169  0.025546092
    ## kw_max_min                                 0.022703077                0.016536865 -0.0098167006  0.0133373302  0.0074393746 -0.030936449
    ## kw_avg_min                                 0.024989571                0.018140659 -0.0144744368  0.0220865868  0.0131355593 -0.043944434
    ## kw_min_max                                 0.022179505                0.023462981  0.0381953031  0.0118603567  0.0109946177  0.021237912
    ## kw_max_max                                 0.015536611                0.014341469  0.0494656825  0.0092971445  0.0102096991 -0.040435963
    ## kw_avg_max                                 0.048517701                0.048684945  0.0251756064 -0.0036766054  0.0096244877  0.029834823
    ## kw_min_avg                                 0.052611459                0.038737286  0.0174574833 -0.0085060948 -0.0096095585  0.087326867
    ## kw_max_avg                                 0.079708326                0.067186343 -0.0136603849  0.0374144051  0.0382814603 -0.139603794
    ## kw_avg_avg                                 0.116393908                0.101319302 -0.0060350046  0.0589315147  0.0327551812 -0.207927701
    ##                                     LDA_03       LDA_04 global_subjectivity global_sentiment_polarity global_rate_positive_words
    ## timedelta                     0.0480539568 -0.108179341         0.103540705              0.1678960687                0.212277094
    ## n_tokens_title               -0.0141395136  0.055947797        -0.025864424             -0.0324897108               -0.050163551
    ## n_tokens_content             -0.1003166356  0.014286549         0.174509510              0.0165484069                0.133398360
    ## n_unique_tokens               0.0229724588  0.010774207         0.517916163              0.1644761724                0.307979537
    ## n_non_stop_words             -0.0743054389  0.029611697         0.668782341              0.1666113644                0.389047907
    ## n_non_stop_unique_tokens     -0.0205870604  0.036418055         0.586378544              0.1728402709                0.366335951
    ## num_hrefs                     0.0305964298 -0.002676527         0.171679694              0.0697894236                0.062663850
    ## num_self_hrefs               -0.0075743776 -0.051715933         0.071935074              0.0426026217                0.041754041
    ## num_imgs                      0.1624481874 -0.025331734        -0.165065751             -0.0001625056               -0.120476895
    ## num_videos                    0.1741912906 -0.008475167         0.054353119              0.0133536388                0.003741034
    ## average_token_length         -0.1008403597  0.012057827         0.609580810              0.1205453933                0.327681005
    ## num_keywords                  0.0359263717  0.168110572         0.052208501              0.1206210481                0.129802584
    ## kw_min_min                    0.0550469409 -0.072565747         0.059886391              0.0714376989                0.115911212
    ## kw_max_min                   -0.0023222116  0.028534259         0.002239524              0.0106851798                0.018858397
    ## kw_avg_min                   -0.0037699992  0.037757804         0.019666251              0.0213893668                0.045888903
    ## kw_min_max                    0.0646658647 -0.081996202        -0.018582775             -0.0171665339               -0.034925125
    ## kw_max_max                   -0.0622741339  0.081551206        -0.067758629             -0.0735830183               -0.128056539
    ## kw_avg_max                    0.1533667569 -0.142264336        -0.047504894             -0.0484004483               -0.109610814
    ## kw_min_avg                    0.0265466507 -0.116625604        -0.039861543             -0.0423854355               -0.046409378
    ## kw_max_avg                    0.1406170851  0.038723445         0.018002298              0.0336168049                0.021770971
    ## kw_avg_avg                    0.2654906010  0.029900858         0.015628560              0.0496071130                0.028879974
    ##                              global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity min_positive_polarity
    ## timedelta                                 -0.0069641304        0.1627738098        -0.118918745           0.081213284           0.045860115
    ## n_tokens_title                            -0.0072885654       -0.0142153447         0.019313476          -0.020087927          -0.016596136
    ## n_tokens_content                           0.1477687270        0.1222961469         0.123844448           0.159570064          -0.206256126
    ## n_unique_tokens                            0.2092537428        0.4753321514         0.240814607           0.466316011           0.394927130
    ## n_non_stop_words                           0.3296968218        0.5954853216         0.381030985           0.605973331           0.254197477
    ## n_non_stop_unique_tokens                   0.2730077173        0.5109125565         0.283254622           0.528642438           0.344531743
    ## num_hrefs                                  0.0208133655        0.1357367828         0.054113401           0.156641142          -0.099263819
    ## num_self_hrefs                            -0.0003027635        0.1218811731         0.043454818           0.109082424          -0.029042270
    ## num_imgs                                  -0.1375281348       -0.1117554321        -0.113429086          -0.116546315          -0.129668724
    ## num_videos                                 0.0208203910        0.0009565529         0.022301895           0.060170573           0.027864561
    ## average_token_length                       0.3134384396        0.5495957641         0.393385661           0.558637515           0.234979829
    ## num_keywords                              -0.0070965525        0.0746116644        -0.111656501           0.048246151          -0.051911679
    ## kw_min_min                                 0.0049750064        0.0844111612        -0.053775131           0.032668098           0.039212045
    ## kw_max_min                                 0.0049106923        0.0043150594        -0.018447629           0.002402896          -0.017681553
    ## kw_avg_min                                 0.0148045862        0.0139393029        -0.025455105           0.004788787          -0.022907993
    ## kw_min_max                                -0.0219072570       -0.0115009192         0.011725718          -0.011268468           0.002823071
    ## kw_max_max                                -0.0094324704       -0.0926326183         0.055484724          -0.032303346          -0.041038678
    ## kw_avg_max                                -0.0470661671       -0.0389978041         0.023052863          -0.022271657           0.018638466
    ## kw_min_avg                                -0.0145087432       -0.0324744562         0.024294882          -0.018040169           0.013657951
    ## kw_max_avg                                -0.0136746462        0.0076240594        -0.040611075           0.008103216          -0.024081180
    ## kw_avg_avg                                -0.0259970457        0.0020168326        -0.058410148           0.011555377          -0.015670695
    ##                              max_positive_polarity avg_negative_polarity min_negative_polarity max_negative_polarity title_subjectivity
    ## timedelta                             0.0430662650           0.038655515           0.110928112         -0.0484050283      -0.0213561795
    ## n_tokens_title                        0.0025170310          -0.028520346          -0.035072217         -0.0167585472       0.0575203289
    ## n_tokens_content                      0.4132757034          -0.148688751          -0.425477309          0.1819560790       0.0225105183
    ## n_unique_tokens                       0.2185430314          -0.269403381          -0.052472114         -0.3352996889      -0.0073358704
    ## n_non_stop_words                      0.5222830424          -0.401943745          -0.357973755         -0.2268729421      -0.0016707843
    ## n_non_stop_unique_tokens              0.3338402745          -0.320256953          -0.163776023         -0.2868066510      -0.0043334317
    ## num_hrefs                             0.2691600864          -0.149329204          -0.237143944          0.0325796053       0.0066906152
    ## num_self_hrefs                        0.1208634089          -0.083678892          -0.087312257         -0.0153862015      -0.0498143336
    ## num_imgs                             -0.0159042567           0.083663986           0.004842936          0.0940403202       0.0285596528
    ## num_videos                            0.0582286119          -0.039056331          -0.047860451         -0.0003170536       0.0593556446
    ## average_token_length                  0.4682913046          -0.379650236          -0.339602095         -0.2171165436      -0.0194017161
    ## num_keywords                          0.0798786700           0.048273009           0.008561095          0.0628593585       0.0307321112
    ## kw_min_min                            0.0116200463           0.008389265           0.064967121         -0.0447695588      -0.0019472720
    ## kw_max_min                            0.0032475092           0.010378976           0.010263036          0.0233671125       0.0175238368
    ## kw_avg_min                            0.0114612818           0.008948776           0.010423111          0.0210926927       0.0213880998
    ## kw_min_max                            0.0042566092          -0.012368095           0.001119292         -0.0181952617      -0.0069271399
    ## kw_max_max                           -0.0088633609          -0.009553876          -0.060855256          0.0453142470       0.0035509673
    ## kw_avg_max                           -0.0360143673          -0.036877370           0.002652512         -0.0383959379      -0.0011952552
    ## kw_min_avg                           -0.0078371472          -0.006995355          -0.002329884         -0.0125649773      -0.0174535949
    ## kw_max_avg                            0.0001328429           0.014670773           0.025676860          0.0230681911       0.0370096493
    ## kw_avg_avg                            0.0111145286           0.014990634           0.038929919          0.0061665091       0.0337444385
    ##                              title_sentiment_polarity abs_title_subjectivity abs_title_sentiment_polarity       shares
    ## timedelta                                0.0409482381           0.0295178627                 -0.010662286  0.003464820
    ## n_tokens_title                           0.0164418623          -0.1295934036                  0.042660612  0.043059656
    ## n_tokens_content                         0.0059762246          -0.0196262094                  0.010380843 -0.011860169
    ## n_unique_tokens                          0.0004658343          -0.0006723587                 -0.010707468 -0.009898121
    ## n_non_stop_words                        -0.0025108531          -0.0049825425                 -0.016071517 -0.027608398
    ## n_non_stop_unique_tokens                -0.0073901415          -0.0030983057                 -0.012235641 -0.017225666
    ## num_hrefs                                0.0233604507          -0.0062680029                  0.013708747  0.021564244
    ## num_self_hrefs                           0.0117991771           0.0196684469                 -0.030414521  0.007657342
    ## num_imgs                                 0.0385402044          -0.0143936310                  0.047552239  0.073436783
    ## num_videos                               0.0277346100          -0.0283356725                  0.055599007  0.033464448
    ## average_token_length                    -0.0113965488           0.0068582659                 -0.028632329 -0.050980834
    ## num_keywords                             0.0268540806          -0.0284615378                  0.012533488  0.022151514
    ## kw_min_min                               0.0523085772           0.0188169521                  0.002051031  0.004232318
    ## kw_max_min                               0.0275673026          -0.0036051582                  0.015888302  0.014939946
    ## kw_avg_min                               0.0347700281          -0.0013387037                  0.019522895  0.012095197
    ## kw_min_max                               0.0058059125          -0.0081316508                  0.007323547  0.002692157
    ## kw_max_max                              -0.0409757242          -0.0285212760                 -0.003399934  0.007648148
    ## kw_avg_max                              -0.0224117716          -0.0178831340                  0.007925339  0.013828890
    ## kw_min_avg                               0.0038027978          -0.0027990745                 -0.004748294 -0.008896838
    ## kw_max_avg                               0.0434664422          -0.0172186972                  0.043744349  0.043295210
    ## kw_avg_avg                               0.0436070593          -0.0261404208                  0.053124783  0.076666592
    ##  [ reached getOption("max.print") -- omitted 26 rows ]
    ## 
    ## $corrPos
    ##                        xName                    yName  x  y          corr
    ## 1                  timedelta                timedelta  1 47  1.0000000000
    ## 2             n_tokens_title                timedelta  2 47 -0.2409080678
    ## 3             n_tokens_title           n_tokens_title  2 46  1.0000000000
    ## 4           n_tokens_content                timedelta  3 47 -0.1140456458
    ## 5           n_tokens_content           n_tokens_title  3 46  0.0587047585
    ## 6           n_tokens_content         n_tokens_content  3 45  1.0000000000
    ## 7            n_unique_tokens                timedelta  4 47  0.1794954759
    ## 8            n_unique_tokens           n_tokens_title  4 46 -0.0586917501
    ## 9            n_unique_tokens         n_tokens_content  4 45 -0.3147462903
    ## 10           n_unique_tokens          n_unique_tokens  4 44  1.0000000000
    ## 11          n_non_stop_words                timedelta  5 47  0.0641544342
    ## 12          n_non_stop_words           n_tokens_title  5 46  0.0027698194
    ## 13          n_non_stop_words         n_tokens_content  5 45  0.2481266849
    ## 14          n_non_stop_words          n_unique_tokens  5 44  0.7388329015
    ## 15          n_non_stop_words         n_non_stop_words  5 43  1.0000000000
    ## 16  n_non_stop_unique_tokens                timedelta  6 47  0.1466696858
    ## 17  n_non_stop_unique_tokens           n_tokens_title  6 46 -0.0357508886
    ## 18  n_non_stop_unique_tokens         n_tokens_content  6 45 -0.1574088483
    ## 19  n_non_stop_unique_tokens          n_unique_tokens  6 44  0.9523418467
    ## 20  n_non_stop_unique_tokens         n_non_stop_words  6 43  0.8170323522
    ## 21  n_non_stop_unique_tokens n_non_stop_unique_tokens  6 42  1.0000000000
    ## 22                 num_hrefs                timedelta  7 47 -0.0142566929
    ## 23                 num_hrefs           n_tokens_title  7 46 -0.0126504040
    ## 24                 num_hrefs         n_tokens_content  7 45  0.3914337602
    ## 25                 num_hrefs          n_unique_tokens  7 44 -0.0385218086
    ## 26                 num_hrefs         n_non_stop_words  7 43  0.1972403114
    ## 27                 num_hrefs n_non_stop_unique_tokens  7 42 -0.0344359168
    ## 28                 num_hrefs                num_hrefs  7 41  1.0000000000
    ## 29            num_self_hrefs                timedelta  8 47  0.0891465587
    ## 30            num_self_hrefs           n_tokens_title  8 46  0.0123149096
    ## 31            num_self_hrefs         n_tokens_content  8 45  0.1598837397
    ## 32            num_self_hrefs          n_unique_tokens  8 44  0.0556760946
    ## 33            num_self_hrefs         n_non_stop_words  8 43  0.1722912579
    ## 34            num_self_hrefs n_non_stop_unique_tokens  8 42  0.0843061709
    ## 35            num_self_hrefs                num_hrefs  8 41  0.2480601231
    ## 36            num_self_hrefs           num_self_hrefs  8 40  1.0000000000
    ## 37                  num_imgs                timedelta  9 47 -0.0656553410
    ## 38                  num_imgs           n_tokens_title  9 46 -0.0018762069
    ## 39                  num_imgs         n_tokens_content  9 45  0.2355109683
    ## 40                  num_imgs          n_unique_tokens  9 44 -0.3593601228
    ## 41                  num_imgs         n_non_stop_words  9 43 -0.2269833111
    ## 42                  num_imgs n_non_stop_unique_tokens  9 42 -0.3954688062
    ## 43                  num_imgs                num_hrefs  9 41  0.1236802372
    ## 44                  num_imgs           num_self_hrefs  9 40  0.0774816550
    ## 45                  num_imgs                 num_imgs  9 39  1.0000000000
    ## 46                num_videos                timedelta 10 47 -0.0576289677
    ## 47                num_videos           n_tokens_title 10 46  0.0224933811
    ## 48                num_videos         n_tokens_content 10 45  0.0598501442
    ## 49                num_videos          n_unique_tokens 10 44  0.0265228264
    ## 50                num_videos         n_non_stop_words 10 43  0.0219465676
    ## 51                num_videos n_non_stop_unique_tokens 10 42  0.0187095836
    ## 52                num_videos                num_hrefs 10 41  0.0991999697
    ## 53                num_videos           num_self_hrefs 10 40  0.0634627352
    ## 54                num_videos                 num_imgs 10 39 -0.0312731739
    ## 55                num_videos               num_videos 10 38  1.0000000000
    ## 56      average_token_length                timedelta 11 47  0.0621714859
    ## 57      average_token_length           n_tokens_title 11 46 -0.0323482133
    ## 58      average_token_length         n_tokens_content 11 45  0.2265739228
    ## 59      average_token_length          n_unique_tokens 11 44  0.7223420949
    ## 60      average_token_length         n_non_stop_words 11 43  0.9620596044
    ## 61      average_token_length n_non_stop_unique_tokens 11 42  0.7724628696
    ## 62      average_token_length                num_hrefs 11 41  0.2323673532
    ## 63      average_token_length           num_self_hrefs 11 40  0.1741489076
    ## 64      average_token_length                 num_imgs 11 39 -0.2170088571
    ## 65      average_token_length               num_videos 11 38  0.0026983757
    ## 66      average_token_length     average_token_length 11 37  1.0000000000
    ## 67              num_keywords                timedelta 12 47  0.1477936372
    ## 68              num_keywords           n_tokens_title 12 46  0.0277507579
    ## 69              num_keywords         n_tokens_content 12 45  0.0914483925
    ## 70              num_keywords          n_unique_tokens 12 44 -0.0634920981
    ## 71              num_keywords         n_non_stop_words 12 43 -0.0241842369
    ## 72              num_keywords n_non_stop_unique_tokens 12 42 -0.0431057738
    ## 73              num_keywords                num_hrefs 12 41  0.0958312983
    ## 74              num_keywords           num_self_hrefs 12 40  0.1181302101
    ## 75              num_keywords                 num_imgs 12 39  0.0595126891
    ## 76              num_keywords               num_videos 12 38 -0.0143759896
    ## 77              num_keywords     average_token_length 12 37 -0.0283659674
    ## 78              num_keywords             num_keywords 12 36  1.0000000000
    ## 79                kw_min_min                timedelta 13 47  0.5730585557
    ## 80                kw_min_min           n_tokens_title 13 46 -0.1040538672
    ## 81                kw_min_min         n_tokens_content 13 45 -0.0941660927
    ## 82                kw_min_min          n_unique_tokens 13 44  0.1135853778
    ## 83                kw_min_min         n_non_stop_words 13 43  0.0406716979
    ## 84                kw_min_min n_non_stop_unique_tokens 13 42  0.0873137484
    ## 85                kw_min_min                num_hrefs 13 41 -0.0478656385
    ## 86                kw_min_min           num_self_hrefs 13 40 -0.0241398942
    ## 87                kw_min_min                 num_imgs 13 39 -0.0689083675
    ## 88                kw_min_min               num_videos 13 38 -0.0163861917
    ## 89                kw_min_min     average_token_length 13 37  0.0387970076
    ## 90                kw_min_min             num_keywords 13 36 -0.0202226266
    ## 91                kw_min_min               kw_min_min 13 35  1.0000000000
    ## 92                kw_max_min                timedelta 14 47  0.0418426514
    ## 93                kw_max_min           n_tokens_title 14 46 -0.0036259049
    ## 94                kw_max_min         n_tokens_content 14 45  0.0060512998
    ## 95                kw_max_min          n_unique_tokens 14 44 -0.0100714463
    ## 96                kw_max_min         n_non_stop_words 14 43 -0.0126427354
    ## 97                kw_max_min n_non_stop_unique_tokens 14 42 -0.0081286192
    ## 98                kw_max_min                num_hrefs 14 41  0.0010533453
    ## 99                kw_max_min           num_self_hrefs 14 40 -0.0156124209
    ## 100               kw_max_min                 num_imgs 14 39 -0.0025746958
    ## 101               kw_max_min               num_videos 14 38 -0.0176564979
    ## 102               kw_max_min     average_token_length 14 37 -0.0111230992
    ## 103               kw_max_min             num_keywords 14 36  0.0962191582
    ## 104               kw_max_min               kw_min_min 14 35  0.0289642271
    ## 105               kw_max_min               kw_max_min 14 34  1.0000000000
    ## 106               kw_avg_min                timedelta 15 47  0.1515700343
    ## 107               kw_avg_min           n_tokens_title 15 46 -0.0241416444
    ## 108               kw_avg_min         n_tokens_content 15 45 -0.0064124054
    ## 109               kw_avg_min          n_unique_tokens 15 44  0.0034692310
    ## 110               kw_avg_min         n_non_stop_words 15 43 -0.0088272166
    ## 111               kw_avg_min n_non_stop_unique_tokens 15 42  0.0032270350
    ## 112               kw_avg_min                num_hrefs 15 41  0.0011116389
    ## 113               kw_avg_min           num_self_hrefs 15 40 -0.0179787333
    ## 114               kw_avg_min                 num_imgs 15 39 -0.0103935537
    ## 115               kw_avg_min               num_videos 15 38 -0.0230880375
    ## 116               kw_avg_min     average_token_length 15 37 -0.0068393450
    ## 117               kw_avg_min             num_keywords 15 36  0.1159672753
    ## 118               kw_avg_min               kw_min_min 15 35  0.1437959656
    ## 119               kw_avg_min               kw_max_min 15 34  0.9555138158
    ## 120               kw_avg_min               kw_avg_min 15 33  1.0000000000
    ## 121               kw_min_max                timedelta 16 47 -0.1341109406
    ## 122               kw_min_max           n_tokens_title 16 46  0.0020517999
    ## 123               kw_min_max         n_tokens_content 16 45 -0.0072501295
    ## 124               kw_min_max          n_unique_tokens 16 44  0.0009253359
    ## 125               kw_min_max         n_non_stop_words 16 43 -0.0014162627
    ## 126               kw_min_max n_non_stop_unique_tokens 16 42 -0.0054875950
    ## 127               kw_min_max                num_hrefs 16 41 -0.0326281611
    ## 128               kw_min_max           num_self_hrefs 16 40 -0.0235997729
    ## 129               kw_min_max                 num_imgs 16 39  0.0383901589
    ## 130               kw_min_max               num_videos 16 38  0.0032373550
    ## 131               kw_min_max     average_token_length 16 37 -0.0043948854
    ## 132               kw_min_max             num_keywords 16 36 -0.3843882506
    ## 133               kw_min_max               kw_min_min 16 35 -0.0707782885
    ## 134               kw_min_max               kw_max_min 16 34 -0.0575773469
    ## 135               kw_min_max               kw_avg_min 16 33 -0.1084503506
    ## 136               kw_min_max               kw_min_max 16 32  1.0000000000
    ## 137               kw_max_max                timedelta 17 47 -0.6243036986
    ## 138               kw_max_max           n_tokens_title 17 46  0.1210262886
    ## 139               kw_max_max         n_tokens_content 17 45  0.0981699307
    ## 140               kw_max_max          n_unique_tokens 17 44 -0.1259910906
    ## 141               kw_max_max         n_non_stop_words 17 43 -0.0479414924
    ## 142               kw_max_max n_non_stop_unique_tokens 17 42 -0.0984148722
    ## 143               kw_max_max                num_hrefs 17 41  0.0434138538
    ## 144               kw_max_max           num_self_hrefs 17 40  0.0228690380
    ## 145               kw_max_max                 num_imgs 17 39  0.0782312467
    ## 146               kw_max_max               num_videos 17 38  0.0283224292
    ## 147               kw_max_max     average_token_length 17 37 -0.0467534479
    ## 148               kw_max_max             num_keywords 17 36  0.0241459746
    ## 149               kw_max_max               kw_min_min 17 35 -0.8654695637
    ## 150               kw_max_max               kw_max_min 17 34 -0.0221649856
    ## 151               kw_max_max               kw_avg_min 17 33 -0.1400572121
    ## 152               kw_max_max               kw_min_max 17 32  0.0767475423
    ## 153               kw_max_max               kw_max_max 17 31  1.0000000000
    ## 154               kw_avg_max                timedelta 18 47 -0.4053192456
    ## 155               kw_avg_max           n_tokens_title 18 46  0.0806902442
    ## 156               kw_avg_max         n_tokens_content 18 45 -0.0132004094
    ## 157               kw_avg_max          n_unique_tokens 18 44 -0.0067380064
    ## 158               kw_avg_max         n_non_stop_words 18 43 -0.0204699524
    ## 159               kw_avg_max n_non_stop_unique_tokens 18 42 -0.0229826426
    ## 160               kw_avg_max                num_hrefs 18 41 -0.0118482199
    ## 161               kw_avg_max           num_self_hrefs 18 40  0.0114160385
    ## 162               kw_avg_max                 num_imgs 18 39  0.0339299660
    ## 163               kw_avg_max               num_videos 18 38  0.0931206698
    ## 164               kw_avg_max     average_token_length 18 37 -0.0276780218
    ## 165               kw_avg_max             num_keywords 18 36 -0.3879931557
    ## 166               kw_avg_max               kw_min_min 18 35 -0.4650255063
    ## 167               kw_avg_max               kw_max_min 18 34 -0.0621603088
    ## 168               kw_avg_max               kw_avg_min 18 33 -0.1788252031
    ## 169               kw_avg_max               kw_min_max 18 32  0.4637116389
    ## 170               kw_avg_max               kw_max_max 18 31  0.5405584596
    ## 171               kw_avg_max               kw_avg_max 18 30  1.0000000000
    ## 172               kw_min_avg                timedelta 19 47 -0.0979210877
    ## 173               kw_min_avg           n_tokens_title 19 46 -0.0041869807
    ## 174               kw_min_avg         n_tokens_content 19 45  0.0072427024
    ## 175               kw_min_avg          n_unique_tokens 19 44 -0.0094461005
    ## 176               kw_min_avg         n_non_stop_words 19 43 -0.0122648884
    ## 177               kw_min_avg n_non_stop_unique_tokens 19 42 -0.0207679225
    ## 178               kw_min_avg                num_hrefs 19 41 -0.0166182659
    ## 179               kw_min_avg           num_self_hrefs 19 40  0.0866157052
    ## 180               kw_min_avg                 num_imgs 19 39  0.0329120582
    ## 181               kw_min_avg               num_videos 19 38  0.0241200696
    ## 182               kw_min_avg     average_token_length 19 37 -0.0054368834
    ## 183               kw_min_avg             num_keywords 19 36 -0.3358114369
    ## 184               kw_min_avg               kw_min_min 19 35 -0.0936569104
    ## 185               kw_min_avg               kw_max_min 19 34  0.0010973353
    ## 186               kw_min_avg               kw_avg_min 19 33 -0.0191662789
    ## 187               kw_min_avg               kw_min_max 19 32  0.5124348022
    ## 188               kw_min_avg               kw_max_max 19 31  0.1006169565
    ## 189               kw_min_avg               kw_avg_max 19 30  0.3835470288
    ## 190               kw_min_avg               kw_min_avg 19 29  1.0000000000
    ## 191               kw_max_avg                timedelta 20 47 -0.0164753938
    ## 192               kw_max_avg           n_tokens_title 20 46  0.0018639220
    ## 193               kw_max_avg         n_tokens_content 20 45 -0.0061674350
    ## 194               kw_max_avg          n_unique_tokens 20 44 -0.0172346835
    ## 195               kw_max_avg         n_non_stop_words 20 43 -0.0298558689
    ## 196               kw_max_avg n_non_stop_unique_tokens 20 42 -0.0218939398
    ## 197               kw_max_avg                num_hrefs 20 41  0.0084685813
    ## 198               kw_max_avg           num_self_hrefs 20 40 -0.0174718393
    ## 199               kw_max_avg                 num_imgs 20 39  0.0548269476
    ## 200               kw_max_avg               num_videos 20 38  0.0326670135
    ##  [ reached 'max' / getOption("max.print") -- omitted 928 rows ]
    ## 
    ## $arg
    ## $arg$type
    ## [1] "upper"

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
numSum <- newsTrain %>% 
  group_by(weekday) %>% 
  summarise(Min = min(shares), Q1 = quantile(shares, 0.25), Mean = mean(shares), 
            Median = median(shares), Q3 = quantile(shares, 0.75), Max = max(shares), SD = sd(shares))

knitr::kable(numSum)
```

| weekday   | Min |     Q1 |     Mean | Median |   Q3 |    Max |        SD |
|:----------|----:|-------:|---------:|-------:|-----:|-------:|----------:|
| Monday    | 111 |  835.0 | 2441.296 |   1100 | 1800 | 141400 |  7465.764 |
| Tuesday   |  42 |  761.0 | 2395.499 |   1100 | 1700 | 115700 |  6535.872 |
| Wednesday |  48 |  785.0 | 1862.439 |   1100 | 1700 |  49800 |  2922.421 |
| Thursday  |  42 |  787.5 | 2590.975 |   1100 | 1800 | 284700 | 10019.067 |
| Friday    |  35 |  853.0 | 2162.653 |   1100 | 1800 | 128500 |  5431.550 |
| Saturday  |  43 | 1000.0 | 2434.490 |   1500 | 2600 |  25200 |  3009.766 |
| Sunday    |  91 | 1100.0 | 2634.593 |   1400 | 2300 |  55600 |  4807.300 |

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
# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)
# 
# trainglmfit <- train(shares ~ num_imgs + kw_min_avg + kw_max_avg + kw_avg_avg, 
#                      data = newsTrain, method = "lm", 
#                      trControl = trainControl(method = "cv", number = 10), 
#                      preProcess = c("center", "scale"))
# 
# pred1 <- predict(trainglmfit, newdata = newsTest)
# 
# lm1Results <- postResample(pred1, obs = newsTest$shares)
# 
# stopCluster(cl)
```

### Linear Model 2

The next linear model uses the variables `num_imgs`, `kw_avg_avg`,
`LDA_02`, `LDA_03`, `average_token_length`, and `rate_negative_words`.
Similar to the previous model, the values are centered and scaled and
this uses 10-fold cross validation on the training set.

``` r
# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)
# 
# mlrFit <- train(shares ~  num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, 
#                 data = newsTrain, method = "lm", 
#                 trControl = trainControl(method = "cv", number = 10), 
#                 preProcess = c("center", "scale"))
# 
# mlrPred <- predict(mlrFit, newsTest)
# 
# mlrResults <- postResample(mlrPred, obs = newsTest$shares)
# 
# stopCluster(cl)
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
# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)
# 
# # random forest model with preprocessed data that is centered and scaled
# # use 5-fold cross validation
# # mtry from 1:7
# rfFit <- train(shares ~ num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, 
#                data = newsTrain, method = "rf", 
#                preProcess = c("center", "scale"), 
#                trControl = trainControl(method = "cv", number = 5), 
#                tuneGrid = expand.grid(mtry = 1:7))
# 
# rfPred <- predict(rfFit, newsTest)
# 
# rfResults <- postResample(rfPred, obs = newsTest$shares)
# 
# stopCluster(cl)
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
# cb <- makePSOCKcluster(5)
# registerDoParallel(cb)
# 
# set.seed(30)
# wtFit <- train(shares ~ num_imgs + kw_avg_avg + LDA_02 + LDA_03 + average_token_length + rate_negative_words, 
#                 data = newsTrain, method = "gbm", 
#                 preProcess = c("center", "scale"), 
#                 trControl = trainControl(method = "cv", number = 5), 
#                 tuneGrid = expand.grid(.n.trees = seq(25, 200, by = 25), 
#                                        .interaction.depth = seq(1, 4, by = 1), 
#                                        .shrinkage = (0.1), 
#                                        .n.minobsinnode = (10)))
# 
# wtPred <- predict(wtFit, newsTest)
# 
# wtResults <- postResample(wtPred, obs = newsTest$shares)
# 
# stopCluster(cb)
```

The optimal tuning parameters for the boosted tree were 75 trees and an
interaction depth of 1, along with the default values for `shrinkage`
and `n.minobsinnode`.

# Comparison

``` r
# results <- rbind(t(lm1Results), t(mlrResults), t(rfResults), t(wtResults))
# models <- c("Linear Model 1", "Linear Model 2", "Random Forest", "Boosted Trees")
# 
# results <- data.frame(results, row.names = models)
# 
# bestModel <- results %>% mutate(model = models) %>% filter(RMSE == min(RMSE))
# 
# knitr::kable(results)
```

The best model out of the 4 that were tested was Linear Model 2 with an
RMSE of 4599.8193812.
