Sentiment Analysis on Amazon
================
Catherine Chen
4/11/2020

## Purpose: Analyze the correlation between Amazon ratings for electronic product Transcend and its customer reviews

## Setup

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(magrittr)
library(rvest)
library(readr)
```

    ## 
    ## Attaching package: 'readr'

    ## The following object is masked from 'package:rvest':
    ## 
    ##     guess_encoding

``` r
library(tidytext)
library(ggplot2)
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` r
library(RColorBrewer)
library(textdata)
```

## I. Identify competitors in the Amazon SD marketplace

``` r
#1 Read File 
electronics<- read_csv("electronics_downsample.csv")
```

    ## New names:
    ## * `` -> ...1

    ## Rows: 65809 Columns: 10

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): asin, helpful, reviewText, reviewTime, reviewerID, reviewerName, su...
    ## dbl (3): ...1, overall, unixReviewTime

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
electronics
```

    ## # A tibble: 65,809 × 10
    ##      ...1 asin       helpful overall reviewText reviewTime reviewerID reviewerName
    ##     <dbl> <chr>      <chr>     <dbl> <chr>      <chr>      <chr>      <chr>       
    ##  1 175426 B000BQ7GW8 [0, 0]        5 Was deliv… 04 27, 20… AYK7KLKHF… "2009BC \"b…
    ##  2 175427 B000BQ7GW8 [0, 0]        5 More than… 09 28, 20… A28OOZEJ1… "2hawk feat…
    ##  3 175428 B000BQ7GW8 [0, 0]        5 Nice pric… 03 12, 20… A2W8HPGBR… "39ejhsosjd…
    ##  4 175429 B000BQ7GW8 [0, 0]        5 This card… 04 17, 20… A2OMYXNS1… "aadavidall…
    ##  5 175430 B000BQ7GW8 [0, 0]        4 I have ha… 05 8, 2014 A3O1Y8Y31… "Aaron"     
    ##  6 175431 B000BQ7GW8 [0, 1]        4 I made th… 05 6, 2013 A1OXQAY95… "Aaron Pier…
    ##  7 175432 B000BQ7GW8 [0, 0]        5 Using thi… 10 29, 20… A55OWI53K… "Abby507vc" 
    ##  8 175434 B000BQ7GW8 [0, 0]        5 ThisSanDi… 12 6, 2012 A2TN0U817… "acanal"    
    ##  9 175435 B000BQ7GW8 [0, 0]        5 I've neve… 12 20, 20… A1IMRWGBX… "A. Carman" 
    ## 10 175436 B000BQ7GW8 [0, 1]        5 At first,… 01 27, 20… A3R3MDH0H… "A Customer"
    ## # … with 65,799 more rows, and 2 more variables: summary <chr>,
    ## #   unixReviewTime <dbl>

``` r
#2 
electronics %>% filter(str_detect(electronics$reviewText, "\\bsd\\b|\\bSD\\b")=="TRUE") %>% group_by(asin) %>% count() %>% arrange(desc(n))
```

    ## # A tibble: 965 × 2
    ## # Groups:   asin [965]
    ##    asin           n
    ##    <chr>      <int>
    ##  1 B007WTAJTO   576
    ##  2 B002WE6D44   214
    ##  3 B000VX6XL6   192
    ##  4 B000BQ7GW8   166
    ##  5 B002MAPRYU   145
    ##  6 B004G6002M   126
    ##  7 B007P4VOWC    84
    ##  8 B000QUUFRW     9
    ##  9 B001L1H0SC     7
    ## 10 B0046TJG1U     7
    ## # … with 955 more rows

### B007WTAJTO: SanDisk

### B002WE6D44: Transcend

### B000VX6XL6: Kingston

## II. EDA

``` r
# Ratings
elec_stars= electronics %>%  group_by(asin) %>% filter(asin == "B007WTAJTO" | asin == "B002WE6D44" | asin == "B000VX6XL6") %>% summarise(n= round(mean(overall),2)) %>% arrange(desc(n)) %>% cbind(c("Transcend" ,"Kingston","SanDisk"))%>% set_colnames(c("ASIN","Average Stars","Producers")) 
print(elec_stars)
```

    ##         ASIN Average Stars Producers
    ## 1 B002WE6D44          4.53 Transcend
    ## 2 B000VX6XL6          4.51  Kingston
    ## 3 B007WTAJTO          4.48   SanDisk

``` r
# Sentiment scores
elec_sent = electronics %>% select(asin,reviewText) %>% unnest_tokens(word,reviewText) %>% inner_join(get_sentiments("afinn")) %>% filter(asin == "B007WTAJTO" | asin == "B002WE6D44" | asin == "B000VX6XL6") %>% group_by(asin) %>% summarise(emotion = round(mean(value),4) )
```

    ## Joining, by = "word"

``` r
elec_sent %>% cbind(c("SanDisk","Transcend","Kingston")) %>% set_colnames(c("ASIN","Average Emotion Score","Producers"))
```

    ##         ASIN Average Emotion Score Producers
    ## 1 B000VX6XL6                0.9643   SanDisk
    ## 2 B002WE6D44                0.8812 Transcend
    ## 3 B007WTAJTO                0.8284  Kingston

``` r
elec_sent %>% ggplot(aes(x=asin,y=emotion,fill=asin)) + geom_bar(stat = "identity") + geom_text(aes(label = emotion),vjust=1.6)
```

![](Lab-5--Transcend_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## III. Text exploration with wordclouds

``` r
#1. clean data with VCorpus, lowercase, punctuation, numbers, stopword, whitespace
top3= electronics %>% filter(asin == "B007WTAJTO" | asin == "B002WE6D44" | asin == "B000VX6XL6")
elec_corp = VCorpus(VectorSource(top3$reviewText))
elec_corp= elec_corp %>% tm_map(removePunctuation)
elec_corp = tm_map(elec_corp, content_transformer(tolower), lazy =T)
elec_corp = tm_map(elec_corp, content_transformer(removeWords),stopwords(c("english")))
elec_corp = tm_map(elec_corp, content_transformer(stemDocument), lazy=TRUE) 
elec_corp = tm_map(elec_corp,stripWhitespace)

#2 Doc-term matrix, RemoveSparse, correlate
elec_dtm = DocumentTermMatrix(elec_corp)
elec_dtms = removeSparseTerms(elec_dtm, sparse = 0.98)
elec_dtmmatrix= as.matrix(elec_dtms)
elec_rating = top3$overall
elec_corr = cor(elec_rating,elec_dtmmatrix)

#top 30 positively and negatively correlated with ratings
pos30 = order(elec_corr,decreasing = T)[1:30]
pos30w = colnames(elec_corr)[pos30]
neg30 = order(elec_corr,decreasing = F)[1:30]
neg30w = colnames(elec_corr)[neg30]

#Wordcloud 1
wcpos30 <- wordcloud(pos30w,freq = elec_corr[1,pos30],random.color = T)
```

![](Lab-5--Transcend_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#\#Create Wordcloud 2

``` r
#Wordcloud 2
wcneg30 <- wordcloud(neg30w,freq = abs(elec_corr[1,neg30]))
```

![](Lab-5--Transcend_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#m= as.matrix(elec_dtm)
#word.freq= colSums(m) 
#word.freq= sort(word.freq,decreasing = T) 
#word.freq[1:10]
```
