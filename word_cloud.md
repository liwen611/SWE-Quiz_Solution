Word Cloud
================
Liwen Huang
8/25/2018

Import some library

``` r
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     compact

``` r
library(wordcloud)
```

    ## Loading required package: RColorBrewer

You want to build a word cloud, an infographic where the size of a word corresponds to how often it appears in the body of text. To do this, you'll need data. Write code that takes a long string and builds its word cloud data in a dictionary ↴ , where the keys are words and the values are the number of times the words occurred.

Think about capitalized words. For example, look at these sentences:

``` r
sentence1="After beating the eggs, Dana read the next step:"
sentence2="Add milk and eggs, then add flour and sugar."
```

First of all, I have written a function that counts words. This function returns a matrix with two columns, the first one indicate the word, the second the number of times it appears in the sentence

``` r
countword<-function(sentence){
 word_count=sentence%>%
  tolower()%>%gsub('[[:punct:] ]+',' ',.)%>%
  noquote()%>%strsplit(split=' ')%>%unlist%>%
  table()%>%as.matrix()
 return (word_count)
}
```

test the function

``` r
t1=countword(sentence1)
t2=countword(sentence2)
t1
```

    ##         [,1]
    ## after      1
    ## beating    1
    ## dana       1
    ## eggs       1
    ## next       1
    ## read       1
    ## step       1
    ## the        2

``` r
t2
```

    ##       [,1]
    ## add      2
    ## and      2
    ## eggs     1
    ## flour    1
    ## milk     1
    ## sugar    1
    ## then     1

Then use a function to aggregate the total word counts

``` r
total_wordcount<-function(count1, count2){
  total=rbind(count1, count2)
  total=data.frame(rownames(total), total)
  aggregate(. ~ rownames.total., data=total, FUN=sum)  
}
```

test the function

``` r
t3=total_wordcount(t1, t2)
t3
```

    ##    rownames.total. total
    ## 1              add     2
    ## 2            after     1
    ## 3              and     2
    ## 4          beating     1
    ## 5             dana     1
    ## 6             eggs     2
    ## 7            flour     1
    ## 8             milk     1
    ## 9             next     1
    ## 10            read     1
    ## 11            step     1
    ## 12           sugar     1
    ## 13             the     2
    ## 14            then     1

Now I can make my little word cloud

``` r
library(wordcloud)
library(RColorBrewer)

t3_cloud=wordcloud(words=t3$rownames.total., freq=t3$total, min.freq = 1,
         max.words=200, random.order=FALSE, rot.per=0, 
         colors=brewer.pal(8, "Dark2"))
```

![](word_cloud_files/figure-markdown_github/unnamed-chunk-7-1.png)
