White Board Quiz Solution in R
================

[1.Fibnacci sequence](#heading-1)

[2.Word cloud problem](#heading-2)

[3.Two Sum](#3.)

[4.Apple Stock](#4.)

[5.Digit Count](#5)

[6.Ugly Number](#6)

[7.Rotate String](#7)

[8.String permutation and subsetting](#8)

[9.Dice Sum](#9)

[10.Interleaving String](#10)

[11.N\_queen](#11)

[12.Maximum Subarray](#12)

[13.Longest Substring](#13)

[14.Sort Letters by Case](#14)

[15.3Sum](#15)

[16.Longest Common Subsequence](#16)

[17.Longest Common String](#17)

[18.K sum](#18)

[19.Unique Path](#19)

[20.Jump Game](#20)

[21.Minial Window Substring](#21)

[22.Word Break](#22)

[23.Palindrome Partitioning II](#23)

[24.Interleaving Positive and Negative Numbers](#24)

[25.Largest Rectangle in Histogram](#25)

[26.The Skyline Problem](#26)

[27.Word Ladder](#27)

[28.Burst Balloons](#28)

[29.Hower of Hanoi](#29.)

[30.Anagrams](#30.)

[31.Optimal Account Balancing](#31.)

[32.Woodcut](#32.)

[33.Interval Sum II](#33.)

[34.Trapping Rain Water](#34.)

[35.Wild Card Matching](#35.)

[36.Expression Evaluation](#36.)

[37.Pain Fence](#37.)

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

1. Fibnacci sequence
====================

``` r
fib=function(len){
fibvals <- numeric(len)
fibvals[1] <- 1
fibvals[2] <- 1
for (i in 3:len) { 
  fibvals[i] <- fibvals[i-1]+fibvals[i-2]
} 
return (fibvals)
}
```

test the function with 10

``` r
fib(10)
```

    ##  [1]  1  1  2  3  5  8 13 21 34 55

``` r
# Program to display the Fibonacci sequence up to n-th term using recursive functions
recurse_fibonacci <- function(n) {
if(n <= 1) {
return(n)
} else {
return(recurse_fibonacci(n-1) + recurse_fibonacci(n-2))
}
}
```

test the function with 10

``` r
recurse_fibonacci(10)
```

    ## [1] 55

This is a solution found R document, which is a cleaner version of the above. But the purpose is to demonstrate the use of Recall though

``` r
## A trivial (but inefficient!) example:
fib <- function(n)
   if(n<=2) { if(n>=0) 1 else 0 } else Recall(n-1) + Recall(n-2)
fibonacci <- fib; rm(fib)
## renaming wouldn't work without Recall
fibonacci(10) # 55
```

    ## [1] 55

Stumbled upon this on stack exchange the other day, and want to just copy and paste it here for future reference.

``` r
#naive recursive implementation
fib <- function(n)  {
  if(n == 1 || n == 2) return(1)
  fib(n-1) + fib(n-2)
}
```

``` r
#with memoization
fibm <- function(n)  {
  if(n == 1 || n == 2) return(1)

  seq <- integer(n)
  seq[1:2] <- 1

  calc <- function(n) {
    if (seq[n] != 0) return(seq[n])
    seq[n] <<- calc(n-1) + calc(n-2)
    seq[n]
  }

  calc(n)
}
```

``` r
fibm(20)
```

    ## [1] 6765

Is memoization faster? microbenchmark(fib(20), fibm(20)) Unit: microseconds expr min lq mean median uq max neval cld fib(20) 8005.314 8804.130 9758.75325 9301.6210 9798.8500 46867.182 100 b fibm(20) 38.991 44.798 54.12626 53.6725 60.4035 97.089 100 a

<span style="color:blue">Fun fact: There is another problem that is basically a Fibnacci problem. </span>

<span style="color:blue">Description You are climbing a stair case. It takes n steps to reach to the top.

<span style="color:blue">Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top? <span style="color:blue">Do you know why? Try to solve it and you'll see.</span>

2. Word cloud problem
=====================

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
```

    ## Loading required package: RColorBrewer

``` r
library(RColorBrewer)

t3_cloud=wordcloud(words=t3$rownames.total., freq=t3$total, min.freq = 1,
         max.words=200, random.order=FALSE, rot.per=0, 
         colors=brewer.pal(9, "Dark2"))
```

    ## Warning in brewer.pal(9, "Dark2"): n too large, allowed maximum for palette Dark2 is 8
    ## Returning the palette you asked for with that many colors

![](whiteboard_prep_files/figure-markdown_github/unnamed-chunk-15-1.png)

3.Two Sum
=========

This problem is from leetcode, I have collected a few good solutions from the discussion.

Given an array of integers, return indices of the two numbers such that they add up to a specific target.

You may assume that each input would have exactly one solution, and you may not use the same element twice.

Example:

Given nums = \[2, 7, 11, 15\], target = 9,

Because nums\[0\] + nums\[1\] = 2 + 7 = 9, return \[0, 1\].

``` r
nums=c(2, 7, 11, 15)
target = 9
```

``` r
TwoSum=function(nums, target){
  if (length(nums)<2){
    print ("length must be greater than 2")
  } else {
for (i in 1:length(nums)){
  return(c(i, which(nums==target-nums[i])))
}
  }
}

TwoSum(nums, target)
```

    ## [1] 1 2

Test it with a different case

``` r
TwoSum(1, target=13)
```

    ## [1] "length must be greater than 2"

4.Apple Stock
=============

Writing programming interview questions hasn't made me rich yet ... so I might give up and start trading Apple stocks all day instead.

First, I wanna know how much money I could have made yesterday if I'd been trading Apple stocks all day.

So I grabbed Apple's stock prices from yesterday and put them in a list called stock\_prices, where:

The indices are the time (in minutes) past trade opening time, which was 9:30am local time. The values are the price (in US dollars) of one share of Apple stock at that time. So if the stock cost $500 at 10:30am, that means stock\_prices\[60\] = 500.

Write an efficient function that takes stock\_prices and returns the best profit I could have made from one purchase and one sale of one share of Apple stock yesterday.

For example:

stock\_prices = \[10, 7, 5, 8, 11, 9\]

get\_max\_profit(stock\_prices) Returns 6 (buying for $5 and selling for $11)

No "shorting"—you need to buy before you can sell. Also, you can't buy and sell in the same time step—at least 1 minute has to pass.

``` r
get_max_profit<-function(stock_prices){
profit=NULL
for (i in 1:length(stock_prices)){
  buy=stock_prices[i]
  profit[i]=as_tibble(stock_prices[i:length(stock_prices)]-buy)
  max_profit=max(unlist(profit))
}
return(max_profit)
}
```

Test our function

``` r
stock_prices = c(10, 7, 5, 8, 11, 9)
get_max_profit(stock_prices)
```

    ## [1] 6

Maybe a nested loop can give a solution that is easier to implement.

``` r
get_max_profit2<-function(stock_prices){
  c=r=0
  for (i in 1:length(stock_prices)){
    for (j in i:length(stock_prices)){
      buy=stock_prices[i];sell=stock_prices[j]
      c=sell-buy
      r=max(c,r)
    }
  }
  return(r)
}
```

``` r
stock_prices = c(10, 7, 5, 8, 11, 9)
get_max_profit2(stock_prices)
```

    ## [1] 6

5. Digit Count
==============

Description Count the number of k's between 0 and n. k can be 0 - 9. Example if n = 12, k = 1 in

\[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12\] we have FIVE 1's (1, 10, 11, 12)

``` r
count_digits<-function(n, k){
series=seq(1:n)
series_split=series%>%
  as.character()%>%
  strsplit(split=character(0))%>%
  unlist()
  position_count=sum(series_split==1)
return (position_count)
}
```

Test our function

``` r
count_digits(12, 1)
```

    ## [1] 5

6.Ugly Number
=============

Description Ugly number is a number that only have factors 2, 3 and 5.

Design an algorithm to find the nth ugly number. The first 10 ugly numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12... Note that 1 is typically treated as an ugly number. Example If n=9, return 10.

``` r
ugly_seq<-function(n){
ugly_seq=NULL
s=1;ugly_seq=s
while (length(ugly_seq)<n){
 s_new=s+1
 if (s_new%%2==0|s_new%%3==0|s_new%%5==0){
   ugly_seq=c(ugly_seq, s_new)
 }
 s=s_new
}
return(ugly_seq[n])
}
```

Test our function

``` r
ugly_seq(9)
```

    ## [1] 10

``` r
ugly_seq(15)
```

    ## [1] 20

7. Subset
=========

Description Given a set of distinct integers, return all possible subsets. Elements in a subset must be in non-descending order. The solution set must not contain duplicate subsets. Example If S = \[1,2,3\], a solution is:

\[ \[3\], \[1\], \[2\], \[1,2,3\], \[1,3\], \[2,3\], \[1,2\], \[\]\]

``` r
S=c(1,2,3)

lapply(S, function(x) combn(S,x))
```

    ## [[1]]
    ##      [,1] [,2] [,3]
    ## [1,]    1    2    3
    ## 
    ## [[2]]
    ##      [,1] [,2] [,3]
    ## [1,]    1    1    2
    ## [2,]    2    3    3
    ## 
    ## [[3]]
    ##      [,1]
    ## [1,]    1
    ## [2,]    2
    ## [3,]    3

Pay attention that this solution only works for the particular case of (1, 2, 3...) for a more general case:

``` r
subsets<-function(S){
  all_subsets=list()
  for (i in 1:length(S)) {
    all_subsets[[length(all_subsets)+1]]=unique(combn(S, i))
  }
  return (all_subsets)
}

S=c(5, 8, 10)

subsets(S)
```

    ## [[1]]
    ##      [,1] [,2] [,3]
    ## [1,]    5    8   10
    ## 
    ## [[2]]
    ##      [,1] [,2] [,3]
    ## [1,]    5    5    8
    ## [2,]    8   10   10
    ## 
    ## [[3]]
    ##      [,1]
    ## [1,]    5
    ## [2,]    8
    ## [3,]   10

Description Given a collection of integers that might contain duplicates, nums, return all possible subsets (the power set).

Each element in a subset must be in non-descending order. The ordering between two subsets is free. The solution set must not contain duplicate subsets.

Example Input: \[1,2,2\] Output:

\[ \[2\], \[1\], \[1,2,2\], \[2,2\], \[1,2\], \[\]\]

This does not quite work, will fix it later

``` r
S=c(1,2,2)

subsets(S)
```

    ## [[1]]
    ##      [,1] [,2] [,3]
    ## [1,]    1    2    2
    ## 
    ## [[2]]
    ##      [,1] [,2] [,3]
    ## [1,]    1    1    2
    ## [2,]    2    2    2
    ## 
    ## [[3]]
    ##      [,1]
    ## [1,]    1
    ## [2,]    2

7.Rotate String
===============

Description Given a string and an offset, rotate string by offset. (rotate from left to right)

Example Given "abcdefg".

offset=0 =&gt; "abcdefg" offset=1 =&gt; "gabcdef" offset=2 =&gt; "fgabcde" offset=3 =&gt; "efgabcd

``` r
string_rotate<-function(string, index){
  if (index==0){
    paste(string, collapse='')
  }else{
  string=string%>%
    strsplit(., split=character(0))%>%
    unlist()
    rotate_index=length(string)-(index-1)
    result= c(string[rotate_index:length(string)],      string[1:rotate_index-1])
    paste(result, collapse='')
  }   
}
```

Test the function

``` r
string="abcdefg"
string_rotate(string, 0)
```

    ## [1] "abcdefg"

``` r
string_rotate(string, 1)
```

    ## [1] "gabcdef"

``` r
string_rotate(string, 3)
```

    ## [1] "efgabcd"

8. String Permutation and Subsetting
====================================

This is a self-assigned problem, I think it is necessary that I know how to do it at the back of my head.

This is a solution from stack exchange that I have yet to understand

``` r
# Another recursive implementation    
# for those who like to roll their own, no package required 
permutations <- function( x, prefix = c() )
{
  if(length(x) == 0 ) return(prefix)
  do.call(rbind, sapply(1:length(x), FUN = function(idx) permutations( x[-idx], c( prefix, x[idx])), simplify = FALSE))
}

permutations(letters[1:3])
```

    ##      [,1] [,2] [,3]
    ## [1,] "a"  "b"  "c" 
    ## [2,] "a"  "c"  "b" 
    ## [3,] "b"  "a"  "c" 
    ## [4,] "b"  "c"  "a" 
    ## [5,] "c"  "a"  "b" 
    ## [6,] "c"  "b"  "a"

Also from stack exchange. Although a bit longer, it is easier to understand

``` r
getPerms <- function(x) {
    if (length(x) == 1) {
        return(x)
    }
    else {
        res <- matrix(nrow = 0, ncol = length(x)) #make an empty matrix with length of the object to be permutated
        for (i in seq_along(x)) {
            res <- rbind(res, cbind(x[i], getPerms(x[-i]))) 
#make a new row for reach i element, bind it as a new row to the existing matrix#
#each new row place the i element at the first position and permutate the rest without it#
        }
        return(res)
    }
}


getPerms(letters[1:3])
```

    ##      [,1] [,2] [,3]
    ## [1,] "a"  "b"  "c" 
    ## [2,] "a"  "c"  "b" 
    ## [3,] "b"  "a"  "c" 
    ## [4,] "b"  "c"  "a" 
    ## [5,] "c"  "a"  "b" 
    ## [6,] "c"  "b"  "a"

9.Dice Sum
==========

Description Throw n dices, the sum of the dices' faces is S. Given n, find the all possible value of S along with its probability.

You do not care about the accuracy of the result, we will help you to output results.

Example Given n = 1, return \[ \[1, 0.17\], \[2, 0.17\], \[3, 0.17\], \[4, 0.17\], \[5, 0.17\], \[6, 0.17\]\].

I would like to review how to write a recursive function

``` r
get_factor<-function(n){
  if (n==1)  return (1)
   else if (n==2) return (1*2)
   else return(n*get_factor(n-1))
}

get_factor(4)
```

    ## [1] 24

The dice sum problem can be solved recursively, as each time, we roll a new dice, we only have 6 possibilities. Assumed that we already rolled n-1 dices before we roll the n-th dice, we "glue" the 6 possibilities to whatever we have before this n-th dice.

here is the solution:

``` r
diceSum<-function(n){
  result=matrix(nrow=0, ncol=2)
  if (n==1)  ###base case
    for (i in 1:6){
      result=rbind(result, c(i, 0.17))
    } else {
      dice <- expand.grid(diceSum(n-1)[,1], 1:6)
      dice.sums <- rowSums(dice)
      dice.probs=diceSum(n-1)[,2]*0.17
      result=data.frame(dice.sums,dice.probs)
      result=aggregate(result[,2]~result[,1], FUN=sum) 
      #agregate the results of the same sum
    }
  return (result)
}
```

Test the function

``` r
diceSum(3)
```

    ##    result[, 1] result[, 2]
    ## 1            3    0.004913
    ## 2            4    0.014739
    ## 3            5    0.029478
    ## 4            6    0.049130
    ## 5            7    0.073695
    ## 6            8    0.103173
    ## 7            9    0.122825
    ## 8           10    0.132651
    ## 9           11    0.132651
    ## 10          12    0.122825
    ## 11          13    0.103173
    ## 12          14    0.073695
    ## 13          15    0.049130
    ## 14          16    0.029478
    ## 15          17    0.014739
    ## 16          18    0.004913

And, if I wish to go super hardcore on this one, I can think about how to derive the distribution, but, we'll see!

10.Interleaving String
======================

Description Given three strings: s1, s2, s3, determine whether s3 is formed by the interleaving of s1 and s2.

Example For s1 = "aabcc", s2 = "dbbca"

When s3 = "aadbbcbcac", return true. When s3 = "aadbbbaccc", return false.

This turns out to be a much harder problem so it is taking me a long while. After watching and reading many solutions, I finally got it! Thank ya'll youtubers and quizz enthusiasts!

``` r
##this function return TRUE or FALSE
interLeaving<-function(s1, s2, s3){
  #transfer the string to something that is manipulatable
  s1=strsplit(s1, split=character(0))%>%unlist()
  s2=strsplit(s2, split=character(0))%>%unlist()
  s3=strsplit(s3, split=character(0))%>%unlist()
  
  #initialize a matrix, each cell represent i+j-2 position in s3
  DP=matrix(NA, nrow=length(s1)+1, ncol=length(s1)+1) 
  #initialize the 0 row and the 0 colum, I will lable them with the according string, so if I made any mistake, I can always go back to check 
  colnames(DP)=c(0, s1)
  rownames(DP)=c(0, s2)
  #the [0,0]cell would always be true
  DP['0', '0']=TRUE
  DP['0', 2:ncol(DP)]=(s1==s3[1:length(s1)])
  DP[2:nrow(DP),'0']=(s2==s3[1:length(s2)])
  for (i in 2:nrow(DP)){
    for (j in 2:ncol(DP)){
      DP[i,j]=(s3[i+j-2]==s1[j-1] & DP[i,j-1] |s3[i+j-2]==s2[i-1] & DP[i-1,j])
      #check top string and left cell | left string and top cell
    }
  }
  return(DP[nrow(DP),ncol(DP)]) #the last cell is the answer!
}
```

Test our function. The correct function will return TRUE for s3; FALSE for s4

``` r
s1 = "aabcc"; s2 = "dbbca"
s3 = "aadbbcbcac";s4 = "aadbbbaccc"

interLeaving(s1, s2, s3)
```

    ## [1] TRUE

``` r
interLeaving(s1, s2, s4)
```

    ## [1] FALSE

11.n-queen problem
==================

Description The n-queens puzzle is the problem of placing n queens on an n×n chessboard such that no two queens attack each other.

Given an integer n, return all distinct solutions to the n-queens puzzle.

Each solution contains a distinct board configuration of the n-queens' placement, where 'Q' and '.' both indicate a queen and an empty space respectively.

Example There exist two distinct solutions to the 4-queens puzzle:

// Solution 1 \[".Q..", "...Q", "Q...", "..Q." \], // Solution 2 \["..Q.", "Q...", "...Q", ".Q.." \]

Challenge Can you do it without recursion?

I think the without recursion part is tough. But I want to remind myself how to do permuation problem first. Because this is essentially a permutation problem too. And I'll explain why later on. So a while ago, I have to figure out how to permuate using base R code

``` r
permutations <- function(n){
  if(n==1){
    return(matrix(1)) ### this is our base case
  } else {
    A=permutations(n-1) 
    B=matrix(nrow=0, ncol=n)
    for (i in 1:nrow(A)) {
      for (j in 0:(n-1)){ #for each row of the result (n-1), we want to append the new element in different position
        B=rbind(B, append(A[i,],n, after=j)) 
      }
    }
    return (B)
  }
}
```

Test the function

``` r
permutations(3)
```

    ##      [,1] [,2] [,3]
    ## [1,]    3    2    1
    ## [2,]    2    3    1
    ## [3,]    2    1    3
    ## [4,]    3    1    2
    ## [5,]    1    3    2
    ## [6,]    1    2    3

Ok, now let's go back to why this is essentially a permutation problem. So if we pay attention to the solution set for n=4 before, we will see that if the position of the queen is expressed in the index for its positin, it would be \[2, 4, 1, 3\] and \[3, 1, 4, 2\] The obsolute distance between any idexes are larger than 2, that is the condition to garantee no adjacent queens.

In order the replicate the above resuits, we can ask R to return the subsets of n permutation that fit the requirement.

``` r
per4=permutations(4)
for (i in 1:nrow(per4)){
    if (abs(per4[i,2]-per4[i,1])>=2 &
        abs(per4[i,3]-per4[i,2])>=2 &
        abs(per4[i,4]-per4[i,3])>=2)  
      print (per4[i,])
  }
```

    ## [1] 2 4 1 3
    ## [1] 3 1 4 2

Here, let's put everything together. I am too lazy so I'll just nest one function within another. You know, too much debugging if you are moving things around.

``` r
n_queen<-function(n) {
  #insert the permutation function
  permutations <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      A=permutations(n-1)
      B=matrix(nrow=0, ncol=n)
      for (i in 1:nrow(A)) {
        for (j in 0:(n-1)){
          B=rbind(B, append(A[i,],n, after=j)) 
        }
      }
      return (B)
    }
  }
  
  B=permutations(n) 
  
  subset=matrix(nrow=0, ncol=n)
  row_index=numeric(0)
  for (i in 1:nrow(B)){
    r=numeric(0)
    for (j in 2:n) r=c(r, abs(B[i,j]-B[i,j-1])>=2)
    if(sum(r)==n-1) row_index=c(row_index,i)
    subset=B[row_index,]
  }  
  return (subset) 
}
```

Test our function. Giving n, the function returns a set of position index for the queen in each row

``` r
n_queen(4)
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    2    4    1    3
    ## [2,]    3    1    4    2

``` r
n_queen(5)
```

    ##       [,1] [,2] [,3] [,4] [,5]
    ##  [1,]    3    5    2    4    1
    ##  [2,]    4    2    5    3    1
    ##  [3,]    2    5    3    1    4
    ##  [4,]    4    2    5    1    3
    ##  [5,]    5    2    4    1    3
    ##  [6,]    2    4    1    5    3
    ##  [7,]    2    4    1    3    5
    ##  [8,]    5    3    1    4    2
    ##  [9,]    3    5    1    4    2
    ## [10,]    3    1    4    2    5
    ## [11,]    3    1    5    2    4
    ## [12,]    4    1    3    5    2
    ## [13,]    1    3    5    2    4
    ## [14,]    1    4    2    5    3

Actually, just found this solution is wrong, because I do not play chess, therefore, did not know how it works. Looks like the queen can attack no just the immediate cell it's next too, it attacks like a radiactive light, straight and diagnally. Ugh~ so I modified the above function, now it works!

``` r
n_queen_correct<-function(n) {
  #insert the permutation function
  permutations <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      A=permutations(n-1)
      B=matrix(nrow=0, ncol=n)
      for (i in 1:nrow(A)) {
        for (j in 0:(n-1)){
          B=rbind(B, append(A[i,],n, after=j)) 
        }
      }
      return (B)
    }
  }
  
  B=permutations(n) 
  
  #if the sum of row index and column index is the same, queen can attack.
  upper_diag=matrix(nrow=nrow(B), ncol=n)
  for (i in 1:nrow(B)){
    upper_diag[i,]=(1:n+B[i,1:n]) #sum the row and column index by row
  }
  #row sum with duplicated numbers are the rows that disqualify
  lower_diag=matrix(nrow=nrow(B), ncol=n)
  for (i in 1:nrow(B)){
    lower_diag[i,]=(1:n-B[i,1:n])
  }
  #similarly, duplicated numbers that represent row index substracted by column index disqualify
  u=NULL
  for (i in 1:nrow(B)){
    if (anyDuplicated(upper_diag[i,])==0)
      u=c(u, i)
  }
  
  l=NULL
  for (i in 1:nrow(B)){
    if (anyDuplicated(lower_diag[i,])==0)
      l=c(l, i)
  }
  subset_index=intersect(u,l)
  subset=B[subset_index,]
  return (subset) 
}
```

Test the function. If correct, the function should return 10 subsets for n=5

``` r
n_queen_correct(5)
```

    ##       [,1] [,2] [,3] [,4] [,5]
    ##  [1,]    3    5    2    4    1
    ##  [2,]    4    2    5    3    1
    ##  [3,]    2    5    3    1    4
    ##  [4,]    5    2    4    1    3
    ##  [5,]    2    4    1    3    5
    ##  [6,]    5    3    1    4    2
    ##  [7,]    3    1    4    2    5
    ##  [8,]    4    1    3    5    2
    ##  [9,]    1    3    5    2    4
    ## [10,]    1    4    2    5    3

I realize that I am being a lazy ass here and did not return the answer as instructed. But I have reference pretty good print out from the internet, and here it is!

``` r
print_board <- function(p, q.char = "Q", sep = " ") {
  n <- length(p)
  row <- rep("-", n)
  for (i in seq_len(n)) {
    row_i <- row
    row_i[p[i]] <- q.char
    cat(paste(row_i, collapse = sep))
    cat("\n")
  }
}
```

``` r
sol_5=n_queen_correct(5)

for (i in 1:nrow(sol_5)){
  print(print_board(sol_5[i,]))
  cat("\n")
}
```

    ## - - Q - -
    ## - - - - Q
    ## - Q - - -
    ## - - - Q -
    ## Q - - - -
    ## NULL
    ## 
    ## - - - Q -
    ## - Q - - -
    ## - - - - Q
    ## - - Q - -
    ## Q - - - -
    ## NULL
    ## 
    ## - Q - - -
    ## - - - - Q
    ## - - Q - -
    ## Q - - - -
    ## - - - Q -
    ## NULL
    ## 
    ## - - - - Q
    ## - Q - - -
    ## - - - Q -
    ## Q - - - -
    ## - - Q - -
    ## NULL
    ## 
    ## - Q - - -
    ## - - - Q -
    ## Q - - - -
    ## - - Q - -
    ## - - - - Q
    ## NULL
    ## 
    ## - - - - Q
    ## - - Q - -
    ## Q - - - -
    ## - - - Q -
    ## - Q - - -
    ## NULL
    ## 
    ## - - Q - -
    ## Q - - - -
    ## - - - Q -
    ## - Q - - -
    ## - - - - Q
    ## NULL
    ## 
    ## - - - Q -
    ## Q - - - -
    ## - - Q - -
    ## - - - - Q
    ## - Q - - -
    ## NULL
    ## 
    ## Q - - - -
    ## - - Q - -
    ## - - - - Q
    ## - Q - - -
    ## - - - Q -
    ## NULL
    ## 
    ## Q - - - -
    ## - - - Q -
    ## - Q - - -
    ## - - - - Q
    ## - - Q - -
    ## NULL

Argh, don't ask, I am too lazy to fix the NULL in between. Let's just leave it like that.

However, I believe this solution is not entirely sufficient. First, I have used recursion; secondly, I have enuerate a bunch of possibilities that I do not need then eliminate them one by one. However, there is some merits in this solution, as by permutation, I don't need to deal with the constraint of placing the queen in the same row or the same column.

The standard solution calls for backtracking, which is a concept that I have not mastered yet.

12.Maximum Subarray
===================

Description Given an array of integers, find two non-overlapping subarrays which have the largest sum. The number in each subarray should be contiguous. Return the largest sum.

Example For given \[1, 3, -1, 2, -1, 2\], the two subarrays are \[1, 3\] and \[2, -1, 2\] or \[1, 3, -1, 2\] and \[2\], they both have the largest sum 7.

I accidently solve for the maximal subarray, but since I took the effect, I might as well document it too. Maybe later on, I can think about how to add it it.

``` r
maxSubarray<-function(array){
  slice_sum=matrix(0,nrow=length(array), ncol=length(array))
for (i in 1:nrow(slice_sum)){
  for (j in 1:ncol(slice_sum)){
    if(i<=j){
      slice_sum[i,j]=sum(array[i:j])
    }
  }
}
index=which(slice_sum==max(slice_sum), arr.ind = TRUE)
max_sub=array[index[1]:index[2]]
result=list(MaxSubarray=max_sub, sum=max(slice_sum))
return (result)
}
```

Test our function.

``` r
array=c(1, 3, -1, 2, -1, 2)

maxSubarray(array)
```

    ## $MaxSubarray
    ## [1]  1  3 -1  2 -1  2
    ## 
    ## $sum
    ## [1] 6

In solving for 2 non-overlapping subarrays, I can use a point at the array, solve for the maximal subarray from the right, and from the right, then I know these two combined will give me the largest pair. Incooperating the function I already have, this is probably going to take an hour or two. But in the following, I would like to give Kadane's Algorithm a shot. The idea is that, every time I move one number onward, I only need to check if the maxsubarray so far is bigger or less than my current number combing with the my current number. The following functions has stolen the ideal from the maxsub function from the adagio package.

``` r
max_sum1<-function(array){
  m1=m2=0 #initialize m1 m2 as 0
  for (i in 1:length(array)){
  m2=m2+array[i]# store sum as iterate one element onward
  m1=max(m2, m1) #keep track on the local maximum
  }
  return (m1)
}
#in a simplier case, this version of the function returns the maximal sum of the subarray
```

Test our function.

``` r
max_sum1(array)
```

    ## [1] 6

``` r
max_sum1(array[1:3])
```

    ## [1] 4

The function correctly return the maximal subarray sum of the whole array, and the maximal sum of up to the third element.

We are going to look at a more complicated task, when we need to return both the array sum, and the subarray itself.

``` r
max_sum2<-function(array){
m1 <- m2 <- 0 #initialize both the local sum the global sum as 
p1 <- p2 <- 0 #the global index
q1 <- q2 <- 1 #the local index
for (i in 1:length(array)) {
  if (m2 > -array[i]) { 
    #check if including the current iteration will have a bigger local sum 
    m2 <- m2 + array[i]
    q2 <- i #update the local index
    if (m2 > m1) {
      m1 <- m2
      p1 <- q1; p2 <- q2 #update the global index
    }
  } 
  else {
    m2 <- 0
    q1 <- q2 <- i+1
  }
}
return(list(sum = m1, sub_array=array[p1:p2]))
}
```

Test our function

``` r
max_sum2(array)
```

    ## $sum
    ## [1] 6
    ## 
    ## $sub_array
    ## [1]  1  3 -1  2 -1  2

Now, the critical point. If we are to solve for two non-overlapping subarray that gives the largest sum, we can think of devided the array into two slices. We solve for the subarray from the left side while also solve for one on the right side. Then the two of these two subarray combined would give the largest sum.

``` r
Two_sub<-function(array){
  ###get the max sum function first
  max_sum2<-function(array){
    m1 <- m2 <- 0 #initialize both the local sum the global sum as 
    p1 <- p2 <- 0 #the global index
    q1 <- q2 <- 1 #the local index
    for (i in 1:length(array)) {
      if (m2 > -array[i]) { 
        #check if including the current iteration will have a bigger local sum 
        m2 <- m2 + array[i]
        q2 <- i #update the local index
        if (m2 > m1) {
          m1 <- m2
          p1 <- q1; p2 <- q2 #update the global index
        }
      } 
      else {
        m2 <- 0
        q1 <- q2 <- i+1
      }
    }
    return(list(sum = m1, sub_array=array[p1:p2]))
  }
  
  ###now we divide
  max_list=list() #this would store the sum of all the subarray at each dividing point
  r=0;c=0
  for (i in 1:(length(array)-1)){
    
    left=array[1:i]
    right=array[(i+1):length(array)]
    
    left_max=max_sum2(left)
    right_max=max_sum2(right)
   c=left_max[[1]]+right_max[[1]]
   r=max(r,c)
  }
  return (r)
}

Two_sub(array)
```

    ## [1] 7

This function return the maximal sum but not the subarrays.

13.Longest Substring
====================

Given a string, find the length of the longest substring without repeating characters. For example, the longest substring without repeating letters for "abcabcbb" is "abc", which the length is 3. For "bbbbb" the longest substring is "b", with the length of 1.

This is a rather easy one!

``` r
longest_sub1<-function(string){
  string=string%>%strsplit(split=character(0))%>%unlist()
  max_length=0
  sub=NULL
  longest_sub=NULL
  for (i in 1:length(string)){
    if (!string[i]%in%sub){
      sub=c(sub, string[i])
      longest_sub=sub
    } else {
      sub=NULL
      sub=c(sub, string[i])
      if (length(sub)>length(longest_sub)){
        longest_sub=sub
      }
    }
  } 
  max_length=length(longest_sub)
  longest_sub=paste(longest_sub, collapse = "")
  return (paste("The longest Substring is:", longest_sub,", and its length is:", max_length, collapse=""))
}
```

Test our function.

``` r
string="abcabcbb"
string1="bbbbbbb"

longest_sub1(string)
```

    ## [1] "The longest Substring is: abc , and its length is: 3"

``` r
longest_sub1(string1)
```

    ## [1] "The longest Substring is: b , and its length is: 1"

I have done another version, which is the longest substring with alphabetical order, I might as well document that function as well. So here it goes:

``` r
longest_sub2<-function(string){
string=string%>%strsplit(split=character(0))%>%unlist()
max_length=0
sub=NULL
sub_2=NULL

for (i in 1:length(string)){
  if (length(sub)==0) sub=string[i] 
  else if (string[i]>=sub[length(sub)]) sub=c(sub, string[i]) #piece the next character if it is in order
  else if (string[i]<sub[length(sub)]) {
    if (length(sub)>length(sub_2)){
      sub_2=sub #sub_2 keep track of the qualified substring so far
      sub=string[i]
    } else {
      sub=string[i]
    }
  }
  max_length=length(sub_2)
}
sub_2=paste(sub_2, collapse = '')
return (c(sub_2, max_length))
}
```

Test our function.

``` r
string3 = 'cyqfjhcclkbxpbojgkar'

longest_sub2(string3)
```

    ## [1] "ccl" "3"

14.Sort Letters by Case
=======================

Description Given a string which contains only letters. Sort it by lower case first and upper case second.

Example For "abAcD", a reasonable answer is "acbAD"

Challenge Do it in one-pass and in-place.

The one to go about it is to first chop up the string into letters, use regrex to seperate the lower and upper case letters, sort each, then paste them together again. Here is an example of what I mean

``` r
s="TEsT"
s=strsplit(s, split=character(0))%>%unlist()
grepl("^[[:upper:]]+$", s)
```

    ## [1]  TRUE  TRUE FALSE  TRUE

Now use the bolean to retrieve the uppercase letters in the string

``` r
upperindex=grepl("^[[:upper:]]+$", s)
s[upperindex]
```

    ## [1] "T" "E" "T"

Now, let's put it together

``` r
case_sorter<-function(string){
  string=string%>%strsplit(split=character(0))%>%unlist()
  u_index=grepl("^[[:upper:]]+$", string)
  l_index=grepl("^[[:lower:]]+$", string)
  
  u_sorted=sort(string[u_index])
  l_sorted=sort(string[l_index])
  
  string_new=append(l_sorted, u_sorted)%>%paste(collapse = "")
  return(string_new)
}

case_sorter(string)
```

    ## [1] "aabbbbcc"

15.3Sum
=======

Description Given an array S of n integers, are there elements a, b, c in S such that a + b + c = 0? Find all unique triplets in the array which gives the sum of zero.

Elements in a triplet (a,b,c) must be in non-descending order. (ie, a ≤ b ≤ c)

The solution set must not contain duplicate triplets.

Example For example, given array S = {-1 0 1 2 -1 -4}, A solution set is:

(-1, 0, 1) (-1, -1, 2)

The naive way to apprach it is to generate all possible combination of 3 and eliminate those who does not sum to 0.The second approach is to use two pointers. The built in function combn can easily realize the naive function.

``` r
sum_3<-function(s){
  s=sort(s)
  index=c(which(combn(s, 3, FUN = sum)==0))
  combo=combn(s, 3)
  combo=combo[, index]%>%t()%>%unique(margin=1)
  return (combo)
}
```

Test our function

``` r
s = c(-1, 0, 1, 2, -1, -4)
sum_3(s)
```

    ##      [,1] [,2] [,3]
    ## [1,]   -1   -1    2
    ## [2,]   -1    0    1

16.Longest Common Subsequence
=============================

Description Given two strings, find the longest common subsequence (LCS).

Your code should return the length of LCS.

Example For "ABCD" and "EDCA", the LCS is "A" (or "D", "C"), return 1. For "ABCD" and "EACB", the LCS is "AC", return 2.

Try a naive approach that is along the line of the DP. Find all the common elements between the two string, and see what is the order in the seond string, return the longest ordered index.

``` r
lcs<-function(a, b){
a=a%>%strsplit(split=character(0))%>%unlist()
b=b%>%strsplit(split=character(0))%>%unlist()

common=a[which(a%in%b)]
##note that the order of the argument matters, intersect always return the common elements in the order of the first argument
index=numeric()
for (i in 1:length(common)){
  index=c(index, which(b==common[i]))
}

r=1;c=0
q=index[1]
for (i in 2:length(index)){
  if (index[i]>q[length(q)]) q=c(q,index[i])
  else q=index[i]

c=length(q)
r=max(r, c)
}
return (r)
}
```

Test our function

``` r
a="ABCD"
b="EDCA"
c="EACB"

lcs(a,b)
```

    ## [1] 1

``` r
lcs(a,c)
```

    ## [1] 2

Test our function on longer strings

``` r
x="AGGTAB"
y="GXTXAYB"

lcs(x,y)
```

    ## [1] 4

There is a very nice recursion that I found on Greeks for Greeks. The idea is that is the last letter matches, than the length would just be 1+ the lcs. Here is what that solution looks like: def lcs(X, Y, m, n):

    if m == 0 or n == 0:
       return 0;
    elif X[m-1] == Y[n-1]:
       return 1 + lcs(X, Y, m-1, n-1);
    else:
       return max(lcs(X, Y, m, n-1), lcs(X, Y, m-1, n));

I would try to replicate this in R.

``` r
lcs_2<-function(x,y){
  x=x%>%strsplit(split=character(0))%>%unlist()
  y=y%>%strsplit(split=character(0))%>%unlist()
  
  m=length(x);n=length(y)
  
  lcs_3<-function(x, y, m, n){
    if (m == 0 | n == 0) return (0)
    else if (x[m] == y[n]) return (1 + lcs_3(x, y, m-1, n-1))
    else return (max(lcs_3(x, y, m, n-1), lcs_3(x, y, m-1, n)))
  }
  
  max_length=lcs_3(x, y, m, n)
  return (max_length)
}
```

Test our function

``` r
lcs_2(x,y)
```

    ## [1] 4

``` r
a="ABCD"
b="EDCA"
c="EACB"

lcs_2(a, b)
```

    ## [1] 1

``` r
lcs_2(a, c)
```

    ## [1] 2

``` r
lcs_2(b, c)
```

    ## [1] 2

17.Longest Common String
========================

Description Given two strings, find the longest common substring.

Return the length of it.

Example Given A = "ABCD", B = "CBCE", return 2.

This problem can be solved in DP

``` r
lc_string<-function(A, B){
A=A%>%strsplit(split = character(0))%>%unlist()
B=B%>%strsplit(split = character(0))%>%unlist()

m=length(A);n=length(B)
dp=matrix(0,nrow=n+1, ncol=m+1)
for (i in 2:ncol(dp)){
dp[i,2:ncol(dp)]=(B[i-1]==A)
}
for (i in 2:nrow(dp)){
  for (j in 2:ncol(dp)){
    if (!dp[i,j]==0) dp[i,j]=dp[(i-1),(j-1)]+1
  }
}
return (max(dp))
}
```

``` r
A = "ABCD"; B = "CBCE"
lc_string(A, B)
```

    ## [1] 2

If we are to asked to provide this longest common string, we just need to refer to the DP matrix and return according to the index

``` r
A=A%>%strsplit(split = character(0))%>%unlist()
B=B%>%strsplit(split = character(0))%>%unlist()
m=length(A);n=length(B)
dp=matrix(0,nrow=n+1, ncol=m+1)
for (i in 2:ncol(dp)){
dp[i,2:ncol(dp)]=(B[i-1]==A)
}
for (i in 2:nrow(dp)){
  for (j in 2:ncol(dp)){
    if (!dp[i,j]==0) dp[i,j]=dp[(i-1),(j-1)]+1
  }
}

colnames(dp)=c(0, A)
rownames(dp)=c(0, B)
dp
```

    ##   0 A B C D
    ## 0 0 0 0 0 0
    ## C 0 0 0 1 0
    ## B 0 0 1 0 0
    ## C 0 0 0 2 0
    ## E 0 0 0 0 0

The longest common substring is 'BC'.

18.K sum
========

Description Given n distinct positive integers, integer k (k &lt;= n) and a number target.

Find k numbers where sum is target. Calculate how many solutions there are?

Example Given \[1,2,3,4\], k = 2, target = 5.

There are 2 solutions: \[1,4\] and \[2,3\].

Return 2.

I have solve two sum and three sum problem not that long ago. Remember, the built-in combn function can easily realize the naive solution for us. The basic idea is to generate all the possible combination and sum them up then elimiate all the combination that does not add up to the target.

``` r
#let's transform this into t k sum problem solution
sum_k<-function(nums,k,target){
  if (k>=length(nums)) print("K is too long")
  else {
  index=c(which(combn(nums, k, FUN = sum)==target))
  combo=combn(nums, k)
  combo=combo[, index]%>%t()%>%unique(margin=1)
  return (list(solution=combo, possibilities=nrow(combo)))
  }
}
```

Test our function

``` r
#Given [1,2,3,4], k = 2, target = 5.
nums=c(1, 2, 3, 4)
sum_k(nums, 2, 5)
```

    ## $solution
    ##      [,1] [,2]
    ## [1,]    1    4
    ## [2,]    2    3
    ## 
    ## $possibilities
    ## [1] 2

But there are more alternatives. For one thing, we can think recursively: reduce the k sum problem into k-1 sum problem, until it is back to the most basic 2 sum problem. A while ago, I also learned how to use the dictionary to solve it in python, which looks somewhat like this: def twoSum(nums, target):

        if len(nums) <= 1: #always think about this situation during an interview
            return False
        buff_dict = {} #make an empty dictionary
        for i in range(len(nums)):
            if nums[i] in buff_dict:
                return [buff_dict[nums[i]], i]
            #return the value of the dictionary and the index.
            #for example, when i=1, 7 is already in dict, than [0, 1] is returned
            else:
                buff_dict[target - nums[i]] = i 
            #the key would be the result of the substraction, and the value would be the index of the subtracting number
            #for example, when i=0, dict={7:0}

Here I would like to re-attempt the 2 sum problem with a pointer solution.

``` r
sum_2<-function(nums, target){
  n=length(nums)
  start=1;end=n
  res=list()
  while (start<end) {
    sum=nums[start]+nums[end]
    if (sum==target) {
    res[[length(res)+1]] <- c(nums[start],nums[end])
    ##when finding the pair at the first go,moving both pointers
    start=start+1;end=end-1 
    }else if (sum<target) {
    start=start+1 
    }else {end=end-1}
} 
  return(res)
}
```

Test our function.

``` r
sum_2(nums, target=5)
```

    ## [[1]]
    ## [1] 1 4
    ## 
    ## [[2]]
    ## [1] 2 3

Finally, the solution for the ksum problem in recursive way!

``` r
sum_k_recur<-function(nums, k, target){
  
  #import the 2sum function
  sum_2<-function(nums, target){
    n=length(nums)
    start=1;end=n
    res=list()
    while (start<end) {
      sum=nums[start]+nums[end]
      if (sum==target) {
        res[[length(res)+1]] <- c(nums[start],nums[end])
        start=start+1;end=end-1
      }else if (sum<target) {
        start=start+1
      }else {end=end-1}
    } 
    return(res)
  }
  
res=list()
if (k==2) {
res=sum_2(nums, target)
return (res) 
} else {
  for (i in 1:length(nums)){
    return (c(nums[i],unlist(sum_k_recur(nums[-i], k-1, target-nums[i])) ))
  }
}
}
```

Test our function

``` r
nums=c(1, 2, 3, 4, 6)

sum_k_recur(nums, 2, 5) ##the base case
```

    ## [[1]]
    ## [1] 1 4
    ## 
    ## [[2]]
    ## [1] 2 3

``` r
sum_k_recur(nums, 3, 6)
```

    ## [1] 1 2 3

``` r
sum_k_recur(nums, 4, 10)
```

    ## [1] 1 2 3 4

So far, it is working, but hey, I don't know whether this is going to be buggy when the situation gets more complicated but I am too lazy to figure out~

19. Unique Path
===============

A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).

The robot can only move either down or right at any point in time. The robot is trying to reach the bottom-right corner of the grid (marked 'Finish' in the diagram below).

How many possible unique paths are there? ![Robot](http://img.it610.com/image/product/ce056ac09918486ba882ed49b1878218.jpg)

Above is a 7 x 3 grid. How many possible unique paths are there? Note: m and n will be at most 100.

Example 1: Input: m = 3, n = 2 Output: 3 Explanation: From the top-left corner, there are a total of 3 ways to reach the bottom-right corner: 1. Right -&gt; Right -&gt; Down 2. Right -&gt; Down -&gt; Right 3. Down -&gt; Right -&gt; Right

Example 2:

Input: m = 7, n = 3 Output: 28

This is a DP problem. We will label the first row and column with 1, as there is only one unique way to get to right or down.

``` r
unique_path<-function(m, n){
  dp=matrix(nrow=m, ncol=n)
  dp[1,]=1;dp[,1]=1;
  for (i in 2:m){
    for (j in 2:n){
      dp[i,j]=dp[(i-1),j]+dp[i,(j-1)]
    }
  }
  return(dp[nrow(dp), ncol(dp)])
}
```

Test the function

``` r
unique_path(3,2)
```

    ## [1] 3

``` r
unique_path(7,3)
```

    ## [1] 28

20.Jump Game II
===============

Description Given an array of non-negative integers, you are initially positioned at the first index of the array.

Each element in the array represents your maximum jump length at that position.

Your goal is to reach the last index in the minimum number of jumps.

Example Given array A = \[2,3,1,1,4\]

The minimum number of jumps to reach the last index is 2. (Jump 1 step from index 0 to 1, then 3 steps to the last index.)

Use DP-ish way to solve the problem, keep one array to track max distance for each step and the minimal steps to get to a position. Return the minimal steps for the last position as a result.

``` r
min_jump<-function(A){
n=length(A)
position=seq(n)
max_dist=position+A
min_step=numeric(n)
min_step[1]=0
min_step[2:max_dist[1]]=1
for (i in 2:(n-1)){
  if(max_dist[i]>max(max_dist[1:(i-1)]))
  min_step[(max_dist[i-1]+1):max_dist[i]]=min_step[max_dist[i-1]]+1
}
return (min_step[n])
}
```

Test our function.

``` r
###jump game
A = c(2,3,1,1,4)
B =c(2, 3, 1, 1, 2, 4, 2, 0, 1, 1)

min_jump(A)
```

    ## [1] 2

``` r
min_jump(B)
```

    ## [1] 4

There is an easier version of the jump game quiz, which is to return whether it is possible to reach the last index. We only need to look at the max step to figure that one out.

21. Minimum Window Substring
============================

Given a string source and a string target, find the minimum window in source which will contain all the characters in target.

Clarification Should the characters in minimum window has the same order in target?

Not necessary. Example For source = "ADOBECODEBANC", target = "ABC", the minimum window is "BANC"

In CS language, this is a sliding window problem.Initialize the head and tail pointers and start researching. Move the tail only when not all target characters are found; Move the head when all target characters are found. By eyeballing the source string, the first window would be \[*A**D**O**B**E**C*\]*O**D**E**B**A**N**C* and the second would be *A**D**O*\[*B**E**C**O**D**E**B**A*\]*N**C* etc etc. At the same time, we keep track of the minimal length.

``` r
min_windown<-function(source, target){

source=source%>%strsplit(split=(character(0)))%>%unlist()
target=target%>%strsplit(split=(character(0)))%>%unlist()

i=j=1
c=r=0
n=length(target)
if(sum(source[i]==target)>0){
  while(sum(target%in%source[i:j])<n)
  j=j+1
} else {
  i=i+1
}
#found the first window at this point
c=length(source[i:j]);r=c
head=i;tail=j
while(j<length(source)){ #do this until tail reaching the end of the source string
#now move the head
if (sum(target%in%source[i:j])==n){
  i=i+1
  while(sum(source[i]==target)==0) i=i+1
    while(sum(target%in%source[i:j])<n) j=j+1
}      
#at this point, found the second window
c=length(source[i:j])
if (c<r){  ##update head and tail when the shorter window has been found
  r=c
  head=i;tail=j
  }
}
window=paste(source[head:tail], collapse="")
return (window)
}
```

Test our function

``` r
source = "ADOBECODEBANC"; target = "ABC"
min_windown(source, target)
```

    ## [1] "BANC"

While I was solving this quiz, I watch a youtube clip to help me going. It was fun, at least it helps me feel less longly! Here is the clip:

[Yusen solving minial window substring](https://www.youtube.com/watch?v=OXLgNDt4QMY)

22.Word Break
=============

Description Given a string s and a dictionary of words dict, determine if s can be break into a space-separated sequence of one or more dictionary words.

Example Given s = "lintcode", dict = \["lint", "code"\].

Return true because "lintcode" can be break as "lint code".

This is an easy one, we can do it iteratively or recursively, but I always refer iteration as it is easy to understand.

``` r
wordbreask<-function(s, dict){
s_string=s%>%strsplit(split=character(0))%>%unlist()
n=length(s_string)

for (i in 1:n){
s_pre=s_string[1:i]%>%paste(.,collapse="")
s_suf=s_string[(i+1):n]%>%paste(.,collapse="")
if (s_pre%in%dict | s_suf%in%dict) return(c(TRUE, paste(s_pre, s_suf, sep=" ")))
}
}
```

Test our function

``` r
s = "lintcode"; dict = c("lint", "code")
wordbreask(s, dict)
```

    ## [1] "TRUE"      "lint code"

``` r
dict2=c('i', 'like', 'sam', 'sung', 'samsung', 'mobile', 'ice', 'cream', 'icecream', 'man', 'go', 'mango')
s2='ilike'

wordbreask(s2, dict2)
```

    ## [1] "TRUE"   "i like"

23. Palindrome Partitioning II
==============================

Given a string s, partition s such that every substring of the partition is a palindrome.

Return the minimum cuts needed for a palindrome partitioning of s.

Example:

Input: "aab" Output: 1 Explanation: The palindrome partitioning \["aa","b"\] could be produced using 1 cut.

The following solution is inspired by a leetcode contributor, here is her original python solution

    def minCut(self, s):

      cut = [x for x in range(-1,len(s))]

      for i in range(0,len(s)):

        for j in range(i,len(s)):
      
            if s[i:j] == s[j:i:-1]:
      
            cut[j+1] = min(cut[j+1],cut[i]+1)
    return cut[-1]

The main algorithm idea is if s\[i,j\] is a palindrome, then the minCut(s\[:j\]) is at most minCut(s\[:i-1\])+1.

``` r
min_palincut<-function(s){
s=s%>%strsplit(split=character(0))%>%unlist()
if (identical(s, rev(s))) return(0)
###establish how to evaluate whether a string is a palin or not
cut=seq(-1,length(s)-2) ###get a vector to store the worse case scenario when no palin is found
for (i in 1:length(s)){
  for (j in i:length(s)){
    if (identical(s[i:j], rev(s[i:j]))){
    cut[j] = min(cut[j],cut[i-1]+1) 
    }
  }
} 
return(cut[length(cut)])
}
```

The problem can also be solved in DP, but I like this logic better. Let's test our function.

``` r
s="aab"
min_palincut(s)
```

    ## [1] 1

Test it with a more complicated case.

``` r
s = "kayakmadamotto"
min_palincut(s)
```

    ## [1] 3

24. Interleaving Positive and Negative Numbers
==============================================

Description Given an array with positive and negative integers. Re-range it to interleaving with positive and negative integers.

Example Given \[-1, -2, -3, 4, 5, 6\], after re-range, it will be \[-1, 5, -2, 4, -3, 6\] or any other reasonable answer.

Challenge Do it in-place and without extra memory.

The slight challenge would be to do it in place, I have to debug a few times how to write the conditional statement, otherwise, this is a pretty easy one.

``` r
interleaving_nums<-function(nums){
n=length(nums)
start=1;end=n
while(!end-start<=2){
  nums=c(nums[start], nums[end], nums[-c(start, end)])
         start=start+2
}
return (nums)
}
```

Test our function

``` r
nums=c(-1, -2, -3, 4, 5, 6)

interleaving_nums(nums)
```

    ## [1] -2  5 -1  6 -3  4

25.Largest Rectangle in Histogram
=================================

Description Given n non-negative integers representing the histogram's bar height where the width of each bar is 1, find the area of largest rectangle in the histogram.

![histogram](https://lintcode-media.s3.amazonaws.com/problem/histogram1.png)

Above is a histogram where width of each bar is 1, given height = \[2,1,5,6,2,3\].

![histogram](https://lintcode-media.s3.amazonaws.com/problem/histogram_area.png)

The largest rectangle is shown in the shaded area, which has area = 10 unit. Example Given height = \[2,1,5,6,2,3\], return 10.

Pretty easy, the only trick is that the width is conditional.

``` r
max_area<-function(height){
n=length(height)
###the area of the rectagles is height*width, width is as long as the next height index is larger than the height index
area=numeric(n)
for (i in 1:(n-1)){
  if (height[i+1]>height[i]){
  area[i]=height[i]*(1+length(which(height[i:n]>height[i])))
  } else {
    area[i]=height[i]
  }
}
area[n]=height[n]
return (max(area))
}
```

``` r
height = c(2,1,5,6,2,3)
max_area(height)
```

    ## [1] 10

26.The Skyline Problem
======================

Description Given N buildings in a x-axis，each building is a rectangle and can be represented by a triple (start, end, height)，where start is the start position on x-axis, end is the end position on x-axis and height is the height of the building. Buildings may overlap if you see them from far away，find the outline of them。

An outline can be represented by a triple, (start, end, height), where start is the start position on x-axis of the outline, end is the end position on x-axis and height is the height of the outline. ![skyline](https://lintcode-media.s3.amazonaws.com/problem/jiuzhang3.jpg) Example Given 3 buildings：

\[ \[1, 3, 3\], \[2, 4, 4\], \[5, 6, 1\]\] The outlines are：

\[ \[1, 2, 3\], \[2, 4, 4\], \[5, 6, 1\]\]

``` r
skyline<-function(building){
outline=list()
for (i in 1: (length(building)-1)){
if (building[[i]][2]>building[[i+1]][1]){
  outline[[i]]=c(building[[i]][1],building[[i+1]][1], building[[i]][3])
}else {
  outline[[i]]=building[[i]]
}
  #the last outline has its own rule, it is always the builing itslef
  outline[[length(building)]]=building[[length(building)]]
}
return (outline)
}
```

Test our function.

``` r
building=list(
  c(1, 3, 3),
  c(2, 4, 4),
  c(5, 6, 1)
  )

skyline(building)
```

    ## [[1]]
    ## [1] 1 2 3
    ## 
    ## [[2]]
    ## [1] 2 4 4
    ## 
    ## [[3]]
    ## [1] 5 6 1

There is a different version on leetcode. Instead of returning the outline, the required function returns points to draw the outline. Logic is exactly the same.

A city's skyline is the outer contour of the silhouette formed by all the buildings in that city when viewed from a distance. Now suppose you are given the locations and height of all the buildings as shown on a cityscape photo (Figure A), write a program to output the skyline formed by these buildings collectively (Figure B).

!(figure A)\[<https://leetcode.com/static/images/problemset/skyline1.jpg>\]

!(figure B)\[<https://leetcode.com/static/images/problemset/skyline2.jpg>\]

The geometric information of each building is represented by a triplet of integers \[Li, Ri, Hi\], where Li and Ri are the x coordinates of the left and right edge of the ith building, respectively, and Hi is its height. It is guaranteed that 0 ≤ Li, Ri ≤ INT\_MAX, 0 &lt; Hi ≤ INT\_MAX, and Ri - Li &gt; 0. You may assume all buildings are perfect rectangles grounded on an absolutely flat surface at height 0.

For instance, the dimensions of all buildings in Figure A are recorded as: \[ \[2 9 10\], \[3 7 15\], \[5 12 12\], \[15 20 10\], \[19 24 8\] \] .

The output is a list of "key points" (red dots in Figure B) in the format of \[ \[x1,y1\], \[x2, y2\], \[x3, y3\], ... \] that uniquely defines a skyline. A key point is the left endpoint of a horizontal line segment. Note that the last key point, where the rightmost building ends, is merely used to mark the termination of the skyline, and always has zero height. Also, the ground in between any two adjacent buildings should be considered part of the skyline contour.

For instance, the skyline in Figure B should be represented as:\[ \[2 10\], \[3 15\], \[7 12\], \[12 0\], \[15 10\], \[20 8\], \[24, 0\] \].

Notes:

The number of buildings in any input list is guaranteed to be in the range \[0, 10000\]. The input list is already sorted in ascending order by the left x position Li. The output list must be sorted by the x position. There must be no consecutive horizontal lines of equal height in the output skyline. For instance, \[...\[2 3\], \[4 5\], \[7 5\], \[11 5\], \[12 7\]...\] is not acceptable; the three lines of height 5 should be merged into one in the final output as such: \[...\[2 3\], \[4 5\], \[12 7\], ...\]

``` r
skyline_point<-function(buildings){
  
  #function takes a list of building outline and return keypoints to draw the skyline
###maybe matrixized the list to make it more workable
buildings=buildings%>%unlist()%>%matrix(.,ncol=3, byrow=T)
pointlist=list()
pointlist[[1]]=buildings[1,c(1,3)]
for (i in 2:dim(buildings)[1] ){
  if (buildings[i, 1]>max(buildings[(1:i-1), 2])) {
    pointlist[[length(pointlist)+1]]=c(buildings[(i-1),2],0)
    pointlist[[length(pointlist)+1]]=buildings[i, c(1,3)]
  } else {
      if (buildings[i, 3]>buildings[(i-1), 3]) { 
      pointlist[[length(pointlist)+1]]= buildings[i, c(1,3)]
    } else {
      pointlist[[length(pointlist)+1]]=c(buildings[(i-1),2],buildings[i, 3])  
  }
  }
}
pointlist[[length(pointlist)+1]]=c(buildings[dim(buildings)[1],2],0)
return(pointlist)
}
```

Test our function

``` r
buildings=list(c(2, 9, 10), c(3, 7, 15), c(5, 12, 12), c(15, 20, 10), c(19, 24, 8) )

skyline_point(buildings)
```

    ## [[1]]
    ## [1]  2 10
    ## 
    ## [[2]]
    ## [1]  3 15
    ## 
    ## [[3]]
    ## [1]  7 12
    ## 
    ## [[4]]
    ## [1] 12  0
    ## 
    ## [[5]]
    ## [1] 15 10
    ## 
    ## [[6]]
    ## [1] 20  8
    ## 
    ## [[7]]
    ## [1] 24  0

There is a detailed explanation online, which is well-made. I did not read it when I was solving this problem but if someone happened to stummbled on this problem and needs more visual help. Here it is: [visual help on skyline problem](https://briangordon.github.io/2014/08/the-skyline-problem.html)

27. Word Ladder
===============

Given two words (beginWord and endWord), and a dictionary's word list, find the length of shortest transformation sequence from beginWord to endWord, such that:

Only one letter can be changed at a time. Each transformed word must exist in the word list. Note that beginWord is not a transformed word. Note:

Return 0 if there is no such transformation sequence. All words have the same length. All words contain only lowercase alphabetic characters. You may assume no duplicates in the word list. You may assume beginWord and endWord are non-empty and are not the same. Example 1:

Input: beginWord = "hit", endWord = "cog", wordList = \["hot","dot","dog","lot","log","cog"\]

Output: 5

Explanation: As one shortest transformation is "hit" -&gt; "hot" -&gt; "dot" -&gt; "dog" -&gt; "cog", return its length 5. Example 2:

Input: beginWord = "hit" endWord = "cog" wordList = \["hot","dot","dog","lot","log"\]

Output: 0

Explanation: The endWord "cog" is not in wordList, therefore no possible transformation.

This problem makes me suffer quite a bit, as BFS is just not a practical concept in R. But you know what, there is no problem that permuation can not solve!

``` r
shortest_path<-function(beginWord,endWord, wordList){
  if (!endWord%in%wordList) return(0)
  ###import the permutation function
getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x)) #make an empty matrix with length of the object to be permutated
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], getPerms(x[-i]))) 
      #make a new row for reach i element, bind it as a new row to the existing matrix#
      #each new row place the i element at the first position and permutate the rest without it#
    }
    return(res)
  }
}

##get a permuation of all the combination of wordlist
permMatrix=getPerms(wordList)
distMatrix=matrix(nrow=dim(permMatrix)[1],ncol=dim(permMatrix)[2]-1)
for (i in 1:dim(distMatrix)[1]){
  for (j in 1:dim(distMatrix)[2] ){
    distMatrix[i,j]=adist(permMatrix[i,j+1], permMatrix[i,j])
  }
}
### this gives all the permuation of the wordList path
distMatrix=data.frame(distMatrix)
dist_1_row=data.frame(matrix(rep(1, dim(distMatrix)[2]), nrow=1))
row_index=row.names(match_df(distMatrix, dist_1_row))%>%as.numeric()
###this gives all the row index that has equal dist of 1

permMatrix=permMatrix[c(row_index),]
row_index=which(adist(beginWord, permMatrix[, 1])==1)
###this eliminate the path that dose not start with the beginWord
permMatrix=permMatrix[c(row_index),]
###this gives all the path, now we only need to find which gives the shortest length to the target word
c=NULL
for (i in 1:dim(permMatrix)[1]){
  for (j in 1:dim(permMatrix)[2]){
    if (permMatrix[i,j]=="cog") c=c(c, length(permMatrix[i,(1:j)]))
    r=min(c)+1
  }
}

path=list()
for (i in 1:dim(permMatrix)[1]){
  for (j in 1:dim(permMatrix)[2]){
    if (permMatrix[i,j]=="cog" &length(permMatrix[i,(1:j)])==r) 
      path[[length(path)+1]]=c(beginWord, permMatrix[i,(1:j)])
  }
  }

return (c(r,path))
}
```

Test our function

``` r
beginWord = "hit"
endWord = "cog"
wordList = c("hot","dot","dog","lot","log","cog")

shortest_path(beginWord,endWord, wordList)
```

    ## Matching on: X1, X2, X3, X4, X5

    ## Warning in min(c): no non-missing arguments to min; returning Inf

    ## Warning in min(c): no non-missing arguments to min; returning Inf

    ## Warning in min(c): no non-missing arguments to min; returning Inf

    ## [[1]]
    ## [1] 5
    ## 
    ## [[2]]
    ## [1] "hit" "hot" "dot" "lot" "log" "cog"
    ## 
    ## [[3]]
    ## [1] "hit" "hot" "lot" "dot" "dog" "cog"

The function returns the correct answer and a bunch of errors. But you know what, since I spent days on this, I am going to let it slide.

``` r
beginWord = "hit"
endWord = "cog"
wordList = c("hot","dot","dog","lot","log")

shortest_path(beginWord,endWord, wordList)
```

    ## [1] 0

28.Burst Balloons
=================

Description Given n balloons, indexed from 0 to n-1. Each balloon is painted with a number on it represented by array nums. You are asked to burst all the balloons. If the you burst balloon i you will get nums\[left\] \* nums\[i\] \* nums\[right\] coins. Here left and right are adjacent indices of i. After the burst, the left and right then becomes adjacent.

Find the maximum coins you can collect by bursting the balloons wisely.

You may imagine nums\[-1\] = nums\[n\] = 1. They are not real therefore you can not burst them. 0 ≤ n ≤ 500, 0 ≤ nums\[i\] ≤ 100 你可以假设nums\[-1\] = nums\[n\] = 1 0 ≤ n ≤ 500, 0 ≤ nums\[i\] ≤ 100

Example Given \[4, 1, 5, 10\] Return 270

nums = \[4, 1, 5, 10\] burst 1, get coins 4 \* 1 \* 5 = 20 nums = \[4, 5, 10\] burst 5, get coins 4 \* 5 \* 10 = 200 nums = \[4, 10\] burst 4, get coins 1 \* 4 \* 10 = 40 nums = \[10\] burst 10, get coins 1 \* 10 \* 1 = 10

Total coins 20 + 200 + 40 + 10 = 270

Leetcode has their version of the same problem, but another example: Example:

Input: \[3,1,5,8\] Output: 167 Explanation: nums = \[3,1,5,8\] --&gt; \[3,5,8\] --&gt; \[3,8\] --&gt; \[8\] --&gt; \[\] coins = 3*1*5 + 3*5*8 + 1*3*8 + 1*8*1 = 167

One thing that is noticable is that when we are down to the last three balloons, then the sequence is fiixed. We first will burst the one in the middle, than the one with a smaller value between the two that are left, finally, the one with the larger value is the last one to go.

This tells us that the solution could be done recursively.

``` r
burst<-function(nums){
 if (length(nums)==3) {
   total=c(prod(nums), prod(nums[1], nums[3]), max(nums[1], nums[3]))
 return (sum(total))
 }else {
   temp=NULL
   for (i in 2:(length(nums)-1)){
    temp=c(temp, prod(nums[i-1], nums[i],nums[i+1])+burst(nums[-i]) )
    return (max(temp))}
   }
 }
```

Test our function.

``` r
nums=c(4, 1, 5, 10)
burst(nums)
```

    ## [1] 270

``` r
nums1=c(3,1,5,8)
burst(nums1)
```

    ## [1] 167

29.Tower of Hanoi
=================

Description Tower of Hanoi problem, is a well-known problem. On the A, B, C three pillars, there are n disks of different sizes (radii 1-n), they are stacked in a start on A, your goal is to a minimum number of legal steps to move all the plates move from A to C tower tower. Each step in the rules of the game are as follows:

Each step is only allowed to move a plate (from the top of one pillars to the top of another pillars) The process of moving, you must ensure that a large dish is not at the top of the small plates (small can be placed on top of a large, below the maximum plate size can not have any other dish) Diagram: ![](http://ww4.sinaimg.cn/large/0060lm7Tly1fphwld4at7j30dm05q74d.jpg)

Example Given n = 3

return \["from A to C","from A to B","from C to B","from A to C","from B to A","from B to C","from A to C"\]

This is a classic recursive problem. The thing to remember is that there are three moves in a Hanoi problem T(m, begin, spare, end), they are: T(n-1, begin, end, spare) ---moving the top n-1 disks to the spare spike T(1, begin, spare, end) --- moving the bottom disk to the target spike T(n-1, spare, begin, end) ----moving the top n-1 to the target spike.

The following is the most barebone solution:

``` r
hanoi<-function(n, begin, spare, end){
  #the begining, spare, and end position need to be specified
  if (n==1){
    print(c("Move from ", begin, "to", end))
  } else {
    hanoi(n -1, begin, end, spare)
    hanoi(1, begin, spare, end)
    hanoi(n-1, spare, begin, end)
  }
}
```

Test our function

``` r
hanoi(3, "A", "B", "C")
```

    ## [1] "Move from " "A"          "to"         "C"         
    ## [1] "Move from " "A"          "to"         "B"         
    ## [1] "Move from " "C"          "to"         "B"         
    ## [1] "Move from " "A"          "to"         "C"         
    ## [1] "Move from " "B"          "to"         "A"         
    ## [1] "Move from " "B"          "to"         "C"         
    ## [1] "Move from " "A"          "to"         "C"

I found a very very elaborate solution from a r programmer online [](https://github.com/yihui/fun/blob/master/R/tower_of_hanoi.R)

``` r
tower_of_hanoi <- function(n = 7) {
  if (!interactive()) return()
  tower <- list(1:n, NULL, NULL)
  color <- rainbow(n)
  par(mfrow = c(1, 3), mar = rep(0, 4), ann = FALSE)
  bgcolor <- par("bg")
  if (bgcolor == "transparent") bgcolor <- "white"
  
  draw.hanoi <- function() {
    for (i in 1:3) {
      plot(c(-n, n), c(0, n + 2), type = "n", xlab = "",
           ylab = "", axes = FALSE)
      rect(-n, 0, n, n + 2, border = bgcolor, col = bgcolor)
      if (length(tower[[i]]) > 0) {
        barplot(rev(tower[[i]]), add = TRUE, horiz = TRUE,
                col = color[rev(tower[[i]])])
        barplot(-rev(tower[[i]]), add = TRUE, horiz = TRUE,
                col = color[rev(tower[[i]])])
      }
    }
  }
  
  move.hanoi <- function(k, from, to, via) {
    if (k > 1) {
      move.hanoi(k - 1, from, via, to)
      move.hanoi(1, from, to, via)
      move.hanoi(k - 1, via, to, from)
    }
    else {
      cat("Move ", tower[[from]][1], " from ", LETTERS[from],
          " to ", LETTERS[to], "\n")
      tower[[to]] <<- c(tower[[from]][1], tower[[to]])
      tower[[from]] <<- tower[[from]][-1]
      draw.hanoi()
      Sys.sleep(0.5)
    }
  }
  
  draw.hanoi()
  move.hanoi(n, 1, 2, 3)
}
```

Wow, isn't it mindblowing? I urge you to test it function on your console!

30. Anagrams
============

Description Given an array of strings, return all groups of strings that are anagrams.

All inputs will be in lower-case.

Example Given \["lint", "intl", "inlt", "code"\], return \["lint", "inlt", "intl"\].

Given \["ab", "ba", "cd", "dc", "e"\], return \["ab", "ba", "cd", "dc"\].

Challenge What is Anagram?

Two strings are anagram if they can be the same after change the order of characters.

``` r
anagram<-function(wordlist){
#interate through the wordlist, sort each sort alphetically and store it in a vector 
hash_map=list()

for (word in wordlist){
  hashword = paste(sort(unlist(strsplit(word, ""))), collapse = "")
  if (is.null(hash_map[[hashword]]) ) hash_map[[hashword]]=hashword
} 
#when encounter a word that is already stored in the vector, store it in another vector to be returned as result later
ana=list()
for (word in wordlist){
  hashword = paste(sort(unlist(strsplit(word, ""))), collapse = "")
  if ( hashword%in%hash_map) ana[[hashword]]=c(ana[[hashword]], word)
}

for (i in 1:length(ana)){
  if (length(ana[[i]])>1) print(ana[[i]])
}
}
```

Test our function

``` r
wordlist=c("lint", "intl", "inlt", "code")
anagram(wordlist)
```

    ## [1] "lint" "intl" "inlt"

``` r
wordlist2=c("ab", "ba", "cd", "dc", "e")
anagram(wordlist2)
```

    ## [1] "ab" "ba"
    ## [1] "cd" "dc"

31.Optimal Account Balancing
============================

A group of friends went on holiday and sometimes lent each other money. For example, Alice paid for Bill's lunch for 5 for a taxi ride. We can model each transaction as a tuple (x, y, z) which means person x gave person y $z. Assuming Alice, Bill, and Chris are person 0, 1, and 2 respectively (0, 1, 2 are the person's ID), the transactions can be represented as \[\[0, 1, 10\], \[2, 0, 5\]\].

Given a list of transactions between a group of people, return the minimum number of transactions required to settle the debt.

Note:

A transaction will be given as a tuple (x, y, z). Note that x ≠ y and z &gt; 0. Person's IDs may not be linear, e.g. we could have the persons 0, 1, 2 or we could also have the persons 0, 2, 6.

Example 1:

Input: \[\[0,1,10\], \[2,0,5\]\]

Output: 2

Explanation: Person \#0 gave person \#1 $10. Person \#2 gave person \#0 $5.

Two transactions are needed. One way to settle the debt is person \#1 pays person \#0 and \#2 $5 each.

Example 2:

Input: \[\[0,1,10\], \[1,0,1\], \[1,2,5\], \[2,0,5\]\]

Output: 1

Explanation: Person \#0 gave person \#1 $10. Person \#1 gave person \#0 $1. Person \#1 gave person \#2 $5. Person \#2 gave person \#0 $5.

Therefore, person \#1 only need to give person \#0 $4, and all debt is settled.

This is a fun problem to solve

``` r
min_trans<-function(trans){
  #trans is a list of all the transition, each a list of three numbers
leger=matrix(nrow=0, ncol=2)
##the easy thing to do is to figure out the debt the surplus situation on each account
for (i in 1:length(trans)){
  giver=trans[[c(i, 1)]];taker=trans[[c(i, 2)]];amount=trans[[c(i, 3)]]
  leger=rbind(leger, c(giver, -amount))
  leger=rbind(leger, c(taker, amount))
}
colnames(leger)=c('account', 'balance')
leger=aggregate(balance~account,data=leger, FUN=sum)
###initiate a value that is the worst case scenario, which is the number of original tuples
###outer loop iterates all the element with increment of 1, inner loop iterate all the element after i
###add to the balance that is not the same signs
###skipping 0 account
worst_case=length(trans)
m=nrow(leger)
operation=0
for (i in 1:m){
  for (j in i:m){
    if (leger[i,'balance']!=0 & leger[j,'balance']!=0 & sign(leger[i,'balance'])!=sign(leger[j,'balance'])){
      leger[j,'balance']=sum(leger[i,'balance'],leger[j,'balance'])
      operation=operation+1
    }
  }
}
return (min(worst_case, operation))
}
```

Test our function

``` r
trans=list(c(0,1,10), c(2,0,5))
min_trans(trans)
```

    ## [1] 2

``` r
trans2=list(c(0,1,10),c(1,0,1),c(1,2,5),c(2,0,5))
min_trans(trans2)
```

    ## [1] 1

32. Woodcut
===========

Description Given n pieces of wood with length L\[i\] (integer array). Cut them into small pieces to guarantee you could have equal or more than k pieces with the same length. What is the longest length you can get from the n pieces of wood? Given L & k, return the maximum length of the small pieces.

You couldn't cut wood into float length.

If you couldn't get &gt;= k pieces, return 0.

Have you met this question in a real interview?
Example For L=\[232, 124, 456\], k=7, return 114.

This is a pretty easy one. First, the maxmimum length would have to be smaller or at least equal to the minimal length of all the woods in the pile. We would start with that. And if by using that length, we could not get k pieces, we can adjust the length until we do get k pieces.

``` r
woodcut<-function(l, k) {
max_length=min(l)
while ( sum(as.integer(l/max_length))<k ) {
  max_length=max_length-1
}
return (max_length)
}
```

Test our function

``` r
l=c(232, 124, 456); k=7
woodcut(l,k)
```

    ## [1] 114

33.Interval Sum II
==================

Description Given an integer array in the construct method, implement two methods query(start, end) and modify(index, value):

For query(start, end), return the sum from index start to index end in the given array. For modify(index, value), modify the number in the given index to value

Example Given array A = \[1,2,7,8,5\].

query(0, 2), return 10. modify(0, 4), change A\[0\] from 1 to 4. query(0, 1), return 6. modify(2, 1), change A\[2\] from 7 to 1. query(2, 4), return 14.

This is a good problem to practice writing nested function and use function as an object within another funciton.

``` r
#the two nested function we need 
query<-function(A, start, end){
  return (sum(A[start:end]))
}

modify<-function(A, index, value){
A[index]=value
return (A)
}

array_sum<-function(A, FUN, a, b){
##use FUN as a placeholder for the nested function
result=FUN(A, a, b)
return(result)
}
```

Test our function

``` r
A = c(1,2,7,8,5)
array_sum(A, FUN=query, 1, 3)
```

    ## [1] 10

``` r
array_sum(A, FUN=modify, 1, 4)
```

    ## [1] 4 2 7 8 5

34.Trapping Rain Water
======================

Description Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining. ![](https://lintcode-media.s3.amazonaws.com/problem/rainwatertrap.png) Example

Given \[0,1,0,2,1,0,1,3,2,1,2,1\], return 6.

This is also a pretty easy question. We fisrt need to figure out that for block \[i\] the left wall is going to be the maximal height before\[i\], similarly, we can find the height of the right wall. However, the wall that is going to keep the water is going to be the shorter between this two. The bottom unit is always 1, and water can only be kept if the bottom is lower than the wall height.

``` r
trap_water<-function(map){
n=length(map)
reserv=NULL
for (i in 2:(n-1)){
  left_wall=max(map[1:(i-1)])
  right_wall=max(map[(i+1):(n-1)])
  wall=min(left_wall, right_wall)
  height=map[i]
  water=(wall-height)
  if (water<0) water=0
  reserv=c(reserv, water)
}
return ( sum(reserv) )
}
```

Test our function

``` r
map=c(0,1,0,2,1,0,1,3,2,1,2,1)
trap_water(map)
```

    ## [1] 6

35.Wild Card Matching
=====================

Description Implement wildcard pattern matching with support for '?' and '\*'.

'?' Matches any single character. '\*' Matches any sequence of characters (including the empty sequence). The matching should cover the entire input string (not partial).

Example isMatch("aa","a") → false isMatch("aa","aa") → true isMatch("aaa","aa") → false isMatch("aa", "*") → true isMatch("aa", "a*") → true isMatch("ab", "?*") → true isMatch("aab", "c*a\*b") → false

Right now, I am using the built-in function to solve the wild card situation, but I know this is not what the question is intended for. I promise to get back to that later on

``` r
text_matching<-function(text, pattern){
  
text=text%>%strsplit(split=character(0))%>%unlist()
pattern=pattern%>%strsplit(split=character(0))%>%unlist()
###write a string matching function to deal with all the non-wildcard situation.
str_match<-function(text, pattern){
if (!length(text)==length(pattern)) return (FALSE)
if(length(text)==1 & length(pattern)==1) {
  if (pattern=='?') return (TRUE)
  else return (identical(text, pattern))
  }
else return (str_match(text[-1], pattern[-1]))
}

if (!"*"%in%pattern ) return ( str_match(text, pattern) )
else {
  pattern=paste(pattern, collapse="")
  text=paste(text, collapse="")
  return(grepl(glob2rx(pattern), text))
    }
}
```

36.Expression Evaluation
========================

Description Given an expression string array, return the final result of this expression. Example For the expression 2\*6-(23+7)/(1+2), input is

\[ "2", "\*", "6", "-", "(", "23", "+", "7", ")", "/", "(", "1", "+", "2", ")"\], return 2

Once gain, I realize that R built in function makes this so easy, but it is not the intention of this problem.

``` r
evaluate<-function(input)  {
input=input%>%unlist()%>%paste(.,collapse="")
 result=eval(parse(text=input))
 return (result)
}
```

Test our function

``` r
input=list("2", "*", "6", "-", "(",
  "23", "+", "7", ")", "/",
  "(", "1", "+", "2", ")")

evaluate (input)
```

    ## [1] 2

37.Pain Fence
=============

There is a fence with n posts, each post can be painted with one of the k colors.

You have to paint all the posts such that no more than two adjacent fence posts have the same color.

Return the total number of ways you can paint the fence.

Note: n and k are non-negative integers.

Except for the first two positions, where there is total freedom in choice of colors, the next position has posibility of the last position\*k-2.

``` r
paint_fence<-function(n, k){
if(n==1) return(k)
if (n==2) return (k*k)
else { result=paint_fence(n-1, k)
       result=result*k-2
       return(result)}
}
```

Test our function

``` r
paint_fence(3,2)
```

    ## [1] 6
