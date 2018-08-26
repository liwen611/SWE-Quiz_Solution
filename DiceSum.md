DiceSum
================
Liwen Huang
8/25/2018

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
