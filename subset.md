Subset
================
Liwen Huang
8/25/2018

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
