Unique Path
================
Liwen Huang
8/27/2018

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
