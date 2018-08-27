JumpGame
================
Liwen Huang
8/27/2018

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
