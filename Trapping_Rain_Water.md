Trapping Rain Water
================
Liwen Huang
8/27/2018

Description Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining. ![](https://lintcode-media.s3.amazonaws.com/problem/rainwatertrap.png)

Example

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
