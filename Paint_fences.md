Paint Fence
================
Liwen Huang
8/27/2018

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
