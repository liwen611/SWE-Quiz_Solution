Interleaving Postive and Negative Numbers
================
Liwen Huang
8/27/2018

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
