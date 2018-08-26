TwoSum
================
Liwen Huang
8/25/2018


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
