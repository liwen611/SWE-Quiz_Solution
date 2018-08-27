Burst Balloons
================
Liwen Huang
8/27/2018

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
