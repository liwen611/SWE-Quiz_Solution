KSum Problem
================
Liwen Huang
8/25/2018

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

For the small numbers we put in, the recursive function works just fine, but things will feel very slow if k gets bigger.
